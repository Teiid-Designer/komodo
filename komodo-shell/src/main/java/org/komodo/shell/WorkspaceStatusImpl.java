/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.shell;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.ServiceLoader;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.TimeUnit;
import org.komodo.core.KEngine;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.SynchronousCallback;
import org.komodo.shell.api.KomodoObjectLabelProvider;
import org.komodo.shell.api.KomodoShell;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandFactory;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.api.WorkspaceStatusEventHandler;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;
import org.modeshape.common.collection.Collections;

/**
 * Test implementation of WorkspaceStatus
 */
public class WorkspaceStatusImpl implements WorkspaceStatus {

    private static final KLog LOGGER = KLog.getLogger();
    /**
     * A transaction commit/rollback source for when the transaction was called directly not going through the WorkspaceStatus.
     */
    static final String UNKNOWN_SOURCE = WorkspaceStatusTransaction.class.getSimpleName();

    private static final String SAVED_CONTEXT_PATH = "SAVED_CONTEXT_PATH"; //$NON-NLS-1$
    private static final List< String > HIDDEN_PROPS = Arrays.asList( new String[] { SAVED_CONTEXT_PATH } );

    private final KomodoShell shell;

    /* Root Context */
    private KomodoObject rootContext;

    private WorkspaceStatusTransaction uow; // the current transaction
    private SynchronousCallback callback;

    private int count = 0; // commit count

    private KomodoObject currentContext;
    private Set<WorkspaceStatusEventHandler> eventHandlers = new HashSet<WorkspaceStatusEventHandler>();

    private Properties wsProperties = new Properties();

    private boolean recordingStatus = false;
    private Writer recordingFileWriter = null;
    private ShellCommandFactory commandFactory;
    private Set<ShellCommand> currentContextCommands = new HashSet<ShellCommand>();

    private Map<String,KomodoObject> stateObjects = new HashMap<String,KomodoObject>();

    private KomodoObjectLabelProvider currentContextLabelProvider;
    private KomodoObjectLabelProvider defaultLabelProvider;
    private KomodoObjectLabelProvider lastUsedLabelProvider;
    private Collection<KomodoObjectLabelProvider> alternateLabelProviders = new ArrayList<>();

    /**
     * Constructor
     * @param shell parent shell
     * @throws Exception error on initialisation failure
     */
    public WorkspaceStatusImpl(KomodoShell shell) throws Exception {
        this( null, shell );
    }

    /**
     * @param transaction
     *        the transaction to use initially in the shell (can be <code>null</code> if one should be created)
     * @param shell
     *        parent shell
     * @throws Exception
     *         error on initialisation failure
     */
    public WorkspaceStatusImpl( final UnitOfWork transaction,
                                final KomodoShell shell) throws Exception {
        this.shell = shell;
        init(transaction);
    }

    private void init( final UnitOfWork transaction ) throws Exception {
        this.commandFactory = new ShellCommandFactoryImpl(this);

        if ( transaction == null ) {
            createTransaction("init"); //$NON-NLS-1$
        } else {
            this.uow = ( ( transaction instanceof WorkspaceStatusTransaction ) ? ( WorkspaceStatusTransaction )transaction
                                                                               : new WorkspaceStatusTransaction( transaction ) );
            Repository.UnitOfWorkListener uowListener = transaction.getCallback();
            if(uowListener!=null && uowListener instanceof SynchronousCallback) {
                this.callback = (SynchronousCallback)uowListener;
            }
        }

        // The default root context is komodoRoot.  The providers may specify a higher root (as in WorkspaceManager)
        final Repository repo = getEngine().getDefaultRepository();
        this.rootContext = new ObjectImpl( repo, RepositoryImpl.KOMODO_ROOT, 0 );
        this.currentContext = this.rootContext;

        // initialize the global properties
        initGlobalProperties();

        // initialize the validation rules
        initValidationRules(true);

        this.defaultLabelProvider = new DefaultLabelProvider();
        this.defaultLabelProvider.setRepository( repo );
        this.defaultLabelProvider.setWorkspaceStatus( this );

        // Discover any other label providers
        discoverLabelProviders();
        setLabelProvider(this.currentContext);
    }

    private void initGlobalProperties() throws KException {
        resetGlobalProperties();

        // load shell properties if they exist
        final String dataDir = this.shell.getShellDataLocation();
        final File startupPropertiesFile = new File( dataDir, this.shell.getShellPropertiesFile() );

        if ( startupPropertiesFile.exists() && startupPropertiesFile.isFile() && startupPropertiesFile.canRead() ) {
            try ( final FileInputStream fis = new FileInputStream( startupPropertiesFile ) ) {
                this.wsProperties.load( fis );
            } catch ( final Exception e ) {
                String msg = I18n.bind( ShellI18n.errorLoadingProperties,
                                        startupPropertiesFile.getAbsolutePath(),
                                        e.getMessage() );
                PrintUtils.print(getOutputWriter(), CompletionConstants.MESSAGE_INDENT, msg);
            }
        }

        // Init the recording output file if it is defined
        String recordingFile = this.wsProperties.getProperty(RECORDING_FILE_KEY);
        if(!StringUtils.isBlank(recordingFile)) {
            setRecordingWriter(recordingFile);
        }

        // Let the providers init any provided states
        initProvidedStates(this.wsProperties);
    }

    private void initValidationRules(boolean overrideExisting) throws KException {
        // load shell properties if they exist
        final String dataDir = this.shell.getShellDataLocation();
        final File validationRulesFile = new File( dataDir, this.shell.getShellValidationRulesFile() );

        final Repository repo = getEngine().getDefaultRepository();
        repo.getValidationManager().importRules(validationRulesFile, getTransaction(), overrideExisting);
    }

    private void createTransaction(final String source ) throws Exception {
        final Repository repo = getEngine().getDefaultRepository();
        this.callback = new SynchronousCallback();
        final UnitOfWork transaction = repo.createTransaction( ( getClass().getSimpleName() + ':' + source + '-' + this.count++ ),
                                                               false,
                                                               this.callback );
        this.uow = new WorkspaceStatusTransaction( transaction );
        KLog.getLogger().debug( "WorkspaceStatusImpl.createTransaction: " + this.uow.getName() ); //$NON-NLS-1$
    }

    @Override
    public KomodoShell getShell() {
        return shell;
    }

    @Override
    public KEngine getEngine() {
        return shell.getEngine();
    }

    @Override
    public InputStream getInputStream() {
        return shell.getInputStream();
    }

    @Override
    public Writer getOutputWriter() {
        return shell.getOutputWriter();
    }

    @Override
    public ShellCommandFactory getCommandFactory() {
        return commandFactory;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#getTransaction()
     */
    @Override
    public UnitOfWork getTransaction() {
        return this.uow;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#setTransaction(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void setTransaction( final UnitOfWork transaction ) {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        this.uow = ( ( transaction instanceof WorkspaceStatusTransaction ) ? ( WorkspaceStatusTransaction )transaction
                                                                           : new WorkspaceStatusTransaction( transaction ) );
    }

    @Override
    public void commit( final String source ) throws Exception {
        String newTxName = source;
        final String txName = this.uow.getName();
        this.uow.getDelegate().commit();

        try {
            final boolean success = this.callback.await( 3, TimeUnit.MINUTES );

            if ( success ) {
                final KException error = this.uow.getError();

                if ( error != null ) {
                    newTxName += "__commitSuccessWithError"; //$NON-NLS-1$
                    throw new KException( I18n.bind( ShellI18n.transactionCommitError, txName ), error );
                }

                final Throwable callbackError = this.callback.error();

                if ( callbackError != null ) {
                    newTxName += "__commitSuccessWithCallbackError"; //$NON-NLS-1$
                    throw new KException( I18n.bind( ShellI18n.transactionCommitError, txName ), callbackError );
                }

                final State txState = this.uow.getState();

                if ( !State.COMMITTED.equals( txState ) ) {
                    newTxName += ( "__commitSuccessWrongState:" + txState ); //$NON-NLS-1$
                    throw new KException( I18n.bind( ShellI18n.transactionCommitError, txName ) );
                }
            } else {
                newTxName += "__commitFail"; //$NON-NLS-1$
                throw new KException( I18n.bind( ShellI18n.transactionTimeout, txName ) );
            }
        } catch ( final Exception e ) {
            if ( newTxName.equals( source ) ) {
                newTxName += "__commitException"; //$NON-NLS-1$
            }

            if ( UNKNOWN_SOURCE.equals( source ) ) {
                this.uow.getCallback().errorOccurred( e );
                KLog.getLogger().debug( "{0}.commit error: ", e, UNKNOWN_SOURCE ); //$NON-NLS-1$
            } else {
                throw e;
            }
        } finally {
            createTransaction( newTxName );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#rollback(java.lang.String)
     */
    @Override
    public void rollback( final String source ) throws Exception {
        String newTxName = source;
        final String txName = this.uow.getName();
        this.uow.getDelegate().rollback();

        try {
            final boolean success = this.callback.await( 3, TimeUnit.MINUTES );

            if ( success ) {
                final KException error = uow.getError();

                if ( error != null ) {
                    newTxName += "__rollbackSuccessWithError"; //$NON-NLS-1$
                    throw new KException( I18n.bind( ShellI18n.transactionRollbackError, txName ), error );
                }

                final Throwable callbackError = this.callback.error();

                if ( callbackError != null ) {
                    newTxName += "__rollbackSuccessWithCallbackError"; //$NON-NLS-1$
                    throw new KException( I18n.bind( ShellI18n.transactionRollbackError, txName ), callbackError );
                }

                final State txState = this.uow.getState();

                if ( !State.ROLLED_BACK.equals( txState ) ) {
                    newTxName += ( "__rollbackSuccessWrongState:" + txState ); //$NON-NLS-1$
                    throw new KException( I18n.bind( ShellI18n.transactionRollbackError, txName ) );
                }
            } else {
                newTxName += "__rollbackFail"; //$NON-NLS-1$
                throw new KException( I18n.bind( ShellI18n.transactionTimeout, txName ) );
            }
        } catch ( final Exception e ) {
            if ( newTxName.equals( source ) ) {
                newTxName += "__rollbackException"; //$NON-NLS-1$
            }

            if ( UNKNOWN_SOURCE.equals( source ) ) {
                this.uow.getCallback().errorOccurred( e );
                KLog.getLogger().debug( "{0}.rollback error: ", e, UNKNOWN_SOURCE ); //$NON-NLS-1$
            } else {
                throw e;
            }
        } finally {
            createTransaction( newTxName );
        }
    }

    @Override
    public void setCurrentContext(KomodoObject context) throws Exception {
        this.currentContext = context;
        this.wsProperties.setProperty( SAVED_CONTEXT_PATH, this.currentContext.getAbsolutePath() );
        setLabelProvider(this.currentContext);  // Resets the current LabelProvider for the context

        { // try and resolve
            final KomodoObject resolved = resolve( this.currentContext );

            if ( resolved != null ) {
                this.currentContext = resolved;
            }
        }

        // Update Available Commands on context change
        updateAvailableCommands( );

        fireContextChangeEvent();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#getAvailableCommands()
     */
    @Override
    public Set<ShellCommand> getAvailableCommands() {
        if(this.currentContextCommands.isEmpty()) {
            updateAvailableCommands();
        }
        return Collections.unmodifiableSet( this.currentContextCommands );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#getAvailableCommandNames()
     */
    @Override
    public String[] getAvailableCommandNames() throws Exception {
        final Set< String > commandNames = new TreeSet< >();

        for ( final ShellCommand possible : getAvailableCommands() ) {
            commandNames.add( possible.getName() );
        }

        return commandNames.toArray( new String[0] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#updateAvailableCommands()
     */
    @Override
    public void updateAvailableCommands() {
        this.currentContextCommands.clear();

        this.currentContextCommands.addAll(this.commandFactory.getCommandsForCurrentContext());
    }

    @Override
    public KomodoObject getCurrentContext() {
        return this.currentContext;
    }

    @Override
    public String getCurrentContextDisplayPath() {
        return getCurrentContextLabelProvider().getDisplayPath(this.currentContext);
    }

    @Override
    public String getDisplayPath(KomodoObject context) {
        return getCurrentContextLabelProvider().getDisplayPath(context);
    }

    @Override
    public KomodoObject getRootContext() {
        return this.rootContext;
    }

    /**
     * @see org.komodo.shell.api.WorkspaceStatus#addHandler(org.komodo.shell.api.WorkspaceStatusEventHandler)
     */
    @Override
    public void addHandler(WorkspaceStatusEventHandler handler) {
        this.eventHandlers.add(handler);
    }

    /**
     * @see org.komodo.shell.api.WorkspaceStatus#removeHandler(org.komodo.shell.api.WorkspaceStatusEventHandler)
     */
    @Override
    public void removeHandler(WorkspaceStatusEventHandler handler) {
        this.eventHandlers.remove(handler);
    }

    /**
     * Fires the context change event.
     * @throws Exception if error occurs
     */
    private void fireContextChangeEvent() throws Exception {
        for (WorkspaceStatusEventHandler handler : eventHandlers) {
            handler.workspaceContextChanged();
        }
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#setRecordingStatus(boolean)
     */
    @Override
    public void setRecordingStatus(boolean recordState) {
        this.recordingStatus = recordState;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#getRecordingStatus()
     */
    @Override
    public boolean getRecordingStatus() {
        return this.recordingStatus;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#getRecordingWriter()
     */
    @Override
    public Writer getRecordingWriter() {
        return this.recordingFileWriter;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#isBooleanProperty(java.lang.String)
     */
    @Override
    public boolean isBooleanProperty( final String name ) {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        final String propertyName = name.toUpperCase();
        return propertyName.equals( SHOW_COMMAND_CATEGORY )
               || propertyName.equals( SHOW_FULL_PATH_IN_PROMPT_KEY )
               || propertyName.equals( SHOW_HIDDEN_PROPERTIES_KEY )
               || propertyName.equals( SHOW_PROP_NAME_PREFIX_KEY )
               || propertyName.equals( SHOW_TYPE_IN_PROMPT_KEY )
               || propertyName.equals( AUTO_COMMIT );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#isShowingCommandCategory()
     */
    @Override
    public boolean isShowingCommandCategory() {
        assert ( this.wsProperties.containsKey( SHOW_COMMAND_CATEGORY ) );
        return Boolean.parseBoolean( this.wsProperties.getProperty( SHOW_COMMAND_CATEGORY ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#isShowingFullPathInPrompt()
     */
    @Override
    public boolean isShowingFullPathInPrompt() {
        assert ( this.wsProperties.containsKey( SHOW_FULL_PATH_IN_PROMPT_KEY ) );
        return Boolean.parseBoolean( this.wsProperties.getProperty( WorkspaceStatus.SHOW_FULL_PATH_IN_PROMPT_KEY ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#isShowingHiddenProperties()
     */
    @Override
    public boolean isShowingHiddenProperties() {
        assert (this.wsProperties.containsKey( SHOW_HIDDEN_PROPERTIES_KEY ));
        return Boolean.parseBoolean( this.wsProperties.getProperty( WorkspaceStatus.SHOW_HIDDEN_PROPERTIES_KEY ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#isShowingPropertyNamePrefixes()
     */
    @Override
    public boolean isShowingPropertyNamePrefixes() {
        assert (this.wsProperties.containsKey( SHOW_PROP_NAME_PREFIX_KEY ));
        return Boolean.parseBoolean( this.wsProperties.getProperty( WorkspaceStatus.SHOW_PROP_NAME_PREFIX_KEY ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#isShowingTypeInPrompt()
     */
    @Override
    public boolean isShowingTypeInPrompt() {
        assert (this.wsProperties.containsKey( SHOW_TYPE_IN_PROMPT_KEY ));
        return Boolean.parseBoolean( this.wsProperties.getProperty( WorkspaceStatus.SHOW_TYPE_IN_PROMPT_KEY ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#validateGlobalPropertyValue(java.lang.String, java.lang.String)
     */
    @Override
    public String validateGlobalPropertyValue( final String propertyName,
                                               final String proposedValue ) {
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$

        if ( !GLOBAL_PROPS.containsKey( propertyName.toUpperCase() ) ) {
            return I18n.bind( ShellI18n.invalidGlobalPropertyName, propertyName );
        }

        // empty value means they want to remove or reset to default value
        if ( StringUtils.isEmpty( proposedValue ) ) {
            return null; // name and value are valid
        }

        if ( isBooleanProperty( propertyName ) ) {
            if ( Boolean.parseBoolean( proposedValue ) || "false".equalsIgnoreCase( ( proposedValue ) ) ) { //$NON-NLS-1$
                return null;
            }

            return I18n.bind( ShellI18n.invalidBooleanGlobalPropertyValue, proposedValue, propertyName.toUpperCase() );
        }

        return null; // name and value are valid
    }

    private void resetGlobalProperties() {
        for ( final Entry< String, String > entry : GLOBAL_PROPS.entrySet() ) {
            setGlobalProperty( entry.getKey(), entry.getValue() );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#setGlobalProperty(java.lang.String, java.lang.String)
     */
    @Override
    public void setGlobalProperty( final String name,
                                   final String value ) {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        if ( !HIDDEN_PROPS.contains( name ) && WorkspaceStatus.GLOBAL_PROPS.containsKey( name.toUpperCase() ) ) {
            // if empty value reset to default value
            if ( StringUtils.isEmpty( value ) ) {
                this.wsProperties.setProperty( name, GLOBAL_PROPS.get( name ) );
            } else {
                // validate new value
                if ( StringUtils.isEmpty( validateGlobalPropertyValue( name, value ) ) ) {
                    this.wsProperties.setProperty( name.toUpperCase(), value );
                } else {
                    // reset to default value if value is invalid
                    this.wsProperties.setProperty( name, GLOBAL_PROPS.get( name ) );
                }
            }
            if(name.toUpperCase().equals(WorkspaceStatus.RECORDING_FILE_KEY)) {
                setRecordingWriter(value);
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#setGlobalProperties(java.util.Properties)
     */
    @Override
    public void setGlobalProperties( final Properties props ) throws Exception {
        resetGlobalProperties();

        if ( ( props != null ) && !props.isEmpty() ) {
            for ( final String name : props.stringPropertyNames() ) {
                setGlobalProperty( name, props.getProperty( name ) );
            }

            // set current context to saved context if necessary
            String savedPath = props.getProperty( SAVED_CONTEXT_PATH );

            if ( StringUtils.isBlank( savedPath ) ) {
                savedPath = KomodoObjectLabelProvider.WORKSPACE_PATH;
            }

            final Repository repo = getEngine().getDefaultRepository();
            KomodoObject context = new ObjectImpl( repo, savedPath, 0 );

            // make sure object still exists
            try {
                context.getName( getTransaction() );
            } catch ( final Exception e ) {
                context = null;
            }

            // saved path no longer exists so set context to workspace root
            if ( context == null ) {
                context = getRootContext();
            }

            setCurrentContext( context );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#setProvidedProperties(java.util.Properties)
     */
    @Override
    public void setProvidedProperties( final Properties props ) throws Exception {
        if ( ( props != null ) && !props.isEmpty() ) {
            for ( final String name : props.stringPropertyNames() ) {
                setProvidedProperty( name, props.getProperty( name ) );
            }
        }
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#setStateProperty(java.lang.String, java.lang.String)
     */
    @Override
    public void setProvidedProperty(String name,
                                    String value) {
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        if ( StringUtils.isEmpty( value ) ) {
            this.wsProperties.remove(name);
        } else {
            this.wsProperties.setProperty( name, value );
        }
    }

    // Attempt to init the writer using the supplied file path
    private void setRecordingWriter(String recordingFilePath) {
        // commandWriter for output of error messages
        Writer commandWriter = getOutputWriter();

        if(recordingFilePath==null) {
            recordingFileWriter = null;
            return;
        }

        // Checks to ensure the specified file is valid and writable
        File outputFile = new File(recordingFilePath);

        recordingFileWriter = null;
        try {
            // Creates file only if it doesnt exist
            outputFile.createNewFile();
            // Make sure we can write to the file
            if(!outputFile.canWrite()) {
                PrintUtils.print( commandWriter,
                                  CompletionConstants.MESSAGE_INDENT,
                                  I18n.bind( ShellI18n.recordingFileCannotWrite, recordingFilePath ) );
                return;
            }
            recordingFileWriter = new FileWriter(outputFile,true);
        } catch (IOException ex) {
            PrintUtils.print(commandWriter, 0, I18n.bind(ShellI18n.recordingFileOutputError,outputFile));
        }
    }

    @Override
    public void closeRecordingWriter() {
        if(this.recordingFileWriter!=null) {
            try {
                this.recordingFileWriter.close();
            } catch (IOException ex) {
                // nothing to do
            }
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#getGlobalProperties()
     */
    @Override
    public Properties getGlobalProperties() {
        final Properties copy = new Properties(); // just provide a copy

        for ( final String propName : this.wsProperties.stringPropertyNames() ) {
            // Includes the defined global properties and hidden properties
            if ( HIDDEN_PROPS.contains( propName ) || WorkspaceStatus.GLOBAL_PROPS.containsKey( propName.toUpperCase() ) ) {
                copy.setProperty( propName, this.wsProperties.getProperty( propName ) );
            }
        }

        return copy;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#getProvidedProperties()
     */
    @Override
    public Properties getProvidedProperties() {
        final Properties copy = new Properties(); // just provide a copy

        for ( final String propName : this.wsProperties.stringPropertyNames() ) {
            if ( !HIDDEN_PROPS.contains(propName) && !WorkspaceStatus.GLOBAL_PROPS.containsKey( propName.toUpperCase() ) ) {
                copy.setProperty( propName, this.wsProperties.getProperty( propName ) );
            }
        }

        return copy;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#getCommand(java.lang.String)
     */
    @Override
    public ShellCommand getCommand( final String commandName ) throws Exception {
        Set<ShellCommand> availableCommands = getAvailableCommands();
        // see if there is a match
        for ( final ShellCommand possible : availableCommands ) {
            if ( commandName.equals( possible.getName() ) ) {
                return possible;
            }
        }

        // see if there is a matching alias
        for ( final ShellCommand possible : availableCommands ) {
            if ( Arrays.asList( possible.getAliases() ).contains( commandName ) ) {
                return possible;
            }
        }

        // command can't be found
        return this.getCommandFactory().createCommandNotFound(commandName);
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#getStateObjects()
     */
    @Override
    public Map<String, KomodoObject> getStateObjects() {
        return this.stateObjects;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#setStateObject(java.lang.String, org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public void setStateObject(String key,
                               KomodoObject stateObj) throws KException {
        String stateObjName = null;
        if(stateObj!=null) {
            this.stateObjects.put(key, stateObj);
            stateObjName = stateObj.getName(getTransaction());
        } else {
            this.stateObjects.remove(key);
        }
        setProvidedProperty(key,stateObjName);
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#removeStateObject(java.lang.String)
     */
    @Override
    public void removeStateObject(String key) {
        this.stateObjects.remove(key);
        setProvidedProperty(key,null);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#getCurrentContextLabelProvider()
     */
    @Override
    public KomodoObjectLabelProvider getCurrentContextLabelProvider() {
        return this.currentContextLabelProvider;
    }

	/**
	 * {@inheritDoc}
	 *
	 * @see org.komodo.shell.api.WorkspaceStatus#getObjectLabelProvider(KomodoObject)
	 */
	@Override
	public KomodoObjectLabelProvider getObjectLabelProvider(KomodoObject kobject) {
		if (lastUsedLabelProvider != null &&
		    !lastUsedLabelProvider.getClass().getName().equals(DefaultLabelProvider.class.getName()) &&
		    lastUsedLabelProvider.getTypeDisplay(uow, kobject) != null ) {
			return lastUsedLabelProvider;
		}
		// If an alternate provider yields a type for this KomodoObject, it is used.  Otherwise, the defaultProvider is used.
    	 KomodoObjectLabelProvider resultLabelProvider = null;
         if(!this.alternateLabelProviders.isEmpty()) {
             for(KomodoObjectLabelProvider altProvider : this.alternateLabelProviders) {
                 if( !StringUtils.isEmpty(altProvider.getTypeDisplay(getTransaction(),kobject)) ) {
                     resultLabelProvider = altProvider;
                     break;
                 }
             }
         }
         lastUsedLabelProvider=(resultLabelProvider != null) ? resultLabelProvider : defaultLabelProvider;
         return lastUsedLabelProvider;
	}

    /*
     * Set the Label provider for the supplied context
     * @param context the context
     */
    private void setLabelProvider(KomodoObject context) {
        this.currentContextLabelProvider = getObjectLabelProvider(context);
    }

	@Override
	public String getTypeDisplay(final KomodoObject kObj) {
		String type=currentContextLabelProvider.getTypeDisplay(getTransaction(), kObj);
		if(type!=null){
			return type;
		}else{
			return  defaultLabelProvider.getTypeDisplay(getTransaction(), kObj);
		}

	}

    @Override
    public List<String> getProvidedStatusMessages( final KomodoObject kObj ) {
        List<String> allMessages = new ArrayList<String>();
        if(!this.commandFactory.getCommandProviders().isEmpty()) {
            for(ShellCommandProvider provider : this.commandFactory.getCommandProviders()) {
                String statusMessage = null;
                try {
                    statusMessage = provider.getStatusMessage(getTransaction(), kObj);
                } catch (KException ex) {
                    // just set message null
                }
                if(!StringUtils.isBlank(statusMessage)) {
                    allMessages.add(statusMessage);
                }
            }
        }
        return allMessages;
    }

    @Override
    public void initProvidedStates( final Properties globalProps ) throws KException {
        if(!this.commandFactory.getCommandProviders().isEmpty()) {
            for(ShellCommandProvider provider : this.commandFactory.getCommandProviders()) {
                provider.initWorkspaceState(this);
            }
        }
    }

    @Override
    public < T extends KomodoObject > T resolve ( final KomodoObject kObj ) throws KException {
        if(!this.commandFactory.getCommandProviders().isEmpty()) {
            for(ShellCommandProvider provider : this.commandFactory.getCommandProviders()) {
                T resolvedObj = provider.resolve(getTransaction(), kObj);
                if(resolvedObj!=null) return resolvedObj;
            }
        }
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#isAutoCommit()
     */
    @Override
    public boolean isAutoCommit() {
        assert ( this.wsProperties.containsKey( AUTO_COMMIT ) );
        return Boolean.parseBoolean( this.wsProperties.getProperty( AUTO_COMMIT ) );
    }

    private void discoverLabelProviders( ) {
        final List< ClassLoader > commandClassloaders = new ArrayList< >();
        commandClassloaders.add( Thread.currentThread().getContextClassLoader() );

        // Find providers in the user's commands directory
        final String userHome = System.getProperty( "user.home", "/" ); //$NON-NLS-1$ //$NON-NLS-2$
        final String commandsDirName = System.getProperty( "komodo.shell.commandsDir", userHome + "/.komodo/commands" ); //$NON-NLS-1$ //$NON-NLS-2$
        LOGGER.debug( "WorkspaceStatusImpl: commands directory is \"{0}\"", commandsDirName ); //$NON-NLS-1$
        final File commandsDir = new File( commandsDirName );

        if ( !commandsDir.exists() ) {
            commandsDir.mkdirs();
        }

        if ( commandsDir.isDirectory() ) {
            try {
                final Collection< File > jarFiles = FileUtils.getFilesForPattern( commandsDir.getCanonicalPath(), "", ".jar" ); //$NON-NLS-1$ //$NON-NLS-2$

                if ( !jarFiles.isEmpty() ) {
                    final List< URL > jarURLs = new ArrayList< >( jarFiles.size() );

                    for ( final File jarFile : jarFiles ) {
                        final URL jarUrl = jarFile.toURI().toURL();
                        jarURLs.add( jarUrl );
                        LOGGER.debug( "WorkspaceStatusImpl: adding discovered jar \"{0}\"", jarUrl ); //$NON-NLS-1$
                    }

                    final URL[] urls = jarURLs.toArray( new URL[ jarURLs.size() ] );
                    final ClassLoader extraCommandsCL = new URLClassLoader( urls,
                                                                            Thread.currentThread().getContextClassLoader() );
                    commandClassloaders.add( extraCommandsCL );
                }
            } catch ( final IOException e ) {
                KEngine.getInstance().getErrorHandler().error( e );
            }
        }

        // iterate through the ClassLoaders and use the Java ServiceLoader mechanism to load the providers
        for ( final ClassLoader classLoader : commandClassloaders ) {
            for ( final KomodoObjectLabelProvider provider : ServiceLoader.load( KomodoObjectLabelProvider.class, classLoader ) ) {
                if ( !Modifier.isAbstract( provider.getClass().getModifiers() ) ) {
                    provider.setRepository(getEngine().getDefaultRepository());
                    provider.setWorkspaceStatus(this);
                    LOGGER.debug( "WorkspaceStatusImpl: adding LabelProvider \"{0}\"", provider.getClass().getName() ); //$NON-NLS-1$
                    this.alternateLabelProviders.add( provider );
                }
            }
        }

        LOGGER.debug( "WorkspaceStatusImpl: found \"{0}\" LabelProviders", alternateLabelProviders.size() ); //$NON-NLS-1$
    }

    /**
     * A class to make sure commit and rollback is not called directly on the UnitOfWork. We want associated WorkspaceStatus
     * methods to be called instead.
     */
    class WorkspaceStatusTransaction extends RepositoryImpl.UnitOfWorkImpl {

        private final UnitOfWork delegate;

        WorkspaceStatusTransaction( final UnitOfWork delegate ) {
            super( delegate.getName(),
                   ( ( RepositoryImpl.UnitOfWorkImpl )delegate ).getSession(),
                   delegate.isRollbackOnly(),
                   delegate.getCallback() );
            this.delegate = delegate;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#commit()
         */
        @Override
        public void commit() {
            try {
                WorkspaceStatusImpl.this.commit( UNKNOWN_SOURCE );
            } catch ( final Exception e ) {
                assert false;
                // should be handled in main class rollback
            }
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#decode(java.lang.String)
         */
        @Override
        public String decode( final String encoded ) {
            return this.delegate.decode( encoded );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#getCallback()
         */
        @Override
        public UnitOfWorkListener getCallback() {
            return this.delegate.getCallback();
        }

        UnitOfWork getDelegate() {
            return this.delegate;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#getError()
         */
        @Override
        public KException getError() {
            return this.delegate.getError();
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#getName()
         */
        @Override
        public String getName() {
            return this.delegate.getName();
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#getState()
         */
        @Override
        public State getState() {
            return this.delegate.getState();
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#hasChanges()
         */
        @Override
        public boolean hasChanges() throws KException {
            return this.delegate.hasChanges();
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#isRollbackOnly()
         */
        @Override
        public boolean isRollbackOnly() {
            return this.delegate.isRollbackOnly();
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.Repository.UnitOfWork#rollback()
         */
        @Override
        public void rollback() {
            try {
                WorkspaceStatusImpl.this.rollback( UNKNOWN_SOURCE );
            } catch ( final Exception e ) {
                assert false;
                // should be handled in main class rollback
            }
        }

    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#getContextForDisplayPath(java.lang.String)
     */
    @Override
    public KomodoObject getContextForDisplayPath(String displayPath) {
        if(StringUtils.isBlank(displayPath)) return getCurrentContext();

        // check path for cd into root options
        if ( displayPath.equals( FORWARD_SLASH ) ) {
            return getRootContext();
        }

        // If supplied path doesnt start with FORWARD_SLASH, it should be relative to current context
        String entireDisplayPath = displayPath;
        if(!displayPath.startsWith(FORWARD_SLASH)) {
            if(KomodoObjectUtils.isRoot(getCurrentContext())) {
                entireDisplayPath = FORWARD_SLASH+displayPath;
            } else {
                entireDisplayPath = getCurrentContextDisplayPath()+FORWARD_SLASH+displayPath;
            }
        }

        entireDisplayPath = removePathDots(entireDisplayPath);
        // check path for cd into root options
        if ( entireDisplayPath.equals( FORWARD_SLASH ) ) {
            return getRootContext();
        }

        String repoPath = getCurrentContextLabelProvider().getPath(entireDisplayPath);
        if(repoPath==null) return null;

        KomodoObject resultObject = null;
        try {
            resultObject = getRootContext().getRepository().getFromWorkspace(getTransaction(), repoPath);
        } catch (KException ex) {
            // Failed to locate the object
        }

        if(resultObject!=null) {
            try {
                final KomodoObject resolved = resolve( resultObject );

                if ( resolved != null ) {
                    return resolved;
                }
            } catch (KException ex) {
                LOGGER.debug( "WorkspaceStatusImpl: problem resolving object" ); //$NON-NLS-1$
            }
        }
        return resultObject;
    }

    /*
     * Remove '..' and '.' segments from a display path
     */
    private String removePathDots(String absoluteDisplayPath) {
        ArgCheck.isNotNull(absoluteDisplayPath);
        String[] segments = absoluteDisplayPath.split(FORWARD_SLASH);

        List<String> newSegments = new ArrayList<String>();
        for(String segment : segments) {
            // Dot stays in same place
            if(segment.equals(DOT) || StringUtils.isBlank(segment)) {
                continue;
            // Dot dot go up
            } else if(segment.equals(DOT_DOT)) {
                if(newSegments.size()>0) {
                    newSegments.remove(newSegments.size()-1);
                }
            } else {
                newSegments.add(segment);
            }
        }

        // Construct the new path without dots
        StringBuilder sb = new StringBuilder(FORWARD_SLASH);
        for(int i=0; i<newSegments.size(); i++) {
            sb.append(newSegments.get(i));
            if(i != newSegments.size()-1) {
                sb.append(FORWARD_SLASH);
            }
        }

        return sb.toString();
    }

}
