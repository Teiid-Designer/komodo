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
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.komodo.core.KEngine;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.SynchronousCallback;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.KomodoShell;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.api.WorkspaceStatusEventHandler;
import org.komodo.shell.util.ContextUtils;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;

/**
 * Test implementation of WorkspaceStatus
 */
public class WorkspaceStatusImpl implements WorkspaceStatus {

    private static final String SERVER_DEFAULT_KEY = "SERVER_DEFAULT"; //$NON-NLS-1$
    private static final String SAVED_CONTEXT_PATH = "SAVED_CONTEXT_PATH"; //$NON-NLS-1$
    private static final List< String > HIDDEN_PROPS = Arrays.asList( new String[] { SAVED_CONTEXT_PATH, SERVER_DEFAULT_KEY } );

    private final KomodoShell shell;

    /* Root Context */
    private KomodoObject rootContext;

    private UnitOfWork uow; // the current transaction
    private SynchronousCallback callback;

    private int count = 0; // commit count

    private KomodoObject currentContext;
    private Set<WorkspaceStatusEventHandler> eventHandlers = new HashSet<WorkspaceStatusEventHandler>();

    private Properties wsProperties;

    private boolean recordingStatus = false;
    private Writer recordingFileWriter = null;
    private ShellCommandFactory commandFactory;

    private String server;

    /**
     * Constructor
     * @param shell parent shell
     * @param commandFactory the command factory
     * @throws Exception error on initialisation failure
     */
    public WorkspaceStatusImpl(KomodoShell shell, ShellCommandFactory commandFactory) throws Exception {
        this( null, shell, commandFactory );
    }

    /**
     * @param transaction
     *        the transaction to use initially in the shell (can be <code>null</code> if one should be created)
     * @param shell
     *        parent shell
     * @param commandFactory the command factory
     * @throws Exception
     *         error on initialisation failure
     */
    public WorkspaceStatusImpl( final UnitOfWork transaction,
                                final KomodoShell shell,
                                final ShellCommandFactory commandFactory) throws Exception {
        this.shell = shell;
        this.commandFactory = commandFactory;
        init(transaction);
    }

    private void init( final UnitOfWork transaction ) throws Exception {
        if ( transaction == null ) {
            createTransaction("init"); //$NON-NLS-1$
        } else {
            this.uow = transaction;
            Repository.UnitOfWorkListener uowListener = transaction.getCallback();
            if(uowListener!=null && uowListener instanceof SynchronousCallback) {
                this.callback = (SynchronousCallback)uowListener;
            }
        }
        
        // The default root context is komodoRoot.  The providers may specify a higher root (as in WorkspaceManager)
        final Repository repo = getEngine().getDefaultRepository();
        final KomodoObject komodoRoot = new ObjectImpl( repo, RepositoryImpl.KOMODO_ROOT, 0 );
        
        // Determine if any children of komodo node were designated as root
        KomodoObject[] children = komodoRoot.getChildren(getTransaction());
        for(KomodoObject child : children) {
            KomodoObject resolvedChild = resolve(child);
            if(isRoot(resolvedChild)) {
                rootContext = resolvedChild;
                break;
            }
        }

        // No child designated, set the komodo node as root
        if(rootContext==null) {
            rootContext = !komodoRoot.getClass().equals(org.komodo.repository.ObjectImpl.class) ? komodoRoot : resolve(komodoRoot);
        }

        currentContext = rootContext;

        // initialize the global properties
        this.wsProperties = initGlobalProperties();
    }
    
    private Properties initGlobalProperties() {
        Properties props = new Properties();
        
        // load shell properties if they exist
        final String dataDir = this.shell.getShellDataLocation();
        final File startupPropertiesFile = new File( dataDir, this.shell.getShellPropertiesFile() );

        if ( startupPropertiesFile.exists() && startupPropertiesFile.isFile() && startupPropertiesFile.canRead() ) {
            try {
                props.load( new FileInputStream( startupPropertiesFile ) );
            } catch ( final Exception e ) {
                String msg = Messages.getString( SHELL.ERROR_LOADING_PROPERTIES,
                                                 startupPropertiesFile.getAbsolutePath(),
                                                 e.getMessage() );
                PrintUtils.print(getOutputWriter(), CompletionConstants.MESSAGE_INDENT, msg);
            }
        }
        
        // Init the recording output file if it is defined
        String recordingFile = props.getProperty(RECORDING_FILE_KEY);
        if(!StringUtils.isBlank(recordingFile)) {
            setRecordingWriter(recordingFile);
        }

        return props;
    }

    private void createTransaction(final String source ) throws Exception {
        final Repository repo = getEngine().getDefaultRepository();
        this.callback = new SynchronousCallback();
        this.uow = repo.createTransaction( ( getClass().getSimpleName() + ':' + source + '-' + this.count++ ), false, callback );
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
        this.uow = transaction;
    }

    @Override
    public void commit( final String source ) throws Exception {
        final String txName = this.uow.getName();
        this.uow.commit();

        final boolean success = this.callback.await( 3, TimeUnit.MINUTES );
        if ( success ) {
            final KException error = uow.getError();
            final State txState = this.uow.getState();

            if ( ( error != null ) || !State.COMMITTED.equals( txState ) ) {
                throw new KException( Messages.getString( SHELL.TRANSACTION_COMMIT_ERROR, txName ), error );
            }
        } else {
            throw new KException( Messages.getString( SHELL.TRANSACTION_TIMEOUT, txName ) );
        }

        // create new transaction
        createTransaction(source);
    }
    
//    @Override
//  public void commitImport( final String source, ImportMessages importMessages ) throws Exception {
//        final String txName = this.uow.getName();
//        this.uow.commit();
//
//        final boolean success = this.callback.await( 3, TimeUnit.MINUTES );
//        if ( success ) {
//          // For imports, if has callback error - add to import errors and return.
//          if(this.callback.hasError()) {
//              importMessages.addErrorMessage(callback.error());
//              createTransaction(source);
//              return;
//          }
//            final KException error = uow.getError();
//            final State txState = this.uow.getState();
//
//            if ( ( error != null ) || !State.COMMITTED.equals( txState ) ) {
//                throw new KException( Messages.getString( SHELL.TRANSACTION_COMMIT_ERROR, txName ), error );
//            }
//        } else {
//            throw new KException( Messages.getString( SHELL.TRANSACTION_TIMEOUT, txName ) );
//        }
//
//        // create new transaction
//        createTransaction(source);
//    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#rollback(java.lang.String)
     */
    @Override
    public void rollback( final String source ) throws Exception {
        final String txName = this.uow.getName();
        this.uow.rollback();

        final boolean success = this.callback.await( 3, TimeUnit.MINUTES );

        if ( success ) {
            final KException error = uow.getError();
            final State txState = this.uow.getState();

            if ( ( error != null ) || !State.ROLLED_BACK.equals( txState ) ) {
                throw new KException( Messages.getString( SHELL.TRANSACTION_ROLLBACK_ERROR, txName ), error );
            }
        } else {
            throw new KException( Messages.getString( SHELL.TRANSACTION_TIMEOUT, txName ) );
        }

        // create new transaction
        createTransaction(source);
    }

    @Override
    public String getServer() {
        // If teiid is not set, look at global default.  use it if found.
        if( server == null ) {
            String globalDefaultServer = getProperties().getProperty(SERVER_DEFAULT_KEY);
            if(!StringUtils.isEmpty(globalDefaultServer)) {
                setServer(globalDefaultServer);
            }
        }
        return server;
    }

    @Override
    public void setServer(String server) {
        this.server = server;
        // Set the hidden property SERVER_DEFAULT_KEY
        String serverName = server==null ? StringConstants.EMPTY_STRING : server;
        this.wsProperties.setProperty( SERVER_DEFAULT_KEY, serverName);
    }

    @Override
    public void setCurrentContext(KomodoObject context) throws Exception {
        currentContext = context;
        String objFullName = KomodoObjectUtils.getFullName(this, context);
        this.wsProperties.setProperty( SAVED_CONTEXT_PATH, objFullName );
        fireContextChangeEvent();
    }

    @Override
    public KomodoObject getCurrentContext() {
        return currentContext;
    }

    @Override
    public String getCurrentContextType() {
        return getTypeDisplay(currentContext);
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#getRootContext()
     */
    @Override
    public KomodoObject getRootContext() {
        return rootContext;
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
        return propertyName.equals( SHOW_FULL_PATH_IN_PROMPT_KEY ) || propertyName.equals( SHOW_HIDDEN_PROPERTIES_KEY )
               || propertyName.equals( SHOW_PROP_NAME_PREFIX_KEY ) || propertyName.equals( SHOW_TYPE_IN_PROMPT_KEY );
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
            return Messages.getString( SHELL.INVALID_GLOBAL_PROPERTY_NAME, propertyName );
        }

        // empty value means they want to remove or reset to default value
        if ( StringUtils.isEmpty( proposedValue ) ) {
            return null; // name and value are valid
        }

        if ( isBooleanProperty( propertyName ) ) {
            if ( Boolean.parseBoolean( proposedValue ) || "false".equalsIgnoreCase( ( proposedValue ) ) ) { //$NON-NLS-1$
                return null;
            }

            return Messages.getString( SHELL.INVALID_BOOLEAN_GLOBAL_PROPERTY_VALUE, proposedValue, propertyName.toUpperCase() );
        }

        return null; // name and value are valid
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#setProperty(java.lang.String, java.lang.String)
     */
    @Override
    public void setProperty( final String name,
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
                    setProperty( name, null );
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
     * @see org.komodo.shell.api.WorkspaceStatus#setProperties(java.util.Properties)
     */
    @Override
    public void setProperties( final Properties props ) throws Exception {
        if ( ( props != null ) && !props.isEmpty() ) {
            for ( final String name : props.stringPropertyNames() ) {
                setProperty( name, props.getProperty( name ) );
            }

            // set current context to saved context if necessary
            final String savedPath = props.getProperty( SAVED_CONTEXT_PATH );

            if ( !StringUtils.isBlank( savedPath ) ) {
                KomodoObject context = ContextUtils.getContextForPath(this, savedPath );

                // saved path no longer exists so set context to workspace root
                if ( context == null ) {
                    context = getRootContext();
                }

                setCurrentContext( context );
            }
            
            // set default teiid if necessary
            final String defaultServer = props.getProperty( SERVER_DEFAULT_KEY );

            if ( !StringUtils.isBlank( defaultServer ) ) {
                setServer(defaultServer);
            }
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
                PrintUtils.print(commandWriter,CompletionConstants.MESSAGE_INDENT, Messages.getString(SHELL.RecordingFileCannotWrite, recordingFilePath));
                return;
            }
            recordingFileWriter = new FileWriter(outputFile,true);
        } catch (IOException ex) {
            PrintUtils.print(commandWriter, 0, Messages.getString(SHELL.RecordingFileOutputError,outputFile));
        }
    }
    
    @Override
    public void closeRecordingWriter() {
        if(this.recordingFileWriter!=null) {
            try {
                this.recordingFileWriter.close();
            } catch (IOException ex) {
            }
        }
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#getProperties()
     */
    @Override
    public Properties getProperties() {
        final Properties copy = new Properties(); // just provide a copy

        for ( final String propName : this.wsProperties.stringPropertyNames() ) {
            copy.setProperty( propName, this.wsProperties.getProperty( propName ) );
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
        return this.commandFactory.getCommand( commandName );
    }
    
    @Override
    public < T extends KomodoObject > T resolve ( final KomodoObject kObj ) throws KException {
        if(this.commandFactory.getCommandProviders()!=null) {
            for(ShellCommandProvider provider : this.commandFactory.getCommandProviders()) {
                KomodoObject resolvedObj = provider.resolve(getTransaction(), kObj);
                if(resolvedObj!=null) return (T)resolvedObj;
            }
        }
        return (T)kObj;
    }
    
    @Override
    public boolean isRoot ( final KomodoObject kObj ) throws KException {
        // parent null or FORWARD_SLASH path is default root
        KomodoObject parentObj = kObj.getParent(getTransaction());
        if(parentObj==null || parentObj.getAbsolutePath().equals(FORWARD_SLASH)) {
            return true;
        }
        // determine if another root has been specified as the root
        boolean isRoot = false;
        if(this.commandFactory.getCommandProviders()!=null) {
            for(ShellCommandProvider provider : this.commandFactory.getCommandProviders()) {
                if(provider.isRoot(getTransaction(), kObj)) {
                    isRoot = true;
                    break;
                }
            }
        }
        return isRoot;
    }

    @Override
    public String getTypeDisplay ( final KomodoObject kObj ) {
        if(this.commandFactory.getCommandProviders()!=null) {
            for(ShellCommandProvider provider : this.commandFactory.getCommandProviders()) {
                String typeString = null;
                try {
                    typeString = provider.getTypeDisplay(getTransaction(), kObj);
                } catch (KException ex) {
                }
                if(typeString!=null) return typeString;
            }
        }
        return kObj.getClass().getSimpleName();
    }
        
}
