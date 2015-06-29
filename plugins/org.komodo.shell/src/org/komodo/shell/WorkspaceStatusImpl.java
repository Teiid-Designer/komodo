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
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.komodo.core.KEngine;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.SynchronousCallback;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.KomodoShell;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.api.WorkspaceStatusEventHandler;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.KException;
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

    private static final String PROPERTIES_FILE_NAME = "vdbbuilder.properties"; //$NON-NLS-1$
    private static final String SAVED_CONTEXT_PATH = "SAVED_CONTEXT_PATH"; //$NON-NLS-1$
    private static final List< String > HIDDEN_PROPS = Arrays.asList( new String[] { SAVED_CONTEXT_PATH } );

    private final KomodoShell shell;

    /* Workspace Context */
    private WorkspaceContextImpl wsContext;

    /* Cache of context to avoid creating needless duplicate contexts */
    private Map<String, WorkspaceContext> contextCache = new HashMap<String, WorkspaceContext>();

    private UnitOfWork uow; // the current transaction
    private SynchronousCallback callback;

    private int count = 0; // commit count

    private WorkspaceContext currentContext;
    private Set<WorkspaceStatusEventHandler> eventHandlers = new HashSet<WorkspaceStatusEventHandler>();

    private Properties wsProperties;

    private boolean recordingStatus = false;

    private Teiid teiid;

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
                                final KomodoShell shell ) throws Exception {
        this.shell = shell;
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

        final Repository repo = getEngine().getDefaultRepository();
        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(repo);

        wsContext = new WorkspaceContextImpl(this,null,wkspMgr);
        contextCache.put( wkspMgr.getAbsolutePath(), wsContext );

        currentContext = wsContext;

        // load saved state
        this.wsProperties = new Properties();
        loadProperties();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#save()
     */
    @Override
    public void save() throws Exception {
        saveProperties();
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
    public PrintStream getOutputStream() {
        return shell.getOutputStream();
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
    public WorkspaceContext getWorkspaceContext(String contextPath) {
        if (contextPath == null)
            return null;

        WorkspaceContext context = contextCache.get(contextPath);
        if (context != null)
            return context;

        if (FORWARD_SLASH.equals(contextPath))
            return getWorkspaceContext();

        //
        // Need to hunt for the path
        // contextPath should be of the form /tko:komodo/tko:workspace/vdb/.../...
        //
        String[] contexts = contextPath.split("\\/"); //$NON-NLS-1$
        context = getWorkspaceContext();
        for (int i = 0; i < contexts.length; ++i) {
            try {
                context = context.getChild(contexts[i]);
            } catch (Exception ex) {
                return null;
            }
        }

        return context;
    }

    @Override
    public void addWorkspaceContext(String contextId, WorkspaceContext context) {
        contextCache.put(contextId, context);
    }

    @Override
    public Teiid getTeiid() {
        return teiid;
    }

    @Override
    public void setTeiid(Teiid teiid) {
        this.teiid = teiid;
    }

    @Override
    public void setCurrentContext(WorkspaceContext context) throws Exception {
        currentContext = context;
        fireContextChangeEvent();
    }

    @Override
    public WorkspaceContext getCurrentContext() {
        return currentContext;
    }

    /**
     * @return the contextCache
     */
    public Map<String, WorkspaceContext> getContextCache() {
        return this.contextCache;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#getWorkspaceContext()
     */
    @Override
    public WorkspaceContext getWorkspaceContext() {
        return wsContext;
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
     * @see org.komodo.shell.api.WorkspaceStatus#getRecordingOutputFile()
     */
    @Override
    public File getRecordingOutputFile() {
    	String filePath = this.wsProperties.getProperty(WorkspaceStatus.RECORDING_FILE_KEY);
    	if(StringUtils.isEmpty(filePath)) {
    		return null;
    	}
        return new File(filePath);
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

    private void loadProperties() throws Exception {
        final String propFileName = this.shell.getShellDataLocation();
        final File propFile = new File( propFileName, PROPERTIES_FILE_NAME );

        if ( propFile.exists() ) {
            this.wsProperties.load( new FileInputStream( propFile.getAbsolutePath() ) );
        }

        // make sure all non-hidden global properties exist
        for ( final Entry< String, String > entry : GLOBAL_PROPS.entrySet() ) {
            if ( !this.wsProperties.containsKey( entry.getKey() ) ) {
                setProperty( entry.getKey(), entry.getValue() );
            }
        }

        // set current context to saved context
        final String savedPath = this.wsProperties.getProperty( SAVED_CONTEXT_PATH );

        if (!StringUtils.isBlank( savedPath )) {
            this.currentContext = ContextUtils.getContextForPath( this, savedPath );

            // saved path no longer exists so set context to workspace root
            if ( this.currentContext == null ) {
                this.currentContext = getWorkspaceContext();
            }
        }
    }

    private void saveProperties() throws Exception {
        final String propFileName = this.shell.getShellDataLocation();
        final File propFile = new File( propFileName, PROPERTIES_FILE_NAME );

        // save current context path
        this.wsProperties.setProperty( SAVED_CONTEXT_PATH, this.currentContext.getFullName() );

        // save
        this.wsProperties.store( new FileOutputStream( propFile.getAbsolutePath() ), null );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#resetProperties()
     */
    @Override
    public void resetProperties() {
        for ( final Entry< String, String > entry : GLOBAL_PROPS.entrySet() ) {
            setProperty( entry.getKey(), entry.getValue() );
        }
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
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.WorkspaceStatus#setProperties(java.util.Properties)
     */
    @Override
    public void setProperties( final Properties props ) {
        resetProperties();

        if ( ( props != null ) && !props.isEmpty() ) {
            for ( final String name : props.stringPropertyNames() ) {
                setProperty( name, props.getProperty( name ) );
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
        final Properties result = new Properties(); // just provide a copy

        // remove hidden properties
        for ( final String propName : this.wsProperties.stringPropertyNames() ) {
            if ( !HIDDEN_PROPS.contains( propName ) ) {
                result.setProperty( propName, this.wsProperties.getProperty( propName ) );
            }
        }

        return result;
    }

}
