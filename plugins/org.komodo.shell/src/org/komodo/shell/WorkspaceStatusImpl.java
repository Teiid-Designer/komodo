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
import java.io.InputStream;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
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

    private Properties wsProperties = new Properties();

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

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#setProperty(java.lang.String, java.lang.String)
     */
    @Override
    public void setProperty(String propKey, String propValue) {
    	if(!StringUtils.isEmpty(propKey)) {
    		if(WorkspaceStatus.GLOBAL_PROP_KEYS.contains(propKey.toUpperCase())) {
    			this.wsProperties.put(propKey, propValue);
    		}
    	}
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#setProperties(java.util.Properties)
     */
    @Override
    public void setProperties(Properties props) {
    	this.wsProperties.clear();
    	for (final String name : props.stringPropertyNames()) {
    		if(WorkspaceStatus.GLOBAL_PROP_KEYS.contains(name.toUpperCase())) {
    			this.wsProperties.put(name, props.getProperty(name));
    		}
    	}
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#getProperties()
     */
    @Override
	public Properties getProperties() {
    	return this.wsProperties;
    }

}
