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
import org.komodo.core.KEngine;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.api.WorkspaceStatusEventHandler;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * Test implementation of WorkspaceStatus
 */
public class WorkspaceStatusImpl implements WorkspaceStatus {

    /* The library context is where all artifacts are stored */
    private WorkspaceContextImpl rootContext;

    /* Cache of context to avoid creating needless duplicate contexts */
    private Map<String, WorkspaceContext> contextCache = new HashMap<String, WorkspaceContext>();

    private WorkspaceContext currentContext;
    private Set<WorkspaceStatusEventHandler> eventHandlers = new HashSet<WorkspaceStatusEventHandler>();
    private boolean recordingStatus = false;
    private File recordingOutputFile;
    private final InputStream inStream;
    private final PrintStream outStream;
    private final KEngine kEngine;

    private Teiid teiid;

    /**
     * Constructor
     * @param kEngine the komodo engine
     * @param outStream output stream
     * @param inStream input stream
     * @throws Exception error on initialisation failure
     */
    public WorkspaceStatusImpl(KEngine kEngine, InputStream inStream, PrintStream outStream) throws Exception {
        this.kEngine = kEngine;
        this.inStream = inStream;
        this.outStream = outStream;
        init();
    }

    private void init() throws Exception {
        Repository repo = getEngine().getDefaultRepository();
        KomodoObject komodoWksp = repo.komodoWorkspace(null);

        KomodoObject komodoRoot = komodoWksp.getParent(null);

        rootContext = new WorkspaceContextImpl(this, null, komodoRoot);
        contextCache.put(komodoRoot.getAbsolutePath(), rootContext);

        currentContext = rootContext;
    }

    @Override
    public KEngine getEngine() {
        return kEngine;
    }

    @Override
    public InputStream getInputStream() {
        return inStream;
    }

    @Override
    public PrintStream getOutputStream() {
        return outStream;
    }

    @Override
    public WorkspaceContext getWorkspaceContext(String contextId) {
        return contextCache.get(contextId);
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
     * @see org.komodo.shell.api.WorkspaceStatus#getRootContext()
     */
    @Override
    public WorkspaceContext getRootContext() {
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
     * @see org.komodo.shell.api.WorkspaceStatus#getRecordingOutputFile()
     */
    @Override
    public File getRecordingOutputFile() {
        return this.recordingOutputFile;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#setRecordingOutputFile(java.lang.String)
     */
    @Override
    public void setRecordingOutputFile(String recordingOutputFilePath) {
        this.recordingOutputFile = new File(recordingOutputFilePath);
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.api.WorkspaceStatus#setProperties(java.util.Properties)
     */
    @Override
    public void setProperties(Properties props) {
        for (final String name : props.stringPropertyNames()) {
            if (name.equals(RECORDING_FILEPATH_KEY)) {
                setRecordingOutputFile(props.getProperty(name));
            }
        }
    }

}
