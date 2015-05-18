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
package org.komodo.shell.api;

import java.io.File;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;
import org.komodo.core.KEngine;
import org.komodo.relational.teiid.Teiid;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * WorkspaceStatus interface.
 */
public interface WorkspaceStatus extends StringConstants {

    /**
     * The type id of the root object
     */
    String WORKSPACE_TYPE = "WORKSPACE"; //$NON-NLS-1$

    @SuppressWarnings("javadoc")
	public static final String RECORDING_FILE_KEY = "RECORDING_FILE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
	public static final String IMPORT_DEFAULT_DIR_KEY = "IMPORT_DEFAULT_DIR"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
	public static final String EXPORT_DEFAULT_DIR_KEY = "EXPORT_DEFAULT_DIR"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final List<String> GLOBAL_PROP_KEYS = 
    		Arrays.asList(RECORDING_FILE_KEY, IMPORT_DEFAULT_DIR_KEY, EXPORT_DEFAULT_DIR_KEY);    
    
	/**
	 * Sets the specified global property value
	 * @param propKey the property key
	 * @param propValue the property value
	 */
	void setProperty(String propKey, String propValue);
	
	/**
	 * Sets global workspace properties on startup
	 * @param props the properties
	 */
	void setProperties(Properties props);

	/**
	 * Gets global workspace properties
	 * @return the global workspace properties
	 */
	Properties getProperties();

	/**
	 * Get the workspace context
	 * @return the workspace context
	 */
	WorkspaceContext getWorkspaceContext();

	/**
	 * Set the current workspace context
	 * @param context the current workspace context
	 * @throws Exception if error occurs
	 */
	void setCurrentContext(WorkspaceContext context) throws Exception;

	/**
	 * Get the current workspace context
	 * @return the current workspace context
	 */
	WorkspaceContext getCurrentContext();

	/**
	 * Toggles the recording status 'on' or 'off'
	 * @param recordState 'true' to enable recording, 'false' to disable
	 */
	void setRecordingStatus(boolean recordState);

	/**
	 * Get the recording status
	 * @return 'true' if recording is enabled, 'false' if not.
	 */
	boolean getRecordingStatus();

	/**
	 * Get the recording output file path
	 * @return the output file
	 */
	File getRecordingOutputFile();

	/**
     * @return current teiid model
     */
    Teiid getTeiid();

    /**
     * Set the current teiid model
     *
     * @param teiid
     */
    void setTeiid(Teiid teiid);

	/**
	 * Add a WorkspaceContext Event Handler
	 * @param handler the workspace context eventHandler
	 */
	void addHandler(WorkspaceStatusEventHandler handler);

	/**
	 * Remove a WorkspaceContext Event Handler
	 * @param handler the workspace context eventHandler
	 */
	void removeHandler(WorkspaceStatusEventHandler handler);

    /**
     * @return the input stream
     */
    InputStream getInputStream();

    /**
     * @return the output stream
     */
    PrintStream getOutputStream();

    /**
     * @return the komodo engine
     */
    KEngine getEngine();

    /**
     * Commit 
     * @param source identifier for commit
     * @throws Exception 
     */
    void commit(String source) throws Exception;
    
    /**
     * @return the current transaction (never <code>null</code>)
     */
    UnitOfWork getTransaction();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     */
    void setTransaction( final UnitOfWork transaction );

    /**
     * @param contextId
     * @return workspace context with given id
     */
    WorkspaceContext getWorkspaceContext(String contextId);

    /**
     * @param contextId
     * @param context
     */
    void addWorkspaceContext(String contextId, WorkspaceContext context);

    /**
     * @return the parent komodo shell
     */
    KomodoShell getShell();

}
