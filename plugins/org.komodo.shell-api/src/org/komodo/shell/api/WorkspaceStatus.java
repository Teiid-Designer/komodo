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
import java.util.Properties;
import org.komodo.core.KEngine;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.TeiidInstance;

/**
 * WorkspaceStatus interface.
 */
public interface WorkspaceStatus extends StringConstants {

    /**
     * The type id of the root object
     */
    String ROOT_TYPE = FORWARD_SLASH;

    @SuppressWarnings("javadoc")
	String RECORDING_FILEPATH_KEY = "RECORDING_FILEPATH"; //$NON-NLS-1$

    
	/**
	 * Allows set workspace properties on startup
	 * @param props the properties
	 */
	void setProperties(Properties props);

	/**
	 * Get the home context
	 * @return the home context
	 */
	WorkspaceContext getRootContext();
	
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
	 * Set the Teiid URL
	 * @param recordingOutputFilePath the recording output file path
	 */
	void setRecordingOutputFile(String recordingOutputFilePath);

    /**
     * @return the parent of the teiid instance
     */
    IShellTeiidParent getTeiidParent();

    /**
     * @return current teiid instance
     */
    TeiidInstance getTeiidInstance();

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
     * @param contextId
     * @return workspace context with given id
     */
    WorkspaceContext getWorkspaceContext(String contextId);

    /**
     * @param contextId
     * @param context
     */
    void addWorkspaceContext(String contextId, WorkspaceContext context);

}
