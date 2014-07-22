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
import org.komodo.spi.runtime.ITeiidInstance;

/**
 * WorkspaceStatus interface.
 */
public interface WorkspaceStatus {

    @SuppressWarnings("javadoc")
	public final String RECORDING_FILEPATH_KEY = "RECORDING_FILEPATH"; //$NON-NLS-1$

    
	/**
	 * Allows set workspace properties on startup
	 * @param props the properties
	 */
	public void setProperties(Properties props);

	/**
	 * Get the home context
	 * @return the home context
	 */
	public WorkspaceContext getHomeContext();
	
	/**
	 * Set the current workspace context
	 * @param context the current workspace context
	 */
	public void setCurrentContext(WorkspaceContext context);
	
	/**
	 * Get the current workspace context
	 * @return the current workspace context
	 */
	public WorkspaceContext getCurrentContext();

	/**
	 * Toggles the recording status 'on' or 'off'
	 * @param recordState 'true' to enable recording, 'false' to disable
	 */
	public void setRecordingStatus(boolean recordState);
	
	/**
	 * Get the recording status
	 * @return 'true' if recording is enabled, 'false' if not.
	 */
	public boolean getRecordingStatus();

	/**
	 * Get the recording output file path
	 * @return the output file
	 */
	public File getRecordingOutputFile();
	
	/**
	 * Set the Teiid URL
	 * @param recordingOutputFilePath the recording output file path
	 */
	public void setRecordingOutputFile(String recordingOutputFilePath);

    /**
     * @return the parent of the teiid instance
     */
    IShellTeiidParent getTeiidParent();

    /**
     * @return current teiid instance
     */
    ITeiidInstance getTeiidInstance();

	/**
	 * Add a WorkspaceContext Event Handler
	 * @param handler the workspace context eventHandler
	 */
	public void addHandler(WorkspaceStatusEventHandler handler);

	/**
	 * Remove a WorkspaceContext Event Handler
	 * @param handler the workspace context eventHandler
	 */
	public void removeHandler(WorkspaceStatusEventHandler handler);

    /**
     * @return the input stream
     */
    public InputStream getInputStream();

    /**
     * @return the output stream
     */
    public PrintStream getOutputStream();

}
