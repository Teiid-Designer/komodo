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
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.komodo.core.KEngine;
import org.komodo.importer.ImportMessages;
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

    /**
     * Set to <code>true</code> if the full path of the current context object should be displayed in the prompt.
     */
    String SHOW_FULL_PATH_IN_PROMPT_KEY = "SHOW_FULL_PATH_IN_PROMPT"; //$NON-NLS-1$

    /**
     * Set to <code>true</code> if the hidden properties should also be displayed.
     */
    String SHOW_HIDDEN_PROPERTIES_KEY = "SHOW_HIDDEN_PROPERTIES"; //$NON-NLS-1$

    /**
     * Set to <code>true</code> if property namespace prefixes should be shown.
     */
    String SHOW_PROP_NAME_PREFIX_KEY = "SHOW_PROP_NAME_PREFIX"; //$NON-NLS-1$

    /**
     * Set to <code>true</code> if the type of the current context object should be displayed in the prompt.
     */
    String SHOW_TYPE_IN_PROMPT_KEY = "SHOW_TYPE_IN_PROMPT"; //$NON-NLS-1$

    /**
     * An unmodifiable collection of the valid global property names and their non-<code>null</code> default values.
     */
    Map< String, String > GLOBAL_PROPS = Collections.unmodifiableMap( new HashMap< String, String >() {

        private static final long serialVersionUID = 1L;

        // add default values for EVERY property
        {
            put( EXPORT_DEFAULT_DIR_KEY, "." ); //$NON-NLS-1$
            put( IMPORT_DEFAULT_DIR_KEY, "." ); //$NON-NLS-1$
            put( RECORDING_FILE_KEY, "./commandOutput.txt" ); //$NON-NLS-1$
            put( SHOW_FULL_PATH_IN_PROMPT_KEY, Boolean.FALSE.toString() );
            put( SHOW_HIDDEN_PROPERTIES_KEY, Boolean.FALSE.toString() );
            put( SHOW_PROP_NAME_PREFIX_KEY, Boolean.FALSE.toString() );
            put( SHOW_TYPE_IN_PROMPT_KEY, Boolean.TRUE.toString() );
        }
    } );

    /**
     * Sets the specified global property value.
     *
     * @param propKey
     *        the property key (cannot be empty)
     * @param propValue
     *        the property value (can be empty when changing to the default value)
     */
    void setProperty( final String propKey,
                      final String propValue );

    /**
     * Sets global workspace properties on startup
     *
     * @param props
     *        the properties (can be <code>null</code> or empty if resetting to default values)
     * @throws Exception
     *         if an error occurs
     */
    void setProperties( final Properties props ) throws Exception;

	/**
	 * Set all properties back to their default values.
	 */
	void resetProperties();

    /**
     * Gets a copy of the global workspace properties.
     *
     * @return the global workspace properties (never <code>null</code> or empty)
     * @see #setProperty(String, String)
     */
    Properties getProperties();

    /**
     * @param propertyName the name of the property being checked (cannot be empty)
     * @return <code>true</code> if a known boolean property
     */
    boolean isBooleanProperty( final String propertyName );

    /**
     * @param propertyName
     *        the name of the property being checked (cannot be empty)
     * @param proposedValue
     *        the proposed value (can be empty if removing or resetting to the default value)
     * @return an error message or <code>null</code> if name and proposed value are valid
     */
    String validateGlobalPropertyValue( final String propertyName,
                                        final String proposedValue );

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
     * @return <code>true</code> if the full object path is being displayed in the prompt
     * @see #SHOW_FULL_PATH_IN_PROMPT_KEY
     */
    boolean isShowingFullPathInPrompt();

    /**
     * @return <code>true</code> if hidden properties are being displayed
     * @see #SHOW_HIDDEN_PROPERTIES_KEY
     */
    boolean isShowingHiddenProperties();

    /**
     * @return <code>true</code> if property namespace prefixes should be shown
     * @see #SHOW_PROP_NAME_PREFIX_KEY
     */
    boolean isShowingPropertyNamePrefixes();

    /**
     * @return <code>true</code> if property namespace prefixes should be shown
     * @see #SHOW_TYPE_IN_PROMPT_KEY
     */
    boolean isShowingTypeInPrompt();

    /**
     * @return current teiid model
     */
    Teiid getTeiid();

    /**
     * Determine if workspace currently has a connected teiid instance
     * @return <code>true</code> if has a connected teiid instance
     */
    boolean hasConnectedTeiid();

    /**
     * Set the current teiid model
     *
     * @param teiid
     * @throws Exception 
     */
    void setTeiid(Teiid teiid) throws Exception;

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
     * Commit
     * @param source identifier for commit
     * @param importMessages collects import messages
     * @throws Exception
     */
	void commitImport( final String source, ImportMessages importMessages ) throws Exception;
    
    /**
     * Rolls back any unsaved changes.
     *
     * @param source
     *        the identifier used to name the transaction created after the rollback (cannot be empty)
     * @throws Exception
     *         if an error occurs
     */
    void rollback( final String source ) throws Exception;

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
