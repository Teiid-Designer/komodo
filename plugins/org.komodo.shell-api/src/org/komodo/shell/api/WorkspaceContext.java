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

import java.util.List;

import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;

/**
 * The interface WorkspaceContext
 */
public interface WorkspaceContext {

	@SuppressWarnings("javadoc")
	public static final String WORKSPACE_ROOT_DISPLAY_NAME = "workspace"; //$NON-NLS-1$
	
    /**
     * Represents all komodo object types
     */
	String ALL_TYPES = "ALL TYPES"; //$NON-NLS-1$

    /**
	 * Get the name
	 * @return the context name
	 * @throws Exception if error occurs
	 */
	String getName() throws Exception;

	/**
	 * Get the type
	 * @return the type
	 * @throws Exception if error occurs
	 */
	String getType() throws Exception;

	/**
	 * Get the allowable child types
	 * @return the list of allowable types
	 */
	List<String> getAllowableChildTypes();

	/**
	 * Get the workspace status
	 * @return the workspace status
	 */
	WorkspaceStatus getWorkspaceStatus();

	/**
	 * Get the workspace manager
	 * @return the workspace manager
	 * @throws Exception if error occurs
	 */
	WorkspaceManager getWorkspaceManager() throws Exception;
	
	/**
	 * Get the parent context
	 * @return the parent
	 */
	WorkspaceContext getParent();

	/**
	 * Get all children
	 * @return the list of children
	 * @throws Exception if error occurs
	 */
	List<WorkspaceContext> getChildren() throws Exception;

	/**
     * Get the child context of the given name
     * @param name the name
     * @return the child
     * @throws Exception if error occurs
     */
    WorkspaceContext getChild(String name) throws Exception;

	/**
	 * Get the child context of the given name and type
	 * @param name the name
	 * @param type the type
	 * @return the child
	 * @throws Exception if error occurs
	 */
	WorkspaceContext getChild(String name, String type) throws Exception;

	/**
	 * Get the full name path for this context.  e.g. root.parentContext.thisContext
	 * @return the full name
	 * @throws Exception if errors occur
	 */
	String getFullName() throws Exception;

	/**
	 * Determine if the context is within relational model
	 * @return 'true' if relational
	 */
	boolean isRelational();

	/**
	 * Get relational object at this context
	 * @return the KomodoObject, null if not applicable
	 */
	KomodoObject getKomodoObj();

	/**
	 * Get the property names at this context
	 * @return the list of property names
	 * @throws Exception if an error occurs
	 */
	List<String> getProperties() throws Exception;

	/**
	 * Get the value for the supplied propertyName.  If no property with the supplied name is found,
	 * returns null.
	 * @param propertyName
	 * @return the value of the property with this name, null if property with name was not found.
	 * @throws Exception if an error occurs
	 */
	String getPropertyValue(String propertyName) throws Exception;

	/**
	 * @param propertyName
	 * @param value
	 * @throws Exception
	 */
	void setPropertyValue(String propertyName, Object value) throws Exception;

    /**
     * @return the repository
     * @throws Exception if error occurs
     */
    Repository getRepository() throws Exception;

    /**
     * @param visitor
     * @return result of visit with the visitor
     * @throws Exception if error occurs
     */
    Object visit(WorkspaceContextVisitor visitor) throws Exception;

}
