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
import java.util.Map;

import org.komodo.relational.model.RelationalObject;

/**
 * The interface WorkspaceContext
 */
public interface WorkspaceContext {
	
	/**
	 * The context type
	 */
	public enum Type {
		@SuppressWarnings("javadoc")
		ALL,
		@SuppressWarnings("javadoc")
		HOME,
		@SuppressWarnings("javadoc")
		PROJECT,
		@SuppressWarnings("javadoc")
		TABLE,
		@SuppressWarnings("javadoc")
		COLUMN,
		@SuppressWarnings("javadoc")
		PROCEDURE,
		@SuppressWarnings("javadoc")
		PARAMETER,
		@SuppressWarnings("javadoc")
		RESULT_SET,
		@SuppressWarnings("javadoc")
		SCHEMA,
		@SuppressWarnings("javadoc")
		VIEW,
		@SuppressWarnings("javadoc")
		UNIQUE_CONSTRAINT,
		@SuppressWarnings("javadoc")
		ACCESS_PATTERN,
		@SuppressWarnings("javadoc")
		PRIMARY_KEY,
		@SuppressWarnings("javadoc")
		FOREIGN_KEY,
		@SuppressWarnings("javadoc")
		INDEX,
		@SuppressWarnings("javadoc")
		MODEL
	}
	
	/**
	 * Get the name
	 * @return the context name
	 */
	public String getName();

	/**
	 * Set the name
	 * @param name the context name
	 */
	public void setName(String name);

	/**
	 * Get the type
	 * @return the type
	 */
	public Type getType();

	/**
	 * Set the type
	 * @param type the type
	 */
	public void setType(Type type);

	/**
	 * Get the workspace status
	 * @return the workspace status
	 */
	public WorkspaceStatus getWorkspaceStatus();
	
	/**
	 * Get the parent context
	 * @return the parent
	 */
	public WorkspaceContext getParent();
	
	/**
	 * Get all children
	 * @return the list of children
	 */
	public List<WorkspaceContext> getChildren();

	/**
	 * Get the child context of the given name and type
	 * @param name the name
	 * @param type the type
	 * @return the child
	 */
	public WorkspaceContext getChild(String name, Type type);
	
	/**
	 * Get the full name path for this context.  e.g. root.parentContext.thisContext
	 * @return the full name
	 */
	public String getFullName();
	
	/**
	 * Determine if the context is within relational model
	 * @return 'true' if relational
	 */
	public boolean isRelational();
	
	/**
	 * Get the relational obj type which corresponds to the context type
	 * @param ctxType the context type
	 * @return the relational object type
	 */
	public int getRelationalObjTypeForWsContextType(WorkspaceContext.Type ctxType);
	
	/**
	 * Get the context type type which corresponds to the relational object type
	 * @param relObjType the relational object type
	 * @return the context type
	 */
	public WorkspaceContext.Type getWsContextTypeForRelationalObjType(int relObjType);
	
	/**
	 * Get relational object at this context
	 * @return the relationalObject, null if not applicable
	 */
	public RelationalObject getRelationalObj();

	/**
	 * Get relational models at project context
	 * @return the relationalObject list
	 */
	public List<RelationalObject> getModels();
	
	/**
	 * Get the property name value map at this context
	 * @return the map of property name-value
	 */
	public Map<String,String> getPropertyNameValueMap();

	/**
	 * Get the valid objects that can be created at this context
	 * @return the list of valid types
	 */
	public List<String> getValidTypesForCreate();

	/**
	 * Add a child context
	 * @param child the child
	 */
	public void addChild(Object child);
	
}
