/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.api;

/**
 * WorkspaceStatus interface.
 */
public interface WorkspaceStatus {

	
	/**
	 * Get the root context
	 * @return the root context
	 */
	public WorkspaceContext getRootContext();
	
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
	 * Add a WorkspaceContext Event Handler
	 * @param handler the workspace context eventHandler
	 */
	public void addHandler(WorkspaceStatusEventHandler handler);

	/**
	 * Remove a WorkspaceContext Event Handler
	 * @param handler the workspace context eventHandler
	 */
	public void removeHandler(WorkspaceStatusEventHandler handler);
	

}
