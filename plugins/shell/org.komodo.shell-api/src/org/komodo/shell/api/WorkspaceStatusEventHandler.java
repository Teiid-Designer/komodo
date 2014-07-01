/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.api;


/**
 * Implement this interface to listen for workspace Status events.
 *
 */
public interface WorkspaceStatusEventHandler {

	/**
	 * Called when workspace context changes
	 */
	public void workspaceContextChanged();


}
