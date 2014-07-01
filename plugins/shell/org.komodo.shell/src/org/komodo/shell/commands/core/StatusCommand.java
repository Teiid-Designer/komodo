/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands.core;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * Displays a summary of the current status, including what repository the
 * user is currently connected to (if any).
 *
 */
public class StatusCommand extends BuiltInShellCommand {

	/**
	 * Constructor.
	 */
	public StatusCommand() {
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
		WorkspaceStatus wsStatus = getWorkspaceStatus();

		String currentRepo = "local Repository"; //$NON-NLS-1$
		String currentServer = "[none : not connected]"; //$NON-NLS-1$
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		
		// Current Repository
		print(Messages.getString("StatusCommand.CurrentRepo", currentRepo)); //$NON-NLS-1$
		
		// Current Server
		print(Messages.getString("StatusCommand.CurrentServer", currentServer)); //$NON-NLS-1$
		
		// Current Context path
		print(Messages.getString("StatusCommand.CurrentContext", currentContext.getFullName())); //$NON-NLS-1$

		return true;
	}

}
