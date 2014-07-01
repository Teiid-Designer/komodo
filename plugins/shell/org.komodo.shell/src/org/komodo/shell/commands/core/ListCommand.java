/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands.core;

import java.util.List;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * Displays a summary of the current status, including what repository the
 * user is currently connected to (if any).
 *
 */
public class ListCommand extends BuiltInShellCommand {

	/**
	 * Constructor.
	 */
	public ListCommand() {
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
		WorkspaceStatus wsStatus = getWorkspaceStatus();
		
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		
		List<WorkspaceContext> children = currentContext.getChildren();
		if(children.isEmpty()) { 
			print("No items to show."); //$NON-NLS-1$
		}
		
		for(WorkspaceContext childContext : children) {
			String childName = childContext.getName();
			WorkspaceContext.Type childType = childContext.getType();
			print(childName+" ["+childType+"]"); //$NON-NLS-1$ //$NON-NLS-2$ 
		}
		return true;
	}

}
