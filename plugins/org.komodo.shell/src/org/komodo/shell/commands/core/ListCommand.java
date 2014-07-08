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
	 * @param name the command name
	 * @param wsStatus the workspace status
	 */
	public ListCommand(String name, WorkspaceStatus wsStatus) {
		super(name,wsStatus);
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
		
		if(wsStatus.getRecordingStatus()) recordCommand(getArguments());

		return true;
	}

}
