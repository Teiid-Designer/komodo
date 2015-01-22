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

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.TraverseWorkspaceVisitor;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * Displays a recursive list of the objects beneath the current context
 *
 */
public class NavigateCommand extends BuiltInShellCommand {

    private static final String NAVIGATE = "navigate"; //$NON-NLS-1$

	/**
	 * Constructor.
	 * @param wsStatus the workspace status
	 */
	public NavigateCommand(WorkspaceStatus wsStatus) {
		super(NAVIGATE,wsStatus);
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
		WorkspaceStatus wsStatus = getWorkspaceStatus();
		
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		TraverseWorkspaceVisitor visitor = new TraverseWorkspaceVisitor();
        Object pathway = currentContext.visit(visitor);
        print(0, pathway.toString());

		return true;
	}

}
