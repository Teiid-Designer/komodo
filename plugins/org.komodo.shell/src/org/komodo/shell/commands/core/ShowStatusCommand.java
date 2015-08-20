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

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository;

/**
 * Show Status Command.
 *
 */
public class ShowStatusCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * The command name.
     */
    public static final String NAME = "show-status"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ShowStatusCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
		printStatus( );
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#shouldCommit()
     */
    @Override
    protected boolean shouldCommit() {
        return false;
    }

	/**
	 * Shows the status at the current workspace context
	 * @throws Exception
	 */
	private void printStatus( ) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();
        WorkspaceContext context = wsStatus.getCurrentContext();

        // Repo info
        final Repository.Id repoId = context.getRepository().getId();
        print(MESSAGE_INDENT, Messages.getString(Messages.ShowCommand.CurrentRepoName, repoId.getWorkspaceName()));
        print(MESSAGE_INDENT, Messages.getString(Messages.ShowCommand.CurrentRepoUrl, repoId.getUrl()));

		// Teiid Instance info
//		Teiid teiid = wsStatus.getTeiid();
//		if (teiid == null)
//			print(MESSAGE_INDENT, Messages.getString(Messages.ShowCommand.NoCurrentTeiid));
//		else {
//			TeiidInstance teiidInstance = teiid.getTeiidInstance(wsStatus.getTransaction());
//            String teiidName = teiid.getName(wsStatus.getTransaction());
//			String teiidUrl = teiidInstance.getUrl();
//			String teiidConnected = teiidInstance.isConnected() ? Messages.getString(Messages.ShowCommand.Connected) : Messages.getString(Messages.ShowCommand.NotConnected);
//            String currentServerText = Messages.getString(Messages.ShowCommand.serverStatusText, teiidName, teiidUrl, teiidConnected);
//            print(MESSAGE_INDENT, Messages.getString(Messages.ShowCommand.CurrentTeiid, currentServerText));
//		}

		// Current Context
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		print(MESSAGE_INDENT, Messages.getString(Messages.ShowCommand.CurrentContext, currentContext.getFullName()));
	}

}
