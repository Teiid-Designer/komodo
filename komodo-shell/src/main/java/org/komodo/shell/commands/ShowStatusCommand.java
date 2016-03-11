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
package org.komodo.shell.commands;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} that shows contributed workspace status items.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * show-status
 * </code>
 */
public class ShowStatusCommand extends BuiltInShellCommand {

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
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            printStatus();
            return CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 0;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ShellI18n.showStatusHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.showStatusExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.showStatusUsage ) );
    }

	/**
	 * Shows the status at the current workspace context
	 * @throws Exception
	 */
	private void printStatus( ) throws Exception {
        WorkspaceStatus wsStatus = getWorkspaceStatus();
        KomodoObject context = wsStatus.getCurrentContext();

        // Repo info
        final Repository.Id repoId = context.getRepository().getId();
        print(MESSAGE_INDENT, I18n.bind(ShellI18n.currentRepoName, repoId.getWorkspaceName()));
        print(MESSAGE_INDENT, I18n.bind(ShellI18n.currentRepoUrl, repoId.getUrl()));

        // Current Context
        KomodoObject currentContext = wsStatus.getCurrentContext();
        final String path = wsStatus.getDisplayPath(currentContext, null);
        print(MESSAGE_INDENT, I18n.bind(ShellI18n.currentContext, path));

        // Get additional provided status messages
        List<String> stateObjectStatusList = wsStatus.getProvidedStatusMessages();
        for(String status : stateObjectStatusList) {
            print(MESSAGE_INDENT, status);
        }
	}

}
