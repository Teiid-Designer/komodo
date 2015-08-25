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
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * ListCommand - shows the children of a KomodoObject.
 *
 */
public class ListCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "list"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ListCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME, "ls", "ll" ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        if (!validate(getArguments())) {
            return false;
        }

        ShellCommand showChildrenCommand = getWorkspaceStatus().getCommand(ShowChildrenCommand.NAME);
        showChildrenCommand.setArguments(getArguments());

        showChildrenCommand.execute();

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

    protected boolean validate(Arguments allArgs) throws Exception {
        // optional path arg
        if(!allArgs.isEmpty()) {
            // Optional path arg
            String pathArg = optionalArgument(0);
            if(!validatePath(pathArg)) {
                return false;
            }
        }

        return true;
    }

    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {

        if (getArguments().isEmpty()) {
            // The arg is expected to be a path
            updateTabCompleteCandidatesForPath(candidates, getContext(), true, lastArgument);

            // Do not put space after it - may want to append more to the path
            return CompletionConstants.NO_APPEND_SEPARATOR;
            // Tab completion for "property" - expects a valid property for the current context.
        }

        return -1;
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

}
