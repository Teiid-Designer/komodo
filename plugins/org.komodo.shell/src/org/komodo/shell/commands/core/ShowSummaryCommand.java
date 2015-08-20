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
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;

/**
 * Show Command.  Has various acceptable args.
 * show < properties | children | status | global | property | summary >
 *
 */
public class ShowSummaryCommand extends BuiltInShellCommand implements StringConstants {

    /**
     * The command name.
     */
    public static final String NAME = "show-summary"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ShowSummaryCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
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

        WorkspaceStatus wsStatus = getWorkspaceStatus();
        String pathArg = optionalArgument(0);
        WorkspaceContext theContext = ContextUtils.getContextForPath(wsStatus, pathArg);

        ShellCommand showPropertiesCommand = getWorkspaceStatus().getCommand(ShowPropertiesCommand.NAME);
        ShellCommand showChildrenCommand = getWorkspaceStatus().getCommand(ShowChildrenCommand.NAME);
        if(theContext!=null) {
            showPropertiesCommand.setArguments(getArguments());
            showChildrenCommand.setArguments(getArguments());
        }

        showPropertiesCommand.execute();
        print();
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#shouldCommit()
     */
    @Override
    protected boolean shouldCommit() {
        return false;
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
    
}
