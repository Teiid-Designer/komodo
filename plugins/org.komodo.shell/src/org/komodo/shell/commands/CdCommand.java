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

import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.StringUtils;

/**
 * CdCommand - allows changing the workspace context
 *
 */
public class CdCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "cd"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public CdCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME, "edit", "goto" ); //$NON-NLS-1$ //$NON-NLS-2$
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
    		String displayPath = requiredArgument(0, Messages.getString(SHELL.InvalidArgMsg_EntryPath));

    		// Validate the display Path
    		String validationMsg = validatePath(displayPath);
    		if(!validationMsg.equals(CompletionConstants.OK)) {
    		    return new CommandResultImpl(false, validationMsg, null);
    		}

    		WorkspaceStatus wsStatus = getWorkspaceStatus();
    		KomodoObject newContext = wsStatus.getContextForDisplayPath(displayPath.trim());

    		if(newContext!=null) {
    			getWorkspaceStatus().setCurrentContext(newContext);
    			return CommandResult.SUCCESS;
    		}
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }

        return CommandResult.FAIL;
	}

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 1;
    }

	/**
	 * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
	 */
	@Override
	public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
		if (getArguments().isEmpty()) {
			KomodoObject currentContext = getWorkspaceStatus().getCurrentContext();

			// The arg is expected to be a path
			updateTabCompleteCandidatesForPath(candidates, currentContext, true, lastArgument);

            return ( candidates.isEmpty() ? -1 : ( StringUtils.isBlank( lastArgument ) ? 0 : ( toString().length() + 1 ) ) );
		}

		return -1;
	}

}
