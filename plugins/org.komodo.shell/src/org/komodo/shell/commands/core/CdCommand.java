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
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.ContextUtils;
import org.komodo.spi.constants.StringConstants;

/**
 * Cd command - allows changing the workspace context
 *
 */
public class CdCommand extends BuiltInShellCommand implements StringConstants {

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
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
		String locationArg = requiredArgument(0, Messages.getString(SHELL.InvalidArgMsg_EntryPath));

		if (!this.validate(locationArg)) {
			return false;
		}

		String locArg = locationArg.trim();
		WorkspaceStatus wsStatus = getWorkspaceStatus();
		WorkspaceContext newContext = ContextUtils.getContextForPath(wsStatus, locArg);

		if(newContext!=null) {
			getWorkspaceStatus().setCurrentContext(newContext);
			return true;
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.komodo.shell.commands.archive.AbstractArchiveCommand#validate
	 * (java.lang.String[])
	 */
	protected boolean validate(String... args) throws Exception {
		if (!validatePath(args[0])) {
			return false;
		}
		return true;
	}

	/**
	 * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
	 */
	@Override
	public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
		if (getArguments().isEmpty()) {
			WorkspaceContext currentContext = getWorkspaceStatus().getCurrentContext();

			// The arg is expected to be a path
			updateTabCompleteCandidatesForPath(candidates, currentContext, true, lastArgument);

			// Do not put space after it - may want to append more to the path
			return CompletionConstants.NO_APPEND_SEPARATOR;
		}
		return -1;
	}

}
