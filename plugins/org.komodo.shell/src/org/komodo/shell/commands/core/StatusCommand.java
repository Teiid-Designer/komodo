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
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.ExecutionAdmin.ConnectivityType;
import org.komodo.spi.runtime.TeiidInstance;

/**
 * Displays a summary of the current status, including what repository the
 * user is currently connected to (if any).
 *
 */
public class StatusCommand extends BuiltInShellCommand implements StringConstants {

	/**
	 * Constructor.
	 * @param name the command name
	 * @param wsStatus the workspace status
	 */
	public StatusCommand(String name, WorkspaceStatus wsStatus) {
		super(name,wsStatus);
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
		WorkspaceStatus wsStatus = getWorkspaceStatus();

		// Repo info
		String currentRepo = "local Repository"; //$NON-NLS-1$
		print(CompletionConstants.MESSAGE_INDENT,Messages.getString("StatusCommand.CurrentRepo", currentRepo)); //$NON-NLS-1$
		
        // Teiid Instance info
        TeiidInstance teiidInstance = wsStatus.getTeiidInstance();
        String teiidUrl = teiidInstance.getUrl();
        String teiidConnected = teiidInstance.isConnected() ?
            Messages.getString(Messages.StatusCommand.Connected) :
                Messages.getString(Messages.StatusCommand.NotConnected);

        String teiidJdbcUrl = teiidInstance.getTeiidJdbcInfo().getUrl();
        /* Only test jdbc connection if teiid instance has been connected to */
        String teiidJdbcConnected = teiidConnected;
        if (teiidInstance.isConnected()) {
            teiidJdbcConnected = teiidInstance.ping(ConnectivityType.JDBC).isOK() ?
                Messages.getString(Messages.StatusCommand.PingOk) :
                    Messages.getString(Messages.StatusCommand.PingFail);
        }

        String currentTeiidInst = OPEN_SQUARE_BRACKET + teiidUrl + " : " + teiidConnected + CLOSE_SQUARE_BRACKET; //$NON-NLS-1$
        String currentTeiidJdbc = OPEN_SQUARE_BRACKET + teiidJdbcUrl + " : " + teiidJdbcConnected + CLOSE_SQUARE_BRACKET; //$NON-NLS-1$
        print(CompletionConstants.MESSAGE_INDENT, Messages.getString("StatusCommand.CurrentTeiid", currentTeiidInst)); //$NON-NLS-1$
        print(CompletionConstants.MESSAGE_INDENT, Messages.getString("StatusCommand.CurrentTeiidJdbc", currentTeiidJdbc)); //$NON-NLS-1$

		// Current Context
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		print(CompletionConstants.MESSAGE_INDENT,Messages.getString("StatusCommand.CurrentContext", currentContext.getFullName())); //$NON-NLS-1$
		
		// Echo command if recording on
		if(wsStatus.getRecordingStatus()) recordCommand(getArguments());
		
		return true;
	}

}
