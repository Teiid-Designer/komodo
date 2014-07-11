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

import java.util.Date;
import java.util.List;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * Enable or disable recording of commands to a file
 *
 */
public class RecordCommand extends BuiltInShellCommand {

	private static final String ON = "ON"; //$NON-NLS-1$
	private static final String OFF = "OFF"; //$NON-NLS-1$
	
	/**
	 * Constructor
	 * @param name the command name
	 * @param wsStatus the workspace status
	 */
	public RecordCommand(String name, WorkspaceStatus wsStatus) {
		super(name,wsStatus);
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
		String onOffArg = requiredArgument(0, Messages.getString("RecordCommand.onOffArg_empty")); //$NON-NLS-1$

		if (!this.validate(onOffArg)) {
			return false;
		}

		WorkspaceStatus wsStatus = getWorkspaceStatus();
		if(onOffArg.equalsIgnoreCase("on")) { //$NON-NLS-1$
			wsStatus.setRecordingStatus(true);
		} else if(onOffArg.equalsIgnoreCase("off")) { //$NON-NLS-1$
			wsStatus.setRecordingStatus(false);
		}
		
		Date d = new Date();
		String rState = wsStatus.getRecordingStatus() ? ON : OFF; 
		String stateChangedMsg = Messages.getString("RecordCommand.setRecordingStateMsg",rState,d.toString()); //$NON-NLS-1$
		
        print(CompletionConstants.MESSAGE_INDENT,stateChangedMsg);  

        recordComment("====== "+stateChangedMsg+" ======"); //$NON-NLS-1$ //$NON-NLS-2$
        
        return true;
	}
	
	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.komodo.shell.commands.archive.AbstractArchiveCommand#validate
	 * (java.lang.String[])
	 */
	protected boolean validate(String... args) {
		String onOffArg = args[0].trim();
		if(onOffArg.length()==0) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RecordCommand.onOffArg_empty")); //$NON-NLS-1$
			return false;
		}
		
		if(!onOffArg.equalsIgnoreCase("on") && !onOffArg.equalsIgnoreCase("off")) { //$NON-NLS-1$ //$NON-NLS-2$
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RecordCommand.onOffArg_invalid")); //$NON-NLS-1$
			return false;
		}
		return true;
	}
	
	/**
	 * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
	 */
	@Override
	public int tabCompletion(String lastArgument, List<CharSequence> candidates) {
		if (getArguments().isEmpty()) {
			if (lastArgument == null) {
				candidates.add("on "); //$NON-NLS-1$
				candidates.add("off "); //$NON-NLS-1$
				return 0;
			} else if ("on".startsWith(lastArgument)) { //$NON-NLS-1$
				candidates.add("on "); //$NON-NLS-1$
				return 0;
			} else if ("off".startsWith(lastArgument)) { //$NON-NLS-1$
				candidates.add("off "); //$NON-NLS-1$
				return 0;
			}
		} 
		return -1;
	}

}
