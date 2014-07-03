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
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * Displays a summary of the current status, including what repository the
 * user is currently connected to (if any).
 *
 */
public class CdCommand extends BuiltInShellCommand {

	/**
	 * Constructor
	 */
	public CdCommand() {
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

		WorkspaceStatus wsStatus = getWorkspaceStatus();
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		
		String locArg = locationArg.trim();
		if("..".equals(locArg)) { //$NON-NLS-1$
			getWorkspaceStatus().setCurrentContext(currentContext.getParent());
			return true;
		} 
		
		// See if matching child
		boolean foundMatch = false;
		WorkspaceContext newContext = null;
		for(WorkspaceContext childContext : currentContext.getChildren()) {
			if(childContext.getName().equals(locArg)) {
				newContext = childContext;
				foundMatch = true;
				break;
			}
		}
		if(foundMatch) {
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
	protected boolean validate(String... args) {
		if (!validateLocationArg(args[0])) {
			return false;
		}
		return true;
	}
	
	private boolean validateLocationArg(String location) {
		String locArg = location.trim();
		if(locArg.length()==0) {
            print(Messages.getString("CdCommand.locationArg_empty")); //$NON-NLS-1$
			return false;
		}
		WorkspaceStatus wsStatus = getWorkspaceStatus();
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		// See if command to go up
		if("..".equals(locArg)) {  //$NON-NLS-1$
			if(currentContext.getParent()==null) {
	            print(Messages.getString("CdCommand.locationArg_cantCdUp")); //$NON-NLS-1$
				return false;
			} 
			return true;
		}
		// See if matching child
		boolean foundMatch = false;
		for(WorkspaceContext childContext : currentContext.getChildren()) {
			if(childContext.getName().equals(locArg)) {
				foundMatch = true;
				break;
			}
		}
		if(!foundMatch) {
            print(Messages.getString("CdCommand.locationArg_noChildWithThisName")); //$NON-NLS-1$
			return false;
		}
		return true;
	}

//	/**
//	 * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
//	 */
//	@Override
//	public int tabCompletion(String lastArgument, List<CharSequence> candidates) {
//		int toReturn = FeedTabCompleter.tabCompletion(getArguments(), getContext(), lastArgument, candidates);
//		if (getArguments().size() == 1) {
//			if (lastArgument == null)
//				lastArgument = ""; //$NON-NLS-1$
//			FileNameCompleter delegate = new FileNameCompleter();
//			return delegate.complete(lastArgument, lastArgument.length(), candidates);
//		}
//		return toReturn;
//	}

}
