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

import java.util.ArrayList;
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

	private static final String ROOT_OPT1 = WorkspaceStatus.ROOT_TYPE;
	private static final String ROOT_OPT2 = ROOT_OPT1 + ContextUtils.ROOT_CONTEXT_NAME;
	private static final String ROOT_OPT3 = ROOT_OPT2 + WorkspaceStatus.ROOT_TYPE;
	
    /**
	 * Constructor
	 * @param name the command name
	 * @param wsStatus the workspace status
	 */
	public CdCommand(String name, WorkspaceStatus wsStatus) {
		super(name,wsStatus);
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
		
		// check for cd into root
		if( locArg.equalsIgnoreCase(ROOT_OPT1) || locArg.equalsIgnoreCase(ROOT_OPT2) || locArg.equalsIgnoreCase(ROOT_OPT3)) { 
			wsStatus.setCurrentContext(wsStatus.getRootContext());
			if(wsStatus.getRecordingStatus()) recordCommand(getArguments());
			return true;
		}
		
		WorkspaceContext newContext = null;
		// Location supplied as absolute path
		if(ContextUtils.isAbsolutePath(locArg)) {
			newContext = ContextUtils.getContextForAbsolutePath(getWorkspaceStatus().getRootContext(), locArg);
		// Location supplied as relative path
		} else {
			newContext = ContextUtils.getRelativeContext(currentContext, locArg);
		}
		
		if(newContext!=null) {
			getWorkspaceStatus().setCurrentContext(newContext);
			if(wsStatus.getRecordingStatus()) recordCommand(getArguments());
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
		if (!validateLocationArg(args[0])) {
			return false;
		}
		return true;
	}
	
	private boolean validateLocationArg(String location) throws Exception {
		String locArg = location.trim();
		if(locArg.length()==0) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("CdCommand.locationArg_empty")); //$NON-NLS-1$
			return false;
		}
		WorkspaceContext currentContext = getWorkspaceStatus().getCurrentContext();

		// check for cd into root
		if( locArg.equalsIgnoreCase(ROOT_OPT1) || locArg.equalsIgnoreCase(ROOT_OPT2) || locArg.equalsIgnoreCase(ROOT_OPT3)) { 
			return true;
		}
		
		WorkspaceContext newContext = null;
		// Location supplied as absolute path
		if(ContextUtils.isAbsolutePath(locArg)) {
			newContext = ContextUtils.getContextForAbsolutePath(getWorkspaceStatus().getRootContext(), locArg);
		// Location supplied as relative path
		} else {
			newContext = ContextUtils.getRelativeContext(currentContext, locArg);
		}
		
		if(newContext==null) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("CdCommand.locationArg_noContextWithThisName")); //$NON-NLS-1$
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
			
			// List of potentials completions
			List<String> potentialsList = new ArrayList<String>();
			// Only offer '..' if below the root
			if(currentContext.getType() != WorkspaceStatus.ROOT_TYPE) {
				potentialsList.add(StringConstants.DOT_DOT);
			}
			
			// --------------------------------------------------------------
			// No arg - offer children relative current context.
			// --------------------------------------------------------------
			if(lastArgument==null) {
				List<WorkspaceContext> children = currentContext.getChildren();
				for(WorkspaceContext wsContext : children) {
					potentialsList.add(wsContext.getName()+ContextUtils.PATH_SEPARATOR);
				}
				candidates.addAll(potentialsList);
			// --------------------------------------------------------------
			// One arg - determine the completion options for it.
			// --------------------------------------------------------------
			} else {
				// --------------------------------------------
				// Absolute Path Arg handling
				// --------------------------------------------
				if( lastArgument.startsWith(ContextUtils.PATH_SEPARATOR) ) {
					// If not the full absolute root, then provide it
					if(!ContextUtils.isAbsolutePath(lastArgument)) {
						potentialsList.add(ROOT_OPT3);
						updateCandidates(candidates,potentialsList,lastArgument);
				    // Starts with correct root - provide next option
					} else {
						String relativePath = ContextUtils.convertAbsolutePathToRootRelative(lastArgument);
						WorkspaceContext deepestMatchingContext = ContextUtils.getDeepestMatchingContextRelative(getWorkspaceStatus().getRootContext(), relativePath);
						
						// Get children of deepest context match to form potentialsList
						List<WorkspaceContext> children = deepestMatchingContext.getChildren();
						if(!children.isEmpty()) {
							// Get all children as potentials
							for(WorkspaceContext childContext : children) {
								String absolutePath = ContextUtils.PATH_SEPARATOR + childContext.getFullName();
								potentialsList.add(absolutePath+ContextUtils.PATH_SEPARATOR);
							}
						} else {
							String absolutePath = ContextUtils.PATH_SEPARATOR + deepestMatchingContext.getFullName();
							potentialsList.add(absolutePath+ContextUtils.PATH_SEPARATOR);
						}
						updateCandidates(candidates, potentialsList, lastArgument);
					}
				// -------------------------------------------
			    // Relative Path Arg handling
			    // -------------------------------------------
				} else {
					// Deepest matching context for relative path
					WorkspaceContext deepestMatchingContext = ContextUtils.getDeepestMatchingContextRelative(currentContext, lastArgument);
					
					// Get children of deepest context match to form potentialsList
					List<WorkspaceContext> children = deepestMatchingContext.getChildren();
					if(!children.isEmpty()) {
						// Get all children as potentials
						for(WorkspaceContext childContext : children) {
							String absolutePath = ContextUtils.PATH_SEPARATOR + childContext.getFullName();
							String relativePath = ContextUtils.convertAbsolutePathToRelative(currentContext, absolutePath);
							potentialsList.add(relativePath+ContextUtils.PATH_SEPARATOR);
						}
					} else {
						String absolutePath = ContextUtils.PATH_SEPARATOR + deepestMatchingContext.getFullName();
						String relativePath = ContextUtils.convertAbsolutePathToRelative(currentContext, absolutePath);
						potentialsList.add(relativePath+ContextUtils.PATH_SEPARATOR);
					}
					updateCandidates(candidates, potentialsList, lastArgument);
				}
			}
			// Do not put space after it - may want to append more to the path
			if(candidates.size()==1) {
				return CompletionConstants.NO_APPEND_SEPARATOR;
			}
			return CompletionConstants.NO_APPEND_SEPARATOR;
		}
		return -1;
	}
	
	/**
	 * Adds the valid items from the completionList to the candidates.  They are added to the candidates if they start
	 * with 'lastArg'
	 * @param candidates the candidates
	 * @param completionList possibilities before filtering based on last arg
	 * @param lastArg the commandline arg
	 */
	private void updateCandidates(List<CharSequence> candidates, List<String> completionList, String lastArg) {
		for (String item : completionList) {
			if (item.toUpperCase().startsWith(lastArg.toUpperCase())) {
				candidates.add(item);
			}
		}
	}
	
}
