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

import java.io.File;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.StringUtils;

/**
 * Enable or disable recording of commands to a file
 *
 */
public class RecordCommand extends BuiltInShellCommand {

    private static final String RECORD = "record"; //$NON-NLS-1$
    
	private static final String ON = "on"; //$NON-NLS-1$
	private static final String OFF = "off"; //$NON-NLS-1$
    private static final List<String> SUBCMDS = 
    		Arrays.asList(ON, OFF);    
	
	/**
	 * Constructor
	 * @param wsStatus the workspace status
	 */
	public RecordCommand(WorkspaceStatus wsStatus) {
		super(RECORD,wsStatus);
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
		if(onOffArg.equalsIgnoreCase(ON)) { 
			wsStatus.setRecordingStatus(true);
		} else if(onOffArg.equalsIgnoreCase(OFF)) { 
			wsStatus.setRecordingStatus(false);
		}
		
		Date d = new Date();
		String rState = wsStatus.getRecordingStatus() ? ON : OFF; 
		String rFile = wsStatus.getRecordingOutputFile().getCanonicalPath();
		String stateChangedMsg = Messages.getString("RecordCommand.setRecordingStateMsg",rState,d.toString(),rFile); //$NON-NLS-1$
		
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
		
		// Check for empty arg
		if(StringUtils.isEmpty(onOffArg)) {
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RecordCommand.onOffArg_empty")); //$NON-NLS-1$
			return false;
		}
		
		// Check for invalid arg
		if(!onOffArg.equalsIgnoreCase(ON) && !onOffArg.equalsIgnoreCase(OFF)) { 
            print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RecordCommand.onOffArg_invalid")); //$NON-NLS-1$
			return false;
		}
		
		// If verify that global file var was set.
		String recordingFileStr = getWorkspaceStatus().getProperties().getProperty(WorkspaceStatus.RECORDING_FILE_KEY);
		if(StringUtils.isEmpty(recordingFileStr)) {
			print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RecordCommand.recordingFileNotSet")); //$NON-NLS-1$
			return false;
		} else {
			File recordingFile = getWorkspaceStatus().getRecordingOutputFile();
			if(recordingFile!=null && recordingFile.exists()) {
				if(!recordingFile.canWrite()) {
					print(CompletionConstants.MESSAGE_INDENT,Messages.getString("RecordCommand.recordingFileNotWriteable",recordingFile)); //$NON-NLS-1$
					return false;
				}
			}
		}
		
		return true;
	}
	
	/**
	 * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
	 */
	@Override
	public int tabCompletion(String lastArgument, List<CharSequence> candidates) {
		if (getArguments().isEmpty()) {
			// --------------------------------------------------------------
			// No arg - offer subcommands
			// --------------------------------------------------------------
			if(lastArgument==null) {
				candidates.addAll(SUBCMDS);
				// --------------------------------------------------------------
				// One arg - determine the completion options for it.
				// --------------------------------------------------------------
			} else {
				for (String item : SUBCMDS) {
					if (item.toUpperCase().startsWith(lastArgument.toUpperCase())) {
						candidates.add(item);
					}
				}
			}
			return 0;
		} 
		return -1;
	}

}
