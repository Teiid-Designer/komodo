/*
 * Copyright 2014 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.shell.commands;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeSet;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;

/**
 * Implements the 'help' command.
 * 
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered generated help messages
 * - altered to use different Messages class
 * 
 * @author eric.wittmann@redhat.com
 */
public class HelpCommand extends BuiltInShellCommand {

	private static final int CMDS_PER_LINE = 4;
	
	private final Map<String, ShellCommand> commands;

	/**
	 * Constructor.
	 * @param name the command name
	 * @param wsStatus the workspace status
	 * @param commands the commands
	 */
	public HelpCommand(String name, WorkspaceStatus wsStatus, Map<String, ShellCommand> commands) {
		super(name,wsStatus);
		this.commands = commands;
	}

	/**
	 * Prints the usage.
	 *
	 * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
	 */
	@Override
	public void printUsage(int indent) {
	    // Nothing to do
	}

	/**
	 * Prints the help.
	 *
	 * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
	 */
	@Override
	public void printHelp(int indent) {
	    // Nothing to do
	}

	/**
	 * Execute.
	 *
	 * @return true, if successful
	 * @throws Exception the exception
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
		String commandName = optionalArgument(0);
		if (commandName == null) {
			printHelpAll();
		} else {
			printHelpForCommand(commandName);
		}
		return true;
	}

	/**
	 * Prints the generic help - all commands for this workspace context
	 */
	private void printHelpAll() throws Exception {
		print(CompletionConstants.MESSAGE_INDENT,Messages.getString(SHELL.Help_COMMAND_LIST_MSG)); 
		
		// Determine the current Workspace Context type
		WorkspaceStatus wsStatus = getWorkspaceStatus();
		WorkspaceContext currentContext = wsStatus.getCurrentContext();
		String currentContextType = currentContext.getType();

		StringBuffer indentBuffer = new StringBuffer();
		for(int i=0; i<CompletionConstants.MESSAGE_INDENT; i++) {
			indentBuffer.append(StringConstants.SPACE);
		}
		
		int colCount = 0;
		StringBuilder builder = new StringBuilder();
		for (Entry<String,ShellCommand> entry : this.commands.entrySet()) {
			String cmdName = entry.getKey();
			ShellCommand command = entry.getValue();
			if(command.isValidForWsContext(currentContextType)) {
				builder.append(String.format("%-18s", cmdName)); //$NON-NLS-1$
				colCount++;

				if (colCount == CMDS_PER_LINE) {
					builder.append("\n"+indentBuffer.toString()); //$NON-NLS-1$
					colCount = 0;
				}
			}
		}
		print(CompletionConstants.MESSAGE_INDENT,builder.toString());
		if(colCount!=0) print(CompletionConstants.MESSAGE_INDENT,"\n"); //$NON-NLS-1$
		print(CompletionConstants.MESSAGE_INDENT,Messages.getString(SHELL.Help_GET_HELP_1)); 
		print(CompletionConstants.MESSAGE_INDENT,""); //$NON-NLS-1$
		print(CompletionConstants.MESSAGE_INDENT,Messages.getString(SHELL.Help_GET_HELP_2)); 
		print(CompletionConstants.MESSAGE_INDENT,""); //$NON-NLS-1$
	}

	/**
	 * Prints the help for a single command.
	 *
	 * @param cmdName the cmd name
	 * @throws Exception the exception
	 */
	private void printHelpForCommand(String cmdName) throws Exception {
		ShellCommand command = this.commands.get(cmdName);
		if (command == null) {
			print(CompletionConstants.MESSAGE_INDENT,Messages.getString(SHELL.Help_INVALID_COMMAND)); 
		} else {
			command.printUsage(0);
			command.printHelp(0);
			print(CompletionConstants.MESSAGE_INDENT,""); //$NON-NLS-1$
		}
	}

	/**
	 * Tab completion.
	 *
	 * @param lastArgument the last argument
	 * @param candidates the candidates
	 * @return the int
	 * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String,
	 *      java.util.List)
	 */
	@Override
	public int tabCompletion(String lastArgument, List<CharSequence> candidates) {
		if (getArguments().isEmpty()) {
			for (String candidate : generateHelpCandidates()) {
				if (lastArgument == null || candidate.startsWith(lastArgument)) {
					candidates.add(candidate);
				}
			}

			return 0;
		} else {
			return -1;
		}
	}

	/**
	 * Generate help candidates.
	 *
	 * @return a collection of all possible command names
	 */
	private Collection<String> generateHelpCandidates() {
		TreeSet<String> candidates = new TreeSet<String>();
		for (String key : this.commands.keySet()) {
			String candidate = key; 
			candidates.add(candidate);
		}
		return candidates;
	}

}
