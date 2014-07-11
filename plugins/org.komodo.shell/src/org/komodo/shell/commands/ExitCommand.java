/*
 * Copyright 2012 JBoss Inc
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

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * Implements the 'exit' command.
 * 
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use different Message class
 * 
 * @author eric.wittmann@redhat.com
 */
public class ExitCommand extends BuiltInShellCommand {

	
	/**
	 * Constructor.
	 * @param name the command name
	 * @param wsStatus the workspace status
	 */
	public ExitCommand(String name, WorkspaceStatus wsStatus) {
		super(name,wsStatus);
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
	 */
	@Override
	public void printUsage(int indent) {
		print(indent,"exit"); //$NON-NLS-1$
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
	 */
	@Override
	public void printHelp(int indent) {
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
		print(CompletionConstants.MESSAGE_INDENT,Messages.getString(SHELL.GOOD_BYE)); 
		System.exit(0);
        return true;
	}

}
