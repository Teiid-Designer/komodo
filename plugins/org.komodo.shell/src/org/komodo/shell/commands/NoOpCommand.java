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

import org.komodo.shell.api.AbstractShellCommand;
import org.komodo.shell.api.Arguments;

/**
 * The command about nothing.  The Seinfeld command.
 * 
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - no changes currently
 * 
 * @author eric.wittmann@redhat.com
 */
public class NoOpCommand extends AbstractShellCommand {

	/**
	 * Constructor.
	 * @param name the command name
	 */
	public NoOpCommand(String name) {
		setName(name);
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
	 */
	@Override
	public void printUsage(int indent) {
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
	public boolean execute() {
        return true;
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.ShellCommand#recordCommand(org.komodo.shell.api.Arguments)
	 */
	@Override
	public void recordCommand(Arguments args) {
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.ShellCommand#recordComment(java.lang.String)
	 */
	@Override
	public void recordComment(String comment) {
	}

}
