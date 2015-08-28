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
import org.komodo.shell.api.WorkspaceStatus;

/**
 * NoOp Command
 */
public class NoOpCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "NoOp"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public NoOpCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

	/**
	 * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
	 */
	@Override
	public void printUsage(int indent) {
	    // Nothing to do
	}

	/**
	 * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
	 */
	@Override
	public void printHelp(int indent) {
	    // Nothing to do
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

    /* (non-Javadoc)
     * @see org.komodo.shell.BuiltInShellCommand#shouldCommit()
     */
    @Override
    protected boolean shouldCommit() {
        return false;
    }

    /* (non-Javadoc)
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        return true;
    }

}
