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
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;

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
     * The command name.
     */
    public static final String NAME = "NoOp"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public NoOpCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#getAliases()
     */
    @Override
    public final String[] getAliases() {
        return StringConstants.EMPTY_ARRAY;
    }

    /**
     * @see org.komodo.shell.api.ShellCommand#getName()
     */
    @Override
    public final String getName() {
        return NAME;
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
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() {
        return true;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see org.komodo.shell.api.ShellCommand#record()
	 */
	@Override
	public void record() {
	    // Nothing to do
	}

	/* (non-Javadoc)
	 * @see org.komodo.shell.api.ShellCommand#recordComment(java.lang.String)
	 */
	@Override
	public void recordComment(String comment) {
	    // Nothing to do
	}

}
