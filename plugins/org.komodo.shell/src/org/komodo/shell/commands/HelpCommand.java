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

import java.util.ArrayList;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
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

    /**
     * The command name.
     */
    public static final String NAME = "help"; //$NON-NLS-1$

    private static final int CMDS_PER_LINE = 4;
    private static final int DEFAULT_COLUMN_WIDTH = 10;

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public HelpCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME, "man" ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
    		String commandName = optionalArgument(0);
    		if (commandName == null) {
    			printHelpAll();
    		} else {
    			printHelpForCommand(commandName);
    		}

    		return CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            return new CommandResultImpl( e );
        }
	}

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 1;
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

	/**
	 * Prints the generic help - all commands for this workspace context
	 */
	private void printHelpAll() throws Exception {
		print(CompletionConstants.MESSAGE_INDENT,Messages.getString(SHELL.Help_COMMAND_LIST_MSG));

		StringBuffer indentBuffer = new StringBuffer();
		for(int i=0; i<CompletionConstants.MESSAGE_INDENT; i++) {
			indentBuffer.append(StringConstants.SPACE);
		}

		// Assemble the valid command names and find the max command character length
		int maxCommandLength = DEFAULT_COLUMN_WIDTH;
        List<String> validCmdNames = new ArrayList<String>();
        for (final String cmdName : getWorkspaceStatus().getAvailableCommands()) {
            if(cmdName.length()>maxCommandLength) {
                maxCommandLength = cmdName.length();
            }
            validCmdNames.add(cmdName);
        }

        // Print appropriate commands per line
		int colCount = 0;
		StringBuilder builder = new StringBuilder();
		for (String cmdName : validCmdNames) {
            builder.append(String.format("%-"+(maxCommandLength+5)+"s", cmdName)); //$NON-NLS-1$ //$NON-NLS-2$
            colCount++;

            if (colCount == CMDS_PER_LINE) {
                builder.append("\n"+indentBuffer.toString()); //$NON-NLS-1$
                colCount = 0;
            }
		}

		print(CompletionConstants.MESSAGE_INDENT,builder.toString());
		if(colCount!=0) print(CompletionConstants.MESSAGE_INDENT,"\n"); //$NON-NLS-1$
		print(CompletionConstants.MESSAGE_INDENT,Messages.getString(SHELL.Help_GET_HELP_1));
		print(CompletionConstants.MESSAGE_INDENT,""); //$NON-NLS-1$
		print(CompletionConstants.MESSAGE_INDENT,Messages.getString(SHELL.Help_GET_HELP_2));
	}

    private void printHelpForCommand( final String cmdName ) throws Exception {
        final ShellCommand command = getWorkspaceStatus().getCommand( cmdName );

        if (command == null) {
            print( CompletionConstants.MESSAGE_INDENT, Messages.getString( SHELL.Help_INVALID_COMMAND, cmdName ) );
        } else {
            command.setWriter( getWorkspaceStatus().getShell().getOutputWriter() );
            command.printHelp( CompletionConstants.MESSAGE_INDENT );
        }
    }

	/**
	 * Tab completion.
	 *
	 * @param lastArgument the last argument
	 * @param candidates the candidates
	 * @return the int
	 * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String,
	 *      java.util.List)
	 */
	@Override
    public int tabCompletion( String lastArgument,
                              List< CharSequence > candidates ) {
        if ( getArguments().isEmpty() ) {
            try {
                for ( String candidate : getWorkspaceStatus().getAvailableCommands() ) {
                    if ( lastArgument == null || candidate.startsWith( lastArgument ) ) {
                        candidates.add( candidate );
                    }
                }
            } catch ( final Exception e ) {
                throw new RuntimeException( e );
            }

            return 0;
        }

        return -1;
    }

}
