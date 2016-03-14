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

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} that displays the available commands for the current context or provides information about a
 * specific command.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * help [command_name]
 * </code>
 */
public class HelpCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "help"; //$NON-NLS-1$

    private static final int CMDS_PER_LINE = 5;
    private static final int DEFAULT_COLUMN_WIDTH = 10;
    private static final String INDENT;

    static {
        final StringBuffer indentBuffer = new StringBuffer( MESSAGE_INDENT );

        for ( int i = 0; i < MESSAGE_INDENT; ++i ) {
            indentBuffer.append( ' ' );
        }

        INDENT = indentBuffer.toString();
    }

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
    		getWorkspaceStatus().updateAvailableCommands();
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

    private void printCommandNames( final String[] commandNames,
                                    final boolean categorized ) {
        int maxCommandLength = DEFAULT_COLUMN_WIDTH;

        // find max length of command names
        for ( final String cmdName : commandNames ) {
            if ( cmdName.length() > maxCommandLength ) {
                maxCommandLength = cmdName.length();
            }
        }

        int colCount = 0;
        final StringBuilder builder = new StringBuilder();

        for ( final String cmdName : commandNames ) {
            if ( categorized ) {
                builder.append( INDENT );
            }

            builder.append( String.format( "%-" + ( maxCommandLength + 5 ) + "s", cmdName ) ); //$NON-NLS-1$ //$NON-NLS-2$
            ++colCount;

            if ( colCount == CMDS_PER_LINE ) {
                builder.append( '\n' ).append( INDENT );

                if ( categorized ) {
                    builder.append( INDENT );
                }

                colCount = 0;
            }
        }

        final int indentSize = ( MESSAGE_INDENT * ( categorized ? 2 : 1 ) );
        print( indentSize, builder.toString() );

        if ( colCount != 0 ) {
            print();
        }
    }

	/**
	 * Prints the generic help - all commands for this workspace context
	 */
	private void printHelpAll() throws Exception {
        print( MESSAGE_INDENT, I18n.bind( ShellI18n.helpCommandListMsg ) + NEW_LINE );
        final String[] validCmdNames = getWorkspaceStatus().getAvailableCommandNames();

        if ( getWorkspaceStatus().isShowingCommandCategory() ) {
            final WorkspaceStatus status = getWorkspaceStatus();
            final Map< String, List< String > > categoryCommands = new TreeMap< >();

            // group commands by category
            for ( final String cmdName : validCmdNames ) {
                final String category = status.getCommand( cmdName ).getCategory();
                List< String > commands = categoryCommands.get( category );

                if ( commands == null ) {
                    commands = new ArrayList< String >();
                    categoryCommands.put( category, commands );
                }

                commands.add( cmdName );
            }

            // print command by category
            for ( final Entry< String, List< String > > entry : categoryCommands.entrySet() ) {
                if ( categoryCommands.size() > 1 ) {
                    print( ( MESSAGE_INDENT * 2 ), I18n.bind( ShellI18n.helpCategoryHeader, entry.getKey() ) );
                }

                Collections.sort( entry.getValue() );
                printCommandNames( entry.getValue().toArray( new String[ entry.getValue().size() ] ), true );
            }
        } else {
            printCommandNames( validCmdNames, false );
        }

        print( MESSAGE_INDENT,I18n.bind(ShellI18n.helpGetHelp1));
		print( MESSAGE_INDENT,I18n.bind(ShellI18n.helpGetHelp2) + NEW_LINE );
	}

    private void printHelpForCommand( final String cmdName ) throws Exception {
        final ShellCommand command = getWorkspaceStatus().getCommand( cmdName );

        if (command == null) {
            print( CompletionConstants.MESSAGE_INDENT, I18n.bind( ShellI18n.helpInvalidCommand, cmdName ) );
        } else {
            command.setWriter( getWriter() );
            command.printHelp( CompletionConstants.MESSAGE_INDENT );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ShellI18n.helpHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.helpExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.helpUsage ) );
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
    public TabCompletionModifier tabCompletion( String lastArgument,
                              List< CharSequence > candidates ) {
        if ( getArguments().isEmpty() ) {
            try {
                for ( String candidate : getWorkspaceStatus().getAvailableCommandNames() ) {
                    if ( lastArgument == null || candidate.startsWith( lastArgument ) ) {
                        candidates.add( candidate );
                    }
                }
            } catch ( final Exception e ) {
                throw new RuntimeException( e );
            }
        }

        return TabCompletionModifier.AUTO;
    }

}
