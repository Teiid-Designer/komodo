package org.komodo.shell.commands;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.ShellCommandReader;
import org.komodo.shell.ShellCommandReaderFactory;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} that executes (plays) a saved komodo CLI script on your local file system. It executes all
 * commands as one transaction.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * play &lt;file-name&gt;
 * </code>
 */
public class PlayCommand  extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "play"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public PlayCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
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
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            String fileNameArg = requiredArgument( 0, I18n.bind( ShellI18n.invalidArgMsgFileName ) );

            // Validate the supplied path
            String validationResult = validateReadableFileArg( fileNameArg );
            if ( !CompletionConstants.OK.equals( validationResult ) ) {
                return new CommandResultImpl( false,
                                              I18n.bind( ShellI18n.fileNotAccessible, fileNameArg, validationResult ),
                                              null );
            }

            final WorkspaceStatus wsStatus = getWorkspaceStatus();
            boolean saveAutoCommit = wsStatus.isAutoCommit(); // save current value

            try {
                // turn auto-commit off for batch
                if ( saveAutoCommit ) {
                    wsStatus.setGlobalProperty( WorkspaceStatus.AUTO_COMMIT, Boolean.FALSE.toString() );
                }

                { // play file
                    String[] args = new String[] { "-f", fileNameArg }; //$NON-NLS-1$

                    ShellCommandReader reader = ShellCommandReaderFactory.createCommandReader( args, wsStatus );
                    reader.open();

                    while ( true ) {
                        final ShellCommand command = reader.read();

                        if ( command == null ) {
                            break;
                        }

                        command.setWriter( getWriter() );

                        if ( command instanceof ExitCommand ) {
                            final ExitCommand exitCmd = ( ExitCommand )command;

                            // make sure max number of arguments has not been exceeded
                            if ( exitCmd.getArguments().size() > exitCmd.getMaxArgCount() ) {
                                return new CommandResultImpl( false,
                                                              I18n.bind( ShellI18n.tooManyArgs, exitCmd ),
                                                              null );
                            }
                        }

                        final CommandResult result = command.execute();

                        if ( !result.isOk() ) {
                            return result;
                        }

                        // see if auto-commit global property was changed
                        if ( ( command instanceof SetGlobalPropertyCommand ) || ( command instanceof SetAutoCommitCommand ) ) {
                            if ( saveAutoCommit != wsStatus.isAutoCommit() ) {
                                saveAutoCommit = wsStatus.isAutoCommit();
                            }
                        }
                    }
                }

                return new CommandResultImpl( I18n.bind( ShellI18n.fileExecuted, fileNameArg ) );
            } catch ( Exception e ) {
                return new CommandResultImpl( false, I18n.bind( ShellI18n.playFailure, fileNameArg ), e );
            } finally {
                // if AUTO_COMMIT if different (script could've changed value also)
                if ( getWorkspaceStatus().isAutoCommit() != saveAutoCommit ) {
                    getWorkspaceStatus().setGlobalProperty( WorkspaceStatus.AUTO_COMMIT,
                                                            Boolean.valueOf( saveAutoCommit ).toString() );
                }
            }
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
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ShellI18n.playHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.playExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.playUsage ) );
    }

}
