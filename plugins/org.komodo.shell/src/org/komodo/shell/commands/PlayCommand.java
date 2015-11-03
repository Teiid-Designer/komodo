package org.komodo.shell.commands;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.ShellCommandReader;
import org.komodo.shell.ShellCommandReaderFactory;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * This command executes (plays) a saved komodo CLI script on your local file system. It executes all commands as one transaction.
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
            String fileNameArg = requiredArgument( 0, Messages.getString( Messages.PlayCommand.InvalidArgMsg_FileName ) );

            // Validate the supplied path
            String validationResult = validateReadableFileArg( fileNameArg );
            if ( !CompletionConstants.OK.equals( validationResult ) ) {
                return new CommandResultImpl( false,
                                              Messages.getString( Messages.SHELL.FileNotAccessible,
                                                                  fileNameArg,
                                                                  validationResult ),
                                              null );
            }

            final WorkspaceStatus wsStatus = getWorkspaceStatus();
            boolean saveAutoCommit = wsStatus.isAutoCommit(); // save current value

            try {
                // turn auto-commit off for batch
                if ( saveAutoCommit ) {
                    wsStatus.setProperty( WorkspaceStatus.AUTO_COMMIT, Boolean.FALSE.toString() );
                }

                { // play file
                    String[] args = new String[] { "-f", fileNameArg }; //$NON-NLS-1$

                    ShellCommandReader reader = ShellCommandReaderFactory.createCommandReader( args, wsStatus );
                    reader.open();

                    while ( true ) {
                        final ShellCommand command = reader.read();

                        if ( ( command == null ) || ( command instanceof ExitCommand ) ) {
                            break;
                        }

                        command.setWriter( getWriter() );
                        final CommandResult result = command.execute();

                        if ( !result.isOk() ) {
                            return result;
                        }

                        // check to see if auto-commit command was used in batch
                        if ( command instanceof SetGlobalPropertyCommand ) {
                            if ( WorkspaceStatus.AUTO_COMMIT.equals( command.getArguments().get( 0 ) ) ) {
                                final boolean newValue = Boolean.parseBoolean( command.getArguments().get( 1 ) );

                                if ( saveAutoCommit != newValue ) {
                                    saveAutoCommit = newValue;
                                }
                            }
                        }
                    }
                }

                return new CommandResultImpl( Messages.getString( Messages.PlayCommand.fileExecuted, fileNameArg ) );
            } catch ( Exception e ) {
                return new CommandResultImpl( false, Messages.getString( Messages.PlayCommand.Failure, fileNameArg ), e );
            } finally {
                // if AUTO_COMMIT if different (script could've changed value also)
                if ( getWorkspaceStatus().isAutoCommit() != saveAutoCommit ) {
                    getWorkspaceStatus().setProperty( WorkspaceStatus.AUTO_COMMIT,
                                                      Boolean.toString( getWorkspaceStatus().isAutoCommit() ) );
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

}
