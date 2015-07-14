package org.komodo.shell.commands.core;

import java.io.Writer;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.ShellCommandFactory;
import org.komodo.shell.ShellCommandReader;
import org.komodo.shell.ShellCommandReaderFactory;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.ExitCommand;
import org.komodo.spi.constants.StringConstants;

/**
 * This command is intended to allow executing (playing) a saved komodo CLI script on your local file system
 *
 * @author blafond
 *
 */
public class PlayCommand  extends BuiltInShellCommand implements StringConstants {

    /**
     * The command name.
     */
    public static final String NAME = "play"; //$NON-NLS-1$

    private Writer commandWriter;

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public PlayCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

	/**
	 * @see org.komodo.shell.api.ShellCommand#execute()
	 */
	@Override
	public boolean execute() throws Exception {
        String fileNameArg = requiredArgument(0, Messages.getString("PlayCommand.InvalidArgMsg_FileName")); //$NON-NLS-1$

        try {
        	playFile(fileNameArg);
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("PlayCommand.fileExecuted", fileNameArg)); //$NON-NLS-1$
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString("PlayCommand.Failure", fileNameArg)); //$NON-NLS-1$
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
	}

	private void playFile(String commandFile ) throws Exception {
		WorkspaceStatus wsStatus = getWorkspaceStatus();
		String[] args = new String[]{"-f", commandFile}; //$NON-NLS-1$

		ShellCommandFactory factory = new ShellCommandFactory(wsStatus);
		ShellCommandReader reader = ShellCommandReaderFactory.createCommandReader(args, factory, wsStatus);
        reader.open();

        runCommands(reader);
	}

	private void runCommands(ShellCommandReader reader) throws Exception {
        while ( true ) {
			final ShellCommand command = reader.read();

            if ( ( command == null ) || ( command instanceof ExitCommand ) ) {
                break;
            }

            command.setOutput( this.commandWriter );

			final boolean success = command.execute();

            if ( !success && reader.isBatch() ) {
                throw new Exception( Messages.getString( "PlayCommand.CommandFailure", command ) ); //$NON-NLS-1$
            }
		}
	}

    /**
     * @param commandWriter
     *        the writer the commands being played should use (cannot be <code>null</code>)
     */
    public void setCommandOutput( final Writer commandWriter ) {
        this.commandWriter = commandWriter;
    }

}
