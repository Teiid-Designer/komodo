package org.komodo.shell.commands.core;

import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.Messages;
import org.komodo.shell.ShellCommandFactory;
import org.komodo.shell.ShellCommandReader;
import org.komodo.shell.ShellCommandReaderFactory;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.ExitCommand;

/**
 * This command is intended to allow executing (playing) a saved komodo CLI script on your local file system
 *
 * @author blafond
 *
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
    protected boolean doExecute() throws Exception {
        String fileNameArg = requiredArgument(0, Messages.getString(Messages.PlayCommand.InvalidArgMsg_FileName));

        // Validate the supplied path
        boolean validFile = validateReadableFileArg(fileNameArg);
        if(!validFile) return false;
        
        try {
        	playFile(fileNameArg);
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.PlayCommand.fileExecuted, fileNameArg));
        } catch (Exception e) {
            print(CompletionConstants.MESSAGE_INDENT, Messages.getString(Messages.PlayCommand.Failure, fileNameArg));
            print(CompletionConstants.MESSAGE_INDENT, TAB + e.getMessage());
            return false;
        }
        return true;
	}

	private void playFile(String commandFile ) throws Exception {
		WorkspaceStatus wsStatus = getWorkspaceStatus();
		String[] args = new String[]{"-f", commandFile}; //$NON-NLS-1$

		ShellCommandFactory factory = new ShellCommandFactory();
        factory.registerCommands(wsStatus);
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

            command.setWriter( getWriter() );

			command.execute();
		}
	}

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#shouldCommit()
     */
    @Override
    protected boolean shouldCommit() {
        return false;
    }


}
