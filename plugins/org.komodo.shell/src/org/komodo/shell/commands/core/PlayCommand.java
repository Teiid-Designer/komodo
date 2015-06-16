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
import org.komodo.spi.constants.StringConstants;

/**
 * This command is intended to allow executing (playing) a saved komodo CLI script on your local file system
 *
 * @author blafond
 *
 */
public class PlayCommand  extends BuiltInShellCommand implements StringConstants {

    private static final String PLAY = "play"; //$NON-NLS-1$

    /**
	 * Constructor.
	 * @param wsStatus the workspace status
	 */
	public PlayCommand(WorkspaceStatus wsStatus) {
		super(PLAY,wsStatus);
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


		//String commandFilePath = UnitTestUtil.getTestDataPath() + "../resources" + commandFile;
		String[] args = new String[]{"-f", commandFile}; //$NON-NLS-1$

		ShellCommandFactory factory = new ShellCommandFactory(wsStatus);
		ShellCommandReader reader = ShellCommandReaderFactory.createCommandReader(args, factory, wsStatus);
        reader.open();

        runCommands(reader);
	}

	private void runCommands(ShellCommandReader reader) throws Exception {
		boolean done = false;
		while (!done) {
			ShellCommand command = reader.read();

			if(command==null || command instanceof ExitCommand) break;

			boolean success = command.execute();

			if (!success && reader.isBatch()) {
				throw new Exception(Messages.getString("PlayCommand.CommandFailure", command.getName())); //$NON-NLS-1$
			}

		}
	}

}
