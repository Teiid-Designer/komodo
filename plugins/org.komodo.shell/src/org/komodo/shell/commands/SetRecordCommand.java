/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.Messages;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.StringUtils;

/**
 * SetRecordCommand - enable or disable command recording
 */
public class SetRecordCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "set-record"; //$NON-NLS-1$

    private static final String ON = "on"; //$NON-NLS-1$
    private static final String OFF = "off"; //$NON-NLS-1$
    private static final List<String> RECORD_CMDS = Arrays.asList(ON, OFF);

    /**
     * @param status
     *        the workspace status (cannot be <code>null</code>)
     */
    public SetRecordCommand( final WorkspaceStatus status ) {
        super( status, NAME );
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
            String onOffArg = requiredArgument(0, Messages.getString(Messages.SetRecordCommand.onOffArg_empty));

            // Check for invalid arg
            if(!onOffArg.equalsIgnoreCase(ON) && !onOffArg.equalsIgnoreCase(OFF)) {
                return new CommandResultImpl( false, Messages.getString(Messages.SetRecordCommand.onOffArg_invalid), null );
            }

            // Set WorkspaceStatus
            WorkspaceStatus wsStatus = getWorkspaceStatus();
            String recordingFileStr = getWorkspaceStatus().getProperties().getProperty(WorkspaceStatus.RECORDING_FILE_KEY);
            if(onOffArg.equalsIgnoreCase(ON)) {
                wsStatus.setRecordingStatus(true);

                // Output message if output file not defined or writer not available.
                if(StringUtils.isEmpty(recordingFileStr)) {
                    return new CommandResultImpl(Messages.getString(Messages.SetRecordCommand.recordingFileNotSet));
                }

                Writer recordingWriter = getWorkspaceStatus().getRecordingWriter();
                if(recordingWriter==null) {
                    return new CommandResultImpl( Messages.getString( Messages.SetRecordCommand.recordingFileProblem,
                                                                      recordingFileStr ) );
                }
            } else if(onOffArg.equalsIgnoreCase(OFF)) {
                wsStatus.setRecordingStatus(false);
            }

            String stateChangedMsg = Messages.getString(Messages.SetRecordCommand.setRecordingStateMsg,onOffArg,(new Date()).toString(),recordingFileStr);
            outputToRecordingFile("#  ----------\n#  "+stateChangedMsg+"\n#  ----------"); //$NON-NLS-1$ //$NON-NLS-2$
            return new CommandResultImpl(stateChangedMsg);
        } catch ( final Exception e ) {
            return new CommandResultImpl( false, Messages.getString( SHELL.CommandFailure, NAME ), e );
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
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public int tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        if ( getArguments().size() == 0 ) {
            if(lastArgument==null) {
                candidates.addAll(RECORD_CMDS);
            } else {
                for (String cmdName : RECORD_CMDS) {
                    if (cmdName.startsWith(lastArgument.toLowerCase())) {
                        candidates.add(cmdName);
                    }
                }
            }
            return ( candidates.isEmpty() ? -1 : ( toString().length() + 1 ) );
        }

        return -1;
    }

    /**
     * Write the supplied message to the recording output file.
     * @param message the line to output
     */
    private void outputToRecordingFile(String message) {
        WorkspaceStatus wsStatus = getWorkspaceStatus();
        Writer recordingWriter = wsStatus.getRecordingWriter();
        if(recordingWriter!=null) {
            try {
                recordingWriter.write(message+StringConstants.NEW_LINE);
                recordingWriter.flush();
            } catch (IOException ex) {
                String filePath = wsStatus.getProperties().getProperty(WorkspaceStatus.RECORDING_FILE_KEY);
                print(MESSAGE_INDENT, Messages.getString(SHELL.RecordingFileOutputError,filePath));
            }
        // Print error message if the recording file was not defined
        } else {
            print(MESSAGE_INDENT,Messages.getString(SHELL.RecordingFileNotDefined));
        }
    }

}
