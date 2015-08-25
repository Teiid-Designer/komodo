/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell.commands.core;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.File;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.Messages;
import org.komodo.shell.api.InvalidCommandArgumentException;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.StringUtils;

/**
 * A command to enable and disable command recording
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
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        try {
            String onOffArg = requiredArgument(0, Messages.getString(Messages.SetRecordCommand.onOffArg_empty));

            if (!this.validateRecord(onOffArg)) {
                return false;
            }

            WorkspaceStatus wsStatus = getWorkspaceStatus();
            if(onOffArg.equalsIgnoreCase(ON)) {
                wsStatus.setRecordingStatus(true);
            } else if(onOffArg.equalsIgnoreCase(OFF)) {
                wsStatus.setRecordingStatus(false);
            }

            Date d = new Date();
            String rState = wsStatus.getRecordingStatus() ? ON : OFF;
            String rFile = wsStatus.getRecordingOutputFile().getCanonicalPath();
            String stateChangedMsg = Messages.getString(Messages.SetRecordCommand.setRecordingStateMsg,rState,d.toString(),rFile);

            print(MESSAGE_INDENT,stateChangedMsg);

            recordComment("====== "+stateChangedMsg+" ======"); //$NON-NLS-1$ //$NON-NLS-2$

            return true;
        } catch ( final InvalidCommandArgumentException e ) {
            throw e;
        } catch ( final Exception e ) {
            print( MESSAGE_INDENT, getString( "error", e.getLocalizedMessage() ) ); //$NON-NLS-1$
            return false;
        }
    }

    private String getString( final String msgKey,
                              final String... args ) {
        return Messages.getString( SetRecordCommand.class.getSimpleName() + '.' + msgKey, ( Object[] )args );
    }

    /**
     * Validate the SET RECORD args
     * @param onOffArg the on / off arg
     * @return 'true' if valid, 'false' if not.
     */
    protected boolean validateRecord(String onOffArg) {
        // Check for empty arg
        if(StringUtils.isEmpty(onOffArg)) {
            print(MESSAGE_INDENT,Messages.getString(Messages.SetRecordCommand.onOffArg_empty));
            return false;
        }

        // Check for invalid arg
        if(!onOffArg.equalsIgnoreCase(ON) && !onOffArg.equalsIgnoreCase(OFF)) {
            print(MESSAGE_INDENT,Messages.getString(Messages.SetRecordCommand.onOffArg_invalid));
            return false;
        }

        // If verify that global file var was set.
        String recordingFileStr = getWorkspaceStatus().getProperties().getProperty(WorkspaceStatus.RECORDING_FILE_KEY);
        if(StringUtils.isEmpty(recordingFileStr)) {
            print(MESSAGE_INDENT,Messages.getString(Messages.SetRecordCommand.recordingFileNotSet));
            return false;
        } else {
            File recordingFile = getWorkspaceStatus().getRecordingOutputFile();
            if(recordingFile!=null && recordingFile.exists()) {
                if(!recordingFile.canWrite()) {
                    print(MESSAGE_INDENT,Messages.getString(Messages.SetRecordCommand.recordingFileNotWriteable,recordingFile));
                    return false;
                }
            }
        }

        return true;
    }
    
    /**
     * @see org.komodo.shell.api.AbstractShellCommand#tabCompletion(java.lang.String, java.util.List)
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
        return true;
    }

}
