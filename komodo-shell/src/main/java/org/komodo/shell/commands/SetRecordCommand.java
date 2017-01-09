/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
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
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} that enables or disables command recording.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * set-record &lt;on | off&gt;
 * </code>
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
            String onOffArg = requiredArgument(0, I18n.bind(ShellI18n.onOffArgEmpty));

            // Check for invalid arg
            if(!onOffArg.equalsIgnoreCase(ON) && !onOffArg.equalsIgnoreCase(OFF)) {
                return new CommandResultImpl( false, I18n.bind(ShellI18n.onOffArgInvalid), null );
            }

            // Set WorkspaceStatus
            WorkspaceStatus wsStatus = getWorkspaceStatus();
            String recordingFileStr = getWorkspaceStatus().getGlobalProperties(false).getProperty(WorkspaceStatus.RECORDING_FILE_KEY);
            if(onOffArg.equalsIgnoreCase(ON)) {
                wsStatus.setRecordingStatus(true);

                // Output message if output file not defined or writer not available.
                if(StringUtils.isEmpty(recordingFileStr)) {
                    return new CommandResultImpl(I18n.bind(ShellI18n.recordingFileNotSet));
                }

                Writer recordingWriter = getWorkspaceStatus().getRecordingWriter();
                if(recordingWriter==null) {
                    return new CommandResultImpl( I18n.bind( ShellI18n.recordingFileProblem, recordingFileStr ) );
                }
            } else if(onOffArg.equalsIgnoreCase(OFF)) {
                wsStatus.setRecordingStatus(false);
            }

            String stateChangedMsg = I18n.bind(ShellI18n.setRecordingStateMsg,onOffArg,(new Date()).toString(),recordingFileStr);
            outputToRecordingFile("#  ----------\n#  "+stateChangedMsg+"\n#  ----------"); //$NON-NLS-1$ //$NON-NLS-2$
            return new CommandResultImpl(stateChangedMsg);
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
        print( indent, I18n.bind( ShellI18n.setRecordHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.setRecordExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.setRecordUsage ) );
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
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
        }

        return TabCompletionModifier.AUTO;
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
                String filePath = wsStatus.getGlobalProperties(false).getProperty(WorkspaceStatus.RECORDING_FILE_KEY);
                print(MESSAGE_INDENT, I18n.bind(ShellI18n.recordingFileOutputError, filePath));
            }
        // Print error message if the recording file was not defined
        } else {
            print(MESSAGE_INDENT,I18n.bind(ShellI18n.recordingFileNotDefined));
        }
    }

}
