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
package org.komodo.relational.commands.server;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.komodo.relational.commands.workspace.UploadVdbCommand;
import org.komodo.relational.commands.workspace.WorkspaceCommandsI18n;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.StringUtils;
import org.komodo.utils.i18n.I18n;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * A shell command to get a server VDB and copy into the workspace
 */
public final class ServerGetVdbCommand extends ServerShellCommand {

    static final String NAME = "server-get-vdb"; //$NON-NLS-1$

    private static final String TEMPFILE_PREFIX = "Vdb-"; //$NON-NLS-1$
    private static final String TEMPFILE_SUFFIX = ".xml"; //$NON-NLS-1$

    private static final List< String > VALID_OVERWRITE_ARGS = Arrays.asList( new String[] { "-o", "--overwrite" } ); //$NON-NLS-1$ //$NON-NLS-2$;

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerGetVdbCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        CommandResult result = null;

        try {
            String vdbName = requiredArgument( 0, I18n.bind( ServerCommandsI18n.missingVdbName ) );

            final String overwriteArg = optionalArgument( 1, null );
            final boolean overwrite = !StringUtils.isBlank( overwriteArg );
            // make sure overwrite arg is valid
            if ( overwrite && !VALID_OVERWRITE_ARGS.contains( overwriteArg ) ) {
                return new CommandResultImpl( false, I18n.bind( WorkspaceCommandsI18n.overwriteArgInvalid, overwriteArg ), null );
            }

            // If VDB with same name is in workspace, make sure we can overwrite
            boolean hasVdb = getWorkspaceManager(getTransaction()).hasChild(getTransaction(), vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE);
            if ( hasVdb && !overwrite ) {
                return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.vdbOverwriteNotEnabled, vdbName ), null );
            }
            
            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Check the vdb name to make sure its valid
            List< String > existingVdbNames = ServerUtils.getVdbNames(getWorkspaceTeiidInstance());
            if(!existingVdbNames.contains(vdbName)) {
                return new CommandResultImpl(false, I18n.bind( ServerCommandsI18n.serverVdbNotFound, vdbName ), null);
            }
            
            // Get the VDB - only dynamic VDBs can be handled currently.  exception will be handled for archive vdb
            TeiidVdb vdb = null;
            try {
                // Get the vdb
                vdb = getWorkspaceTeiidInstance().getVdb(vdbName);
            } catch (Exception ex) {
                result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.serverGetVdbError, vdbName, ex.getLocalizedMessage() ), null );
                return result;
            }
            
            if(vdb == null) {
                return new CommandResultImpl( false, I18n.bind(ServerCommandsI18n.serverVdbNotFound, vdbName), null );
            }

            // Export the vdb content into a string
            String vdbStr = vdb.export();

            // Output the content to a temp file
            File tempFile = File.createTempFile(TEMPFILE_PREFIX, TEMPFILE_SUFFIX);
            Files.write(Paths.get(tempFile.getPath()), vdbStr.getBytes());

            // Upload the VdbFile
            UploadVdbCommand uploadVdbCommand = new UploadVdbCommand(getWorkspaceStatus());
            uploadVdbCommand.setArguments(new Arguments( vdbName + StringConstants.SPACE + tempFile.getAbsolutePath() + " -o"));  //$NON-NLS-1$
            CommandResult uploadResult = uploadVdbCommand.execute();
            if(!uploadResult.isOk()) {
                return uploadResult;
            }

            print( MESSAGE_INDENT, I18n.bind(ServerCommandsI18n.vdbCopyToRepoFinished) );
            result = CommandResult.SUCCESS;
        } catch ( final Exception e ) {
            result = new CommandResultImpl( e );
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 2;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        return (isWorkspaceContext() && hasConnectedWorkspaceServer());
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverGetVdbHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverGetVdbExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverGetVdbUsage ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion( final String lastArgument,
                              final List< CharSequence > candidates ) throws Exception {
        final Arguments args = getArguments();

        try {
            List<String> existingVdbNames = ServerUtils.getVdbNames(getWorkspaceTeiidInstance());
            Collections.sort(existingVdbNames);

            if ( args.isEmpty() ) {
                if ( lastArgument == null ) {
                    candidates.addAll( existingVdbNames );
                } else {
                    for ( final String item : existingVdbNames ) {
                        if ( item.startsWith( lastArgument ) ) {
                            candidates.add( item );
                        }
                    }
                }
            }
        } catch (Exception ex) {
            print( );
            print( MESSAGE_INDENT, I18n.bind(ServerCommandsI18n.connectionErrorWillDisconnect) );
            WkspStatusServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer();
        }

        return TabCompletionModifier.AUTO;
    }

}
