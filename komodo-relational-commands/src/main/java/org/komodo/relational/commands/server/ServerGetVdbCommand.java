/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.shell.CompletionConstants.MESSAGE_INDENT;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import org.komodo.relational.commands.workspace.UploadVdbCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.utils.i18n.I18n;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * A shell command to get a server VDB and copy into the workspace
 */
public final class ServerGetVdbCommand extends ServerShellCommand {

    static final String NAME = "server-get-vdb"; //$NON-NLS-1$

    private static final String TEMPFILE_PREFIX = "Vdb-"; //$NON-NLS-1$
    private static final String TEMPFILE_SUFFIX = ".xml"; //$NON-NLS-1$

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

            // Make sure no VDB currently in workspace with this name
            if(getWorkspaceManager().hasChild(getTransaction(), vdbName, VdbLexicon.Vdb.VIRTUAL_DATABASE)) {
                return new CommandResultImpl( false, I18n.bind(ServerCommandsI18n.repoVdbWithNameExists, vdbName), null );
            }
            
            // Validates that a server is connected
            CommandResult validationResult = validateHasConnectedWorkspaceServer();
            if ( !validationResult.isOk() ) {
                return validationResult;
            }

            // Get the VDB - make sure its a dynamic VDB
            TeiidVdb vdb = null;
            try {
                // Check the vdb name to make sure its valid
                List< String > existingVdbNames = ServerUtils.getVdbNames(getWorkspaceTeiidInstance());
                if(!existingVdbNames.contains(vdbName)) {
                    return new CommandResultImpl(false, I18n.bind( ServerCommandsI18n.serverVdbNotFound, vdbName ), null);
                }
                // Get the vdb
                vdb = getWorkspaceTeiidInstance().getVdb(vdbName);
            } catch (Exception ex) {
                result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.connectionErrorWillDisconnect ), ex );
                ServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer();
                return result;
            }
            if(vdb == null) {
                return new CommandResultImpl( false, I18n.bind(ServerCommandsI18n.serverVdbNotFound, vdbName), null );
            }
            if(!vdb.isXmlDeployment()) {
                return new CommandResultImpl( false, I18n.bind(ServerCommandsI18n.canOnlyCopyDynamicVDBs, vdbName), null );
            }

            // Export the vdb content into a string
            String vdbStr = vdb.export();

            // Output the content to a temp file
            File tempFile = File.createTempFile(TEMPFILE_PREFIX, TEMPFILE_SUFFIX);
            Files.write(Paths.get(tempFile.getPath()), vdbStr.getBytes());

            // Upload the VdbFile
            UploadVdbCommand uploadVdbCommand = new UploadVdbCommand(getWorkspaceStatus());
            uploadVdbCommand.setArguments(new Arguments( vdbName + StringConstants.SPACE + tempFile.getAbsolutePath() ));
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
        return 1;
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
            ServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer();
        }

        return TabCompletionModifier.AUTO;
    }

}
