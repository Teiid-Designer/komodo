/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import java.util.ArrayList;
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command to set the default server name
 */
public final class ServerSetCommand extends ServerShellCommand {

    static final String NAME = "set-server"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerSetCommand( final WorkspaceStatus status ) {
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
            String serverName = requiredArgument( 0, I18n.bind( ServerCommandsI18n.missingServerNameArg ) );

            // Validate that server object with this name exists in the workspace
            Teiid wsTeiid = ServerUtils.getWorkspaceTeiidObject( getWorkspaceManager(), getWorkspaceStatus(), serverName );
            if ( wsTeiid == null ) {
                return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.serverDoesNotExist, serverName ), null );
            }

            // Check for current server
            if ( hasWorkspaceServer() ) {
                // Request set to current server, no need to reset
                String currentServerName = getWorkspaceServerName();
                if ( serverName.equals( currentServerName ) ) {
                    return new CommandResultImpl( I18n.bind( ServerCommandsI18n.serverSetSuccess, serverName ) );
                }

                // Has different server currently.  Disconnect it.
                if ( hasConnectedWorkspaceServer() ) {
                    getCommand( ServerDisconnectCommand.NAME ).execute();
                }
            }

            // Set the server on workspace status
            getWorkspaceStatus().setStateObject( ServerCommandProvider.SERVER_DEFAULT_KEY, wsTeiid );

            // Updates available commands upon setting server
            getWorkspaceStatus().updateAvailableCommands();

            result = new CommandResultImpl( I18n.bind( ServerCommandsI18n.serverSetSuccess, serverName ) );
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
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverSetHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverSetExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverSetUsage ) );
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

        final UnitOfWork uow = getTransaction();
        final WorkspaceManager mgr = getWorkspaceManager();
        final KomodoObject[] teiids = mgr.findTeiids(getTransaction());
        List<String> existingTeiidNames = new ArrayList<String>(teiids.length);
        for(KomodoObject teiid : teiids) {
            existingTeiidNames.add(teiid.getName(uow));
        }

        if ( args.isEmpty() ) {
            if ( lastArgument == null ) {
                candidates.addAll( existingTeiidNames );
            } else {
                for ( final String item : existingTeiidNames ) {
                    if ( item.toUpperCase().startsWith( lastArgument.toUpperCase() ) ) {
                        candidates.add( item );
                    }
                }
            }
        }
        return TabCompletionModifier.AUTO;
    }

}
