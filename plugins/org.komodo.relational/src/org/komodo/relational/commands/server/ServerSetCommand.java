/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.WorkspaceCommandMessages.CreateSchemaCommand.CREATE_SCHEMA_ERROR;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerSetCommand.MissingServerNameArg;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerSetCommand.ServerDoesNotExist;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerSetCommand.ServerSetSuccess;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;

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
            String serverName = requiredArgument( 0, getMessage( MissingServerNameArg ) );

            // Validate that server object with this name exists in the workspace
            Teiid wsTeiid = ServerUtils.getWorkspaceTeiidObject( getWorkspaceManager(), getWorkspaceStatus(), serverName );
            if ( wsTeiid == null ) {
                return new CommandResultImpl( false, getMessage( ServerDoesNotExist, serverName ), null );
            }

            // Check for current server
            if ( hasWorkspaceServer() ) {
                // Request set to current server, no need to reset
                String currentServerName = getWorkspaceServerName();
                if ( serverName.equals( currentServerName ) ) {
                    return new CommandResultImpl( getMessage( ServerSetSuccess, serverName ) );
                }

                // Has different server currently.  Disconnect it.
                if ( hasConnectedWorkspaceServer() ) {
                    getCommand( ServerDisconnectCommand.NAME ).execute();
                }
            }

            // Set the server on workspace status
            getWorkspaceStatus().setStateObject( ServerCommandProvider.SERVER_DEFAULT_KEY, wsTeiid );
            result = new CommandResultImpl( getMessage( ServerSetSuccess, serverName ) );
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( CREATE_SCHEMA_ERROR ), e );
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

}
