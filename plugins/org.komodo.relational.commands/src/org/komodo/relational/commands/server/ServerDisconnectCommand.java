/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.Common.NoTeiidDefined;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerConnectCommand.ConnectionError;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDisconnectCommand.AttemptingToDisconnect;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDisconnectCommand.DisconnectSuccessMsg;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDisconnectCommand.NoServerToDisconnectMsg;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.runtime.TeiidInstance;

/**
 * A shell command to connect to the default server
 */
public final class ServerDisconnectCommand extends ServerShellCommand {

    static final String NAME = "server-disconnect"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerDisconnectCommand( final WorkspaceStatus status ) {
        super( NAME, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        if ( !hasWorkspaceServer() ) {
            return new CommandResultImpl( false, getMessage( NoTeiidDefined ), null );
        }

        CommandResult result = null;

        try {
            Teiid teiid = getWorkspaceServer();
            TeiidInstance teiidInstance = teiid.getTeiidInstance( getWorkspaceStatus().getTransaction() );

            if ( teiidInstance.isConnected() ) {
                print( CompletionConstants.MESSAGE_INDENT,
                       getMessage( AttemptingToDisconnect, teiid.getName( getWorkspaceStatus().getTransaction() ) ) );

                teiidInstance.disconnect();
                result = new CommandResultImpl( getMessage( DisconnectSuccessMsg, teiid.getName( getTransaction() ) ) );
            } else {
                result = new CommandResultImpl( getMessage( NoServerToDisconnectMsg ) );
            }
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, getMessage( ConnectionError, e.getLocalizedMessage() ), e );
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
        return 0;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        return hasConnectedWorkspaceServer();
    }

}
