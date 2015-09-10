/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.ServerSetCommand.MissingServerNameArg;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerSetCommand.ServerDoesNotExist;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerSetCommand.ServerSetSuccess;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.repository.KomodoObject;

/**
 * A shell command to set the default server name
 */
public final class ServerSetCommand extends ServerShellCommand {

    static final String NAME = "server-set"; //$NON-NLS-1$

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public ServerSetCommand( final WorkspaceStatus status ) {
        super( NAME, true, status );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        String serverName = requiredArgument(0, getMessage(MissingServerNameArg));

        // Validate that server object with this name exists in the workspace
        Teiid wsTeiid = getWorkspaceTeiid(serverName);
        if(wsTeiid==null) {
            print(CompletionConstants.MESSAGE_INDENT, getMessage(ServerDoesNotExist,serverName));
            return false;
        }
        
        // Check for current server
        KomodoObject currentServer = getWorkspaceStatus().getServer();
        if(currentServer!=null) {
            // Request set to current server, no need to reset
            if(serverName.equals(currentServer.getName(getTransaction()))) {
                print(CompletionConstants.MESSAGE_INDENT, getMessage(ServerSetSuccess,serverName));
                return true;
            // Has different server currently.  Disconnect it.
            } else {
                if(hasConnectedDefaultTeiid()) {
                    getCommand(ServerDisconnectCommand.NAME).execute();
                }
            }
        }
        
        // Set the server name on workspace status
        getWorkspaceStatus().setServer(wsTeiid);

        print(CompletionConstants.MESSAGE_INDENT, getMessage(ServerSetSuccess,serverName));
        return true;
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
