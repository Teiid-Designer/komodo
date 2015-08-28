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
import java.util.List;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.WorkspaceStatus;

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

        // Validate that the server object exists
        if(!hasServer(serverName)) {
            print(CompletionConstants.MESSAGE_INDENT, getMessage(ServerDoesNotExist,serverName));
            return false;
        }
        
        // Set the server name
        getWorkspaceStatus().setServer(serverName);
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
    
    // Determine if a server with the supplied name exists
    private boolean hasServer(String serverName) throws Exception {
        List<Teiid> teiids = getWorkspaceManager().findTeiids(getTransaction());

        if (teiids == null || teiids.size() == 0) {
            return false;
        }

        for (Teiid theTeiid : teiids) {
            String teiidName = theTeiid.getName(getTransaction());
            if (serverName.equals(theTeiid.getId(getTransaction())) || serverName.equals(teiidName)) {
                return true;
            }
        }
        return false;
    }
    
}
