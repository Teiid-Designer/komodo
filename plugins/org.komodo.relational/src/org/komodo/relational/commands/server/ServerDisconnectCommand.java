/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.Common.NoTeiidDefined;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDisconnectCommand.AttemptingToDisconnect;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDisconnectCommand.DisconnectSuccessMsg;
import static org.komodo.relational.commands.server.ServerCommandMessages.ServerDisconnectCommand.NoServerToDisconnectMsg;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CompletionConstants;
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
        super( NAME, true, status );
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected boolean doExecute() throws Exception {
        if(!hasDefaultServer()) {
            print(CompletionConstants.MESSAGE_INDENT, getMessage(NoTeiidDefined));
            return false;
        }
        
        Teiid teiid = getDefaultServer();

        print(CompletionConstants.MESSAGE_INDENT, getMessage(AttemptingToDisconnect,teiid.getName(getWorkspaceStatus().getTransaction())));
        TeiidInstance teiidInstance = teiid.getTeiidInstance(getWorkspaceStatus().getTransaction());
        if(!teiidInstance.isConnected()) {
            print(CompletionConstants.MESSAGE_INDENT, getMessage(NoServerToDisconnectMsg));
        }
        
        teiidInstance.disconnect();
        print(CompletionConstants.MESSAGE_INDENT, getMessage(DisconnectSuccessMsg, teiid.getName(getWorkspaceStatus().getTransaction())));
        
        return true;
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public final boolean isValidForCurrentContext() {
        return hasConnectedDefaultTeiid();
    }
    
}
