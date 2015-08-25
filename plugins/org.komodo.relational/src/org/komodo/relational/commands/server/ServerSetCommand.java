/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.ServerSetCommand.MISSING_SERVER_NAME;
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
        String serverName = requiredArgument(0, getMessage(MISSING_SERVER_NAME));

        WorkspaceStatus wsStatus = getWorkspaceStatus();

        wsStatus.setServer(serverName);
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
