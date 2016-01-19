/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.CompletionConstants;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.i18n.I18n;

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
            return new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.noTeiidDefined ), null );
        }

        CommandResult result = null;

        try {
            TeiidInstance teiidInstance = getWorkspaceTeiidInstance();

            if ( teiidInstance.isConnected() ) {
                print( CompletionConstants.MESSAGE_INDENT,
                       I18n.bind( ServerCommandsI18n.attemptingToDisconnect, getWorkspaceServerName() ) );

                teiidInstance.disconnect();

                // Updates available commands upon disconnecting
                getWorkspaceStatus().updateAvailableCommands();

                result = new CommandResultImpl( I18n.bind( ServerCommandsI18n.disconnectSuccessMsg, getWorkspaceServerName() ) );
            } else {
                result = new CommandResultImpl( I18n.bind( ServerCommandsI18n.noServerToDisconnectMsg ) );
            }
        } catch ( final Exception e ) {
            result = new CommandResultImpl( false, I18n.bind( ServerCommandsI18n.connectionError, e.getLocalizedMessage() ), e );
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
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDisconnectHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDisconnectExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ServerCommandsI18n.serverDisconnectUsage ) );
    }

}
