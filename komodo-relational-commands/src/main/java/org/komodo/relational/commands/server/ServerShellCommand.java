/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.server;

import java.util.Arrays;
import java.util.List;

import org.komodo.relational.commands.RelationalShellCommand;
import org.komodo.relational.teiid.Teiid;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.ui.DefaultLabelProvider;
import org.komodo.utils.KLog;
import org.komodo.utils.i18n.I18n;

/**
 * A base class for @{link {@link Teiid server}-related shell commands.
 */
abstract class ServerShellCommand extends RelationalShellCommand {

    protected static final String ADMIN_PORT = "adminPort"; //$NON-NLS-1$
    protected static final String ADMIN_PSWD = "adminPswd"; //$NON-NLS-1$
    protected static final String ADMIN_USER = "adminUser"; //$NON-NLS-1$
    protected static final String ADMIN_SECURE = "adminSecure"; //$NON-NLS-1$
    protected static final String JDBC_PORT = "jdbcPort"; //$NON-NLS-1$
    protected static final String JDBC_PSWD = "jdbcPswd"; //$NON-NLS-1$
    protected static final String JDBC_USER = "jdbcUser"; //$NON-NLS-1$
    protected static final String JDBC_SECURE = "jdbcSecure"; //$NON-NLS-1$
    protected static final String HOST = "host"; //$NON-NLS-1$

    protected static final List< String > ALL_PROPS = Arrays.asList( new String[] { ADMIN_PORT, ADMIN_PSWD, ADMIN_USER, ADMIN_SECURE,
                                                                                    JDBC_PORT, JDBC_PSWD, JDBC_USER, JDBC_SECURE, HOST } );
    protected ServerShellCommand( final String name,
                                  final WorkspaceStatus status ) {
        super( status, name );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        return hasConnectedWorkspaceServer();
    }

    protected boolean isWorkspaceContext() {
        return DefaultLabelProvider.WORKSPACE_PATH.equals( getContext().getAbsolutePath() );
    }

    /**
     * Validates the existence of a connected server.
     * @return the result
     */
    protected CommandResult validateHasConnectedWorkspaceServer() {
        if( !hasConnectedWorkspaceServer() ) {
            return new CommandResultImpl(false, I18n.bind(ServerCommandsI18n.serverNotConnected), null );
        }
        return CommandResult.SUCCESS;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.RelationalShellCommand#getCategory()
     */
    @Override
    public String getCategory() {
        return I18n.bind( ServerCommandsI18n.commandCategory );
    }

    protected String getWorkspaceServerName() throws KException {
        return ServerManager.getInstance(getWorkspaceStatus()).getDefaultServer( ).getName( getTransaction() );
    }

    protected Teiid getWorkspaceServer() throws KException {
        return ServerManager.getInstance(getWorkspaceStatus()).getDefaultServer( );
    }

    protected TeiidInstance getWorkspaceTeiidInstance() throws KException {
        return ServerManager.getInstance(getWorkspaceStatus()).getDefaultTeiidInstance( );
    }

    protected boolean connectWorkspaceServer() throws Exception {
        return ServerManager.getInstance(getWorkspaceStatus()).connectDefaultServer( );
    }

    protected boolean disconnectWorkspaceServer() throws Exception {
        return ServerManager.getInstance(getWorkspaceStatus()).disconnectDefaultServer( );
    }

    protected boolean hasConnectedWorkspaceServer( ) {
        boolean isConnected = false;
        try {
            isConnected = ServerManager.getInstance(getWorkspaceStatus()).isDefaultServerConnected( );
        } catch ( KException e ) {
            KLog.getLogger().error( "Error attempting to check default server connection status: " + e.getLocalizedMessage() ); //$NON-NLS-1$
        }
        return isConnected;
    }

}
