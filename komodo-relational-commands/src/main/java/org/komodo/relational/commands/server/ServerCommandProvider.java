/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.commands.server;

import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.ServerManager;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.KLog;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command provider for server commands.
 */
public class ServerCommandProvider implements ShellCommandProvider {

    /**
     * Key for storage of default server in the servers area.
     */
    public static final String SERVER_DEFAULT_KEY = "SERVER_DEFAULT"; //$NON-NLS-1$

    /**
     * Constructs a command provider for VDB shell commands.
     */
    public ServerCommandProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#provideCommands()
     */
    @Override
    public Set< Class< ? extends ShellCommand > > provideCommands() {
        final Set< Class< ? extends ShellCommand > > result = new HashSet< >();

        result.add( ServerConnectCommand.class );
        result.add( ServerDisconnectCommand.class );
        result.add( ServerVdbsCommand.class );
        result.add( ServerTranslatorsCommand.class );
        result.add( ServerDatasourcesCommand.class );
        result.add( ServerDatasourceTypesCommand.class );
        result.add( ServerDeployDatasourceCommand.class );
        result.add( ServerDeployDriverCommand.class );
        result.add( ServerDeployVdbCommand.class );
        result.add( ServerUndeployDatasourceCommand.class );
        result.add( ServerUndeployVdbCommand.class );
        result.add( ServerGetDatasourceCommand.class );
        result.add( ServerGetVdbCommand.class );
        result.add( ServerVdbCommand.class );
        result.add( ServerTranslatorCommand.class );
        result.add( ServerDatasourceCommand.class );
        result.add( ServerDatasourceTypeCommand.class );
        result.add( ServerShowPropertiesCommand.class );
        result.add( ServerSetPropertyCommand.class );
        result.add( ServerUnsetPropertyCommand.class );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @SuppressWarnings( "unchecked" )
    @Override
    public Teiid resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(Teiid.RESOLVER.resolvable(uow, kObj)) {
            return Teiid.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#getStatusMessage(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public String getStatusMessage( final WorkspaceStatus wsStatus ) throws KException {
        TeiidInstance teiidInstance = WkspStatusServerManager.getInstance(wsStatus).getDefaultTeiidInstance(); 
        String teiidUrl = teiidInstance.getUrl();
        
        boolean isConnected = WkspStatusServerManager.getInstance(wsStatus).isDefaultServerConnected();
        String teiidConnected = isConnected ? I18n.bind( ServerCommandsI18n.connected )
                                            : I18n.bind( ServerCommandsI18n.notConnected );
        
        String currentServerText = I18n.bind(ServerCommandsI18n.serverStatusText, teiidUrl, teiidConnected);

        return I18n.bind(ServerCommandsI18n.currentServer,currentServerText);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#initWorkspaceState(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public void initWorkspaceState(WorkspaceStatus wsStatus) {
        Properties providedGlobalProps = wsStatus.getProvidedGlobalProperties();

        // If provided global 'connect on startup' check is not available, set it to the default.
        if(!providedGlobalProps.containsKey(ServerManager.SERVER_CONNECT_ON_STARTUP)) {
            wsStatus.setProvidedGlobalProperty( ServerManager.SERVER_CONNECT_ON_STARTUP, Boolean.toString(ServerManager.DEFAULT_SERVER_CONNECT_ON_STARTUP), Boolean.class.getName() );
        }
        
        boolean connectOnStartup = Boolean.parseBoolean( wsStatus.getProvidedGlobalProperties().getProperty( ServerManager.SERVER_CONNECT_ON_STARTUP ) );
        if(connectOnStartup) {
            try {
                WkspStatusServerManager.getInstance( wsStatus ).connectDefaultServer( );
            } catch (Exception ex) {
                KLog.getLogger().error(I18n.bind(ServerCommandsI18n.errorConnectingToServerOnStartup));
            }
        }
    }
    
}
