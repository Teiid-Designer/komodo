/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.server;

import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.i18n.I18n;

/**
 * A shell command provider for VDBs.
 */
public class ServerCommandProvider implements ShellCommandProvider {

    /**
     * Key for storage of default server in the workspace.
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
        result.add( ServerRenameCommand.class );
        result.add( ServerSetCommand.class );
        result.add( ServerVdbsCommand.class );
        result.add( ServerTranslatorsCommand.class );
        result.add( ServerDatasourcesCommand.class );
        result.add( ServerDatasourceTypesCommand.class );
        result.add( ServerDeployDatasourceCommand.class );
        result.add( ServerDeployVdbCommand.class );
        result.add( ServerUndeployDatasourceCommand.class );
        result.add( ServerUndeployVdbCommand.class );
        result.add( ServerGetDatasourceCommand.class );
        result.add( ServerGetVdbCommand.class );
        result.add( ServerVdbCommand.class );
        result.add( ServerTranslatorCommand.class );
        result.add( ServerDatasourceCommand.class );
        result.add( ServerDatasourceTypeCommand.class );

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
     * @see org.komodo.shell.api.ShellCommandProvider#getStatusMessage(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public String getStatusMessage ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(Teiid.RESOLVER.resolvable(uow, kObj)) {
            Teiid teiid = (Teiid)kObj;

            TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);
            String teiidName = teiid.getName(uow);
            String teiidUrl = teiidInstance.getUrl();
            String teiidConnected = teiidInstance.isConnected() ? I18n.bind( ServerCommandsI18n.connected )
                                                                : I18n.bind( ServerCommandsI18n.notConnected );
            String currentServerText = I18n.bind(ServerCommandsI18n.serverStatusText, teiidName, teiidUrl, teiidConnected);

            String resultMessage = I18n.bind(ServerCommandsI18n.currentTeiid,currentServerText);

            return resultMessage;
        }
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#initWorkspaceState(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public void initWorkspaceState(WorkspaceStatus wsStatus) throws KException {
        Properties providedProps = wsStatus.getProvidedProperties();
        // Look for Server default key.  If found, attempt to set the state object
        if(providedProps.containsKey(ServerCommandProvider.SERVER_DEFAULT_KEY)) {
            String defaultServerName = providedProps.getProperty(ServerCommandProvider.SERVER_DEFAULT_KEY);
            WorkspaceManager wsMgr = WorkspaceManager.getInstance(wsStatus.getCurrentContext().getRepository());
            Teiid teiid = ServerUtils.getWorkspaceTeiidObject(wsMgr, wsStatus, defaultServerName);
            wsStatus.setStateObject(ServerCommandProvider.SERVER_DEFAULT_KEY, teiid);
        }
    }

}
