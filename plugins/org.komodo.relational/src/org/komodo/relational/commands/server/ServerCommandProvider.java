/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.server;

import static org.komodo.relational.commands.server.ServerCommandMessages.Common.Connected;
import static org.komodo.relational.commands.server.ServerCommandMessages.Common.NotConnected;
import static org.komodo.relational.commands.server.ServerCommandMessages.Common.serverStatusText;
import static org.komodo.relational.commands.server.ServerCommandMessages.Common.CurrentTeiid;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import org.komodo.relational.Messages;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.runtime.TeiidInstance;

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
    public Map< String, Class< ? extends ShellCommand >> provideCommands() {
        final Map< String, Class< ? extends ShellCommand >> result = new HashMap<>();

        result.put( ServerConnectCommand.NAME, ServerConnectCommand.class );
        result.put( ServerDisconnectCommand.NAME, ServerDisconnectCommand.class );
        result.put( ServerSetCommand.NAME, ServerSetCommand.class );
        result.put( ServerShowVdbsCommand.NAME, ServerShowVdbsCommand.class );
        result.put( ServerShowTranslatorsCommand.NAME, ServerShowTranslatorsCommand.class );
        result.put( ServerShowDatasourcesCommand.NAME, ServerShowDatasourcesCommand.class );
        result.put( ServerShowDatasourceTypesCommand.NAME, ServerShowDatasourceTypesCommand.class );

        return result;
    }
    
    @Override
    public Teiid resolve ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TeiidImpl.RESOLVER.resolvable(uow, kObj)) {
            return TeiidImpl.RESOLVER.resolve(uow, kObj);
        }
        return null;
    }
    
    @Override
    public boolean isRoot ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TeiidImpl.RESOLVER.resolvable(uow, kObj)) {
            return false;
        }
        return false;
    }
    
    @Override
    public String getTypeDisplay ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TeiidImpl.RESOLVER.resolvable(uow, kObj)) {
            Teiid teiid = TeiidImpl.RESOLVER.resolve(uow, kObj);
            return teiid.getTypeDisplayName();
        }
        return null;
    }
    
    @Override
    public String getStatusMessage ( final Repository.UnitOfWork uow, final KomodoObject kObj ) throws KException {
        if(TeiidImpl.RESOLVER.resolvable(uow, kObj)) {
            Teiid teiid = (Teiid)kObj;

            TeiidInstance teiidInstance = teiid.getTeiidInstance(uow);
            String teiidName = teiid.getName(uow);
            String teiidUrl = teiidInstance.getUrl();
            String teiidConnected = teiidInstance.isConnected() ? getMessage(Connected) : getMessage(NotConnected);
            String currentServerText = getMessage(serverStatusText, teiidName, teiidUrl, teiidConnected);

            String resultMessage = getMessage(CurrentTeiid,currentServerText);

            return resultMessage;
        }
        return null;
    }
    
    private String getMessage(Enum< ? > key, Object... parameters) {
        return Messages.getString(ServerCommandMessages.RESOURCE_BUNDLE,key.toString(),parameters);
    }
    
    /* (non-Javadoc)
     * @see org.komodo.shell.api.ShellCommandProvider#initWorkspaceState(org.komodo.spi.repository.Reposi)
     */
    /**
     * @throws KException the exception 
     */
    @Override
    public void initWorkspaceState(WorkspaceStatus wsStatus) throws KException {
        Properties globalProps = wsStatus.getProperties();
        // Look for Server default key.  If found, attempt to set the state object
        if(globalProps.containsKey(ServerCommandProvider.SERVER_DEFAULT_KEY)) {
            String defaultServerName = globalProps.getProperty(ServerCommandProvider.SERVER_DEFAULT_KEY);
            WorkspaceManager wsMgr = WorkspaceManager.getInstance(wsStatus.getCurrentContext().getRepository());
            Teiid teiid = ServerUtils.getWorkspaceTeiidObject(wsMgr, wsStatus, defaultServerName);
            wsStatus.setStateObject(ServerCommandProvider.SERVER_DEFAULT_KEY, teiid);
        }
    }

}
