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

import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.ServerManager;
import org.komodo.relational.workspace.ServerManager.ServerCallback;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.utils.ArgCheck;

/**
 *  Manages the default server used for the VDB Builder Session.
 */
public class WkspStatusServerManager implements StringConstants {

    private static WkspStatusServerManager instance;

    /**
     * @param wsStatus the WorkspaceStatus
     * @return singleton instance
     * @throws KException if error occurs
     */
    public static WkspStatusServerManager getInstance(final WorkspaceStatus wsStatus) throws KException {
        if (instance == null) {
            instance = new WkspStatusServerManager(wsStatus);
        }
        return instance;
    }

    /**
     * Reset the manager
     * @throws KException if error occurs
     */
    public static void reset() throws KException {
        if (instance == null)
            return;

        Repository repository = instance.wsStatus.getCurrentContext().getRepository();
        ServerManager.uncacheInstance(repository);
        instance = null;
    }

    private class UpdateCallback implements ServerCallback {
        @Override
        public void execute() {
            wsStatus.updateAvailableCommands();
        }
    }

    private UpdateCallback wsUpdateCallback = new UpdateCallback();

    private WorkspaceStatus wsStatus;

    private ServerManager serverManager;

    private WkspStatusServerManager(WorkspaceStatus wsStatus) throws KException {
        ArgCheck.isNotNull(wsStatus, "wsStatus");
        this.wsStatus = wsStatus;
        this.serverManager = ServerManager.getInstance(wsStatus.getCurrentContext().getRepository());
    }

    /**
         * Get the server defined for this session.  If the default server does not exist, create it.
         * @return the Teiid server
         * @throws KException if error occurs
         */
    public Teiid getDefaultServer() throws KException {
        return serverManager.getDefaultServer(wsStatus.getTransaction());
    }

    /**
      * Requests connection to default server
      * @return <code>true</code> if the server is connected, <code>false</code> if not.
      * @throws KException if error occurs
      */
    public boolean connectDefaultServer() throws KException {
        return serverManager.connectDefaultServer(wsStatus.getTransaction(), wsUpdateCallback);
    }

    /**
     * Requests default server disconnect
     * @return <code>true</code> if the server is connected, <code>false</code> if not.
     * @throws KException if error occurs
     */
    public boolean disconnectDefaultServer() throws KException {
        return serverManager.disconnectDefaultServer(wsStatus.getTransaction(), wsUpdateCallback);
    }

    /**
     * Determine if the default server is connected.
     * @return <code>true</code> if the server is connected, <code>false</code> if not.
     * @throws KException if error occurs
     */
    public boolean isDefaultServerConnected() throws KException {
        return serverManager.isDefaultServerConnected(wsStatus.getTransaction());
    }

    /**
     * Get the Teiid instance associated with the current server.
     * @return the Teiid instance
     * @throws KException if error occurs
     */
    public TeiidInstance getDefaultTeiidInstance() throws KException {
        return serverManager.getDefaultTeiidInstance(wsStatus.getTransaction());
    }
}
