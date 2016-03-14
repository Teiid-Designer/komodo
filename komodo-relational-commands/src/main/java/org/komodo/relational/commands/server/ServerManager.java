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

import javax.jcr.Node;
import javax.jcr.Session;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.teiid.Teiid;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.TeiidInstance;
import org.modeshape.jcr.api.JcrTools;

/**
 *  Manages the default server used for the VDB Builder Session.
 */
public class ServerManager implements StringConstants {

    /**
     * Global property key - indicates if server connection should be attempted on startup.
     */
    public static String SERVER_CONNECT_ON_STARTUP = "SERVER_CONNECT_ON_STARTUP"; //$NON-NLS-1$

    /**
     * The name of the default server
     */
    public static final String DEFAULT_SERVER_NAME = "DefaultServer"; //$NON-NLS-1$

    /**
     * The default server connect on startup value
     */
    public static final boolean DEFAULT_SERVER_CONNECT_ON_STARTUP = true;


    private static ServerManager instance;
    private WorkspaceStatus wsStatus;
    private Teiid server;
    private boolean isConnected = false;
    
    /**
     * @param wsStatus the WorkspaceStatus
     * @return singleton instance
     */
    public static ServerManager getInstance( final WorkspaceStatus wsStatus ) {
        if (instance == null) {
            instance = new ServerManager();
            instance.wsStatus = wsStatus;
        }
        return instance;
    }

    private ServerManager() {
    }
    
    /**
     * Get the server defined for this session.  If the default server does not exist, create it.
     * @return the Teiid server
     * @throws KException if error occurs
     */
    public Teiid getDefaultServer( ) throws KException {
        if( this.server == null ) {
            UnitOfWork uow = this.wsStatus.getTransaction();
            KomodoObject serversNode = getServersNode( );

            KomodoObject serverObj = null;
            if(!serversNode.hasChild(uow, DEFAULT_SERVER_NAME)) {
                serverObj = serversNode.addChild( uow, DEFAULT_SERVER_NAME, KomodoLexicon.Teiid.NODE_TYPE);
            } else {
                serverObj = serversNode.getChild( uow, DEFAULT_SERVER_NAME, KomodoLexicon.Teiid.NODE_TYPE );
            }
            if( serverObj!=null && Teiid.RESOLVER.resolvable( uow, serverObj ) ) {
                this.server = Teiid.RESOLVER.resolve( uow, serverObj );
            }
        }
        return this.server;
    }
    
    /**
     * Reset the manager
     */
    public static void reset() {
        instance = null;
    }
    
    /**
     * Requests connection to default server
     * @return <code>true</code> if the server is connected, <code>false</code> if not.
     * @throws KException if error occurs
     */
    public boolean connectDefaultServer( ) throws KException {
        if(!isDefaultServerConnected( )) {
            TeiidInstance defaultTeiid = getDefaultTeiidInstance( );
            if(defaultTeiid!=null) {
                try {
                    defaultTeiid.connect();
                } catch (Exception ex) {
                    throw new KException(ex);
                }
                isConnected = defaultTeiid.isConnected();
                this.wsStatus.updateAvailableCommands();
            }
        }
        return isConnected;
    }

    /**
     * Requests default server disconnect
     * @return <code>true</code> if the server is connected, <code>false</code> if not.
     */
    public boolean disconnectDefaultServer( ) {
        if(server!=null) {
            TeiidInstance teiidInstance = server.getTeiidInstance(this.wsStatus.getTransaction());
            if(teiidInstance!=null) teiidInstance.disconnect();
        }
        isConnected = false;
        this.wsStatus.updateAvailableCommands();
        return isConnected;
    }
    
    /**
     * Determine if the default server is connected.
     * @return <code>true</code> if the server is connected, <code>false</code> if not.
     * @throws KException if error occurs
     */
    public boolean isDefaultServerConnected( ) throws KException {
        return isConnected;
    }
    
    /**
     * Get the Teiid instance associated with the current server.
     * @return the Teiid instance
     * @throws KException if error occurs
     */
    public TeiidInstance getDefaultTeiidInstance( ) throws KException {
        UnitOfWork uow = this.wsStatus.getTransaction();
        Teiid wsTeiid = getDefaultServer( );
        return (wsTeiid!=null) ? wsTeiid.getTeiidInstance( uow ) : null;
    }
    
    /**
     * Get the servers area in the repository.
     * @return the servers grouping node
     * @throws KException if an error occurs
     */
    private KomodoObject getServersNode( ) throws KException {
        UnitOfWork uow = this.wsStatus.getTransaction();

        final Session session = ( ( UnitOfWorkImpl )uow ).getSession();
        final JcrTools jcrTools = new JcrTools();
        try {
            final Node node = jcrTools.findOrCreateNode( session, RepositoryImpl.SERVERS_ROOT );

            return new ObjectImpl( this.wsStatus.getCurrentContext().getRepository() , node.getPath(), node.getIndex() );
        } catch (Exception e) {
            throw new KException(e);
        }
    }
    
}
