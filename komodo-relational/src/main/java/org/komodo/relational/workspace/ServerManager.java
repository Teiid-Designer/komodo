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
package org.komodo.relational.workspace;

import org.komodo.core.KomodoLexicon;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.teiid.CachedTeiid;
import org.komodo.relational.teiid.Teiid;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.Id;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;
import org.komodo.utils.ArgCheck;

public class ServerManager extends ObjectImpl implements RelationalObject {

    /**
     * Callback interface for actions to be executed
     * after both server connections and disconnections.
     */
    public interface ServerCallback {

        /**
         * Execute the custom operation
         */
        public void execute();
    }

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

    /**
     * The type identifier.
     */
    public static final int TYPE_ID = ServerManager.class.hashCode();

    private static class ServerMgrAdapter implements KeyFromValueAdapter<Repository.Id, ServerManager> {

        @Override
        public Id getKey(ServerManager value) {
            Repository repository = value.getRepository();
            return repository.getId();
        }
    }

    private static KeyFromValueAdapter<Repository.Id, ServerManager> adapter = new ServerMgrAdapter();

    private static KeyInValueHashMap<Repository.Id, ServerManager> instances = new KeyInValueHashMap<Repository.Id, ServerManager>(adapter);

    /**
     * @param repository
     *        the repository whose workspace manager is being requested (cannot be <code>null</code>)
     *
     * @return singleton instance
     * @throws KException
     *                  if there is an error obtaining the server manager
     */
    public static ServerManager getInstance(Repository repository) throws KException {
        ServerManager instance = instances.get(repository.getId());

        if ( instance == null ) {
            // We must create a transaction here so that it can be passed on to the constructor. Since the
            // node associated with the ServerManager always exists we don't have to create it.
            final UnitOfWork uow = repository.createTransaction(Repository.SYSTEM_USER, "createServerManager", false, null ); //$NON-NLS-1$
            instance = new ServerManager( uow, repository );
            uow.commit();

            instances.add( instance );
        }

        return instance;
    }

    /**
     * Primarily used in tests to remove the workspace manager instance from the instances cache.
     *
     * @param repository remove instance with given repository
     */
    public static void uncacheInstance(final Repository repository) {
        if (repository == null)
            return;

        instances.remove(repository.getId());
    }

    private ServerManager(UnitOfWork uow, Repository repository ) throws KException {
        super( repository, RepositoryImpl.SERVERS_ROOT, 0 );

        repository.addObserver(new RepositoryObserver() {

            @Override
            public void eventOccurred() {
                // Disposal observer
                if (getRepository() == null || State.NOT_REACHABLE == getRepository().getState() || !(getRepository().ping())) {
                    instances.remove(ServerManager.this);
                }
            }

            @Override
            public void errorOccurred(Throwable e) {
                // Nothing to do
            }
        });
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject#getFilters()
     */
    @Override
    public Filter[] getFilters() {
        return RelationalObject.NO_FILTERS;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.RelationalObject#setFilters(org.komodo.relational.RelationalObject.Filter[])
     */
    @Override
    public void setFilters( Filter[] newFilters ) {
        // filters not allowed
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return KomodoType.SERVER_MANAGER;
    }

    /**
     * Get the server defined for this session.  If the default server does not exist, create it.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return the Teiid server
     * @throws KException if error occurs
     */
    public Teiid getDefaultServer(UnitOfWork transaction) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == UnitOfWork.State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        KomodoObject serversNode = getRepository().komodoServersNode(transaction);

        KomodoObject serverObj = null;
        if (!serversNode.hasChild(transaction, DEFAULT_SERVER_NAME)) {
            serverObj = serversNode.addChild(transaction, DEFAULT_SERVER_NAME, KomodoLexicon.Teiid.NODE_TYPE);
        } else {
            serverObj = serversNode.getChild(transaction, DEFAULT_SERVER_NAME, KomodoLexicon.Teiid.NODE_TYPE);
        }

        if (serverObj != null && Teiid.RESOLVER.resolvable(transaction, serverObj)) {
            return Teiid.RESOLVER.resolve(transaction, serverObj);
        }

        throw new KException(Messages.getString(Relational.SERVER_MANAGER_DEFAULT_TEIID_ERROR));
    }

    /**
     * Requests connection to default server
     *
     *  @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param callback the callback to be executed after successful connection (can be null)
     *
     * @return <code>true</code> if the server is connected, <code>false</code> if not.
     * @throws KException if error occurs
     */
    public boolean connectDefaultServer(UnitOfWork transaction, ServerCallback callback) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == UnitOfWork.State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        if (isDefaultServerConnected(transaction))
            return true;

        TeiidInstance defaultTeiid = getDefaultTeiidInstance(transaction);
        try {
            defaultTeiid.connect();
        } catch (Exception ex) {
            throw new KException(ex);
        }

        if (callback != null)
            callback.execute();

        return defaultTeiid.isConnected();
    }

    /**
     * Requests default server disconnect
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param callback the callback to be executed after successful connection (can be null)
     *
     * @return <code>true</code> if the server is connected, <code>false</code> if not.
     * @throws KException if error occurs
     */
    public boolean disconnectDefaultServer(UnitOfWork transaction, ServerCallback callback) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == UnitOfWork.State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        Teiid server = getDefaultServer(transaction);

        TeiidInstance teiidInstance = server.getTeiidInstance(transaction);
        if (teiidInstance != null)
            teiidInstance.disconnect();

        if (callback != null)
            callback.execute();

        return isDefaultServerConnected(transaction);
    }

    /**
     * Determine if the default server is connected.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return <code>true</code> if the server is connected, <code>false</code> if not.
     * @throws KException if error occurs
     */
    public boolean isDefaultServerConnected(UnitOfWork transaction) throws KException {
        TeiidInstance teiidInstance = getDefaultTeiidInstance(transaction);
        if (teiidInstance == null)
            throw new KException("Failed to acquire the default teiid instance");

        return teiidInstance.isConnected();
    }

    /**
     * Get the Teiid instance associated with the current server.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *
     * @return the Teiid instance
     * @throws KException if error occurs
     */
    public TeiidInstance getDefaultTeiidInstance(UnitOfWork transaction) throws KException {
        ArgCheck.isNotNull(transaction, "transaction"); //$NON-NLS-1$
        ArgCheck.isTrue((transaction.getState() == UnitOfWork.State.NOT_STARTED), "transaction state is not NOT_STARTED"); //$NON-NLS-1$

        Teiid wsTeiid = getDefaultServer(transaction);
        return wsTeiid.getTeiidInstance(transaction);
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     *        AND should be owned by {@link Repository#SYSTEM_USER}
     * @param srcTeiid the source teiid model
     * @return the teiid object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public CachedTeiid createCachedTeiid( final UnitOfWork uow, final Teiid srcTeiid) throws KException {
        return RelationalModelFactory.createCachedTeiid( uow, getRepository(), srcTeiid);
    }
}
