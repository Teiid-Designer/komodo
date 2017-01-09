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
package org.komodo.relational.teiid;

import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.query.QueryService;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.version.TeiidVersion;

/**
 * A model of a teiid instance
 */
public interface Teiid extends RelationalObject, TeiidArchetype {

    /**
     * The type identifier.
     */
    int TYPE_ID = Teiid.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.TEIID;

    /**
     * An empty array of teiids.
     */
    Teiid[] NO_TEIIDS = new Teiid[0];

    /**
     * The resolver of a {@link Teiid}.
     */
    TypeResolver< Teiid > RESOLVER = new TypeResolver< Teiid >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class< TeiidImpl > owningClass() {
            return TeiidImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, KomodoLexicon.Teiid.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Teiid resolve( final UnitOfWork transaction,
                              final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Teiid.TYPE_ID ) {
                return ( Teiid )kobject;
            }

            return new TeiidImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param uow the transaction
     * @return the teiid instance
     */
    TeiidInstance getTeiidInstance(UnitOfWork uow);

    /**
     * @param uow the transaction
     * @return the query service
     * @throws KException if error occurs
     */
    QueryService getQueryService(UnitOfWork uow) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return id of this teiid model
     * @throws KException
     */
    @Override
    String getId(UnitOfWork uow) throws KException;

    /**
     * @param uow
     *         the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the teiid version
     * @throws KException
     */
    @Override
    TeiidVersion getVersion(UnitOfWork uow) throws KException;

    /**
     * @param uow
     *         the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param version the teiid version
     * @throws KException
     */
    void setVersion(UnitOfWork uow, TeiidVersion version) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param host the host name
     * @throws KException if error occurs
     */
    void setHost(UnitOfWork uow, String host) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param port
     * @throws KException if error occurs
     */
    void setAdminPort(UnitOfWork uow, int port) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param userName
     * @throws KException if error occurs
     */
    void setAdminUser(UnitOfWork uow, String userName) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param password
     * @throws KException if error occurs
     */
    void setAdminPassword(UnitOfWork uow, String password) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param secure
     * @throws KException if error occurs
     */
    void setAdminSecure(UnitOfWork uow, boolean secure) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param port
     * @throws KException if error occurs
     */
    void setJdbcPort(UnitOfWork uow, int port) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param userName
     * @throws KException if error occurs
     */
    void setJdbcUsername(UnitOfWork uow, String userName) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param password
     * @throws KException if error occurs
     */
    void setJdbcPassword(UnitOfWork uow, String password) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param secure
     * @throws KException if error occurs
     */
    void setJdbcSecure(UnitOfWork uow, boolean secure) throws KException;

    /**
     * @param uow
     * @return whether this teiid model is currently connected to the teiid instance
     */
    boolean isConnected(UnitOfWork uow);

    /**
     * Import the teiid content into the teiid cache
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     *        AND should be owned by {@link Repository#SYSTEM_USER}
     *
     * @return the cached teiid node
     *
     * @throws KException
     */
    CachedTeiid importContent(UnitOfWork transaction) throws KException;
}
