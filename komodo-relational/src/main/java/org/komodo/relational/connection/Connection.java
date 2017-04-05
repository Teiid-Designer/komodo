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
package org.komodo.relational.connection;

import java.util.Properties;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.connection.internal.ConnectionImpl;
import org.komodo.relational.teiid.Teiid;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.TeiidInstance;
import org.teiid.modeshape.sequencer.dataservice.DataServiceManifest;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * A model of a connection instance
 */
public interface Connection extends Exportable, RelationalObject {

    /**
     * The file extension of connections.
     */
    DocumentType DOC_TYPE = new DocumentType( DataServiceManifest.CONNECTION_ENTRY_SUFFIX );

    /**
     * The type identifier.
     */
    int TYPE_ID = Connection.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.CONNECTION;

    /**
     * An empty array of connections.
     */
    Connection[] NO_CONNECTIONS = new Connection[0];

    /**
     * The default value for the <code>jdbc</code> property. Value is {@value} .
     */
    boolean DEFAULT_JDBC = true;

    /**
     * The default value for the <code>preview</code> property. Value is {@value} .
     */
    boolean DEFAULT_PREVIEW = false;

    /**
     * The resolver of a {@link Connection}.
     */
    TypeResolver< Connection > RESOLVER = new TypeResolver< Connection >() {

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
        public Class< ConnectionImpl > owningClass() {
            return ConnectionImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, DataVirtLexicon.Connection.NODE_TYPE );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Connection resolve( final UnitOfWork transaction,
                              final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Connection.TYPE_ID ) {
                return ( Connection )kobject;
            }
            return new ConnectionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.Exportable#getDocumentType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    default DocumentType getDocumentType( final UnitOfWork transaction ) {
        return DOC_TYPE;
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return id of this connection
     * @throws KException
     */
    String getId(UnitOfWork uow) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return description of this connection (may be <code>null</code>)
     * @throws KException if error occurs
     */
    String getDescription(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param description description of this connection
     * @throws KException if error occurs
     */
    void setDescription(UnitOfWork transaction, String description) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return external location of this connection (may be <code>null</code>)
     * @throws KException if error occurs
     */
    String getExternalLocation(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param extLoc external location of this connection
     * @throws KException if error occurs
     */
    void setExternalLocation(UnitOfWork transaction, String extLoc) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return jndi name of this connection (may be <code>null</code>)
     * @throws KException if error occurs
     */
    String getJndiName(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param jndiName jndi name of this connection
     * @throws KException if error occurs
     */
    void setJndiName(UnitOfWork transaction, String jndiName) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return driver name of this connection.  (may be <code>null</code>)
     * @throws KException if error occurs
     */
    String getDriverName(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param driverName driver name of this connection
     * @throws KException if error occurs
     */
    void setDriverName(UnitOfWork transaction, String driverName) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return class name of this connection.  (may be <code>null</code>)
     * @throws KException if error occurs
     */
    String getClassName(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param className class name of this connection
     * @throws KException if error occurs
     */
    void setClassName(UnitOfWork transaction, String className) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return 'true' if a JDBC source, 'false' if not.
     * @throws KException if error occurs
     */
    boolean isJdbc(UnitOfWork transaction) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param isJdbc 'true' if source is JDBC, 'false' if not.
     * @throws KException if error occurs
     */
    void setJdbc(UnitOfWork transaction, boolean isJdbc) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param teiidInstance the teiid instance for deployment
     * @return the properties for server deployment
     * @throws Exception if error occurs
     */
    Properties getPropertiesForServerDeployment(UnitOfWork transaction, TeiidInstance teiidInstance) throws Exception;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param teiid
     *        the Teiid instance
     * @return the deployment status of this data source to the given teiid
     */
    DeployStatus deploy(UnitOfWork uow, Teiid teiid);

}
