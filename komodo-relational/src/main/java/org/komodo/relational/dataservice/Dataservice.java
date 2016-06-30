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
package org.komodo.relational.dataservice;

import org.komodo.core.KomodoLexicon;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.driver.Driver;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * A model of a dataservice instance
 */
public interface Dataservice extends Exportable, RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Dataservice.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.DATASERVICE;

    /**
     * An empty array of Dataservices.
     */
    Dataservice[] NO_DATASERVICES = new Dataservice[0];
    
    /**
     * The resolver of a {@link Dataservice}.
     */
    public static final TypeResolver< Dataservice > RESOLVER = new TypeResolver< Dataservice >() {
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public Dataservice create( final UnitOfWork transaction,
                                   final Repository repository,
                                   final KomodoObject parent,
                                   final String id,
                                   final RelationalProperties properties ) throws KException {
            final WorkspaceManager mgr = WorkspaceManager.getInstance( repository );
            return mgr.createDataservice( transaction, parent, id );
        }
    
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
        public Class< DataserviceImpl > owningClass() {
            return DataserviceImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, KomodoLexicon.DataService.NODE_TYPE );
        }
    
        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Dataservice resolve( final UnitOfWork transaction,
                                    final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Dataservice.TYPE_ID ) {
                return ( Dataservice )kobject;
            }
            return new DataserviceImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    
    };

    /**
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDescription
     *        the new value of the <code>description</code> property
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the VDBs (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Vdb[] getVdbs( final UnitOfWork uow,
                   final String... namePatterns ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param vdbName
     *        the name of the VDB to create (cannot be empty)
     * @param externalFilePath
     *        the VDB file path on the local file system (cannot be empty)
     * @return the VDB (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Vdb addVdb( final UnitOfWork uow,
                final String vdbName,
                final String externalFilePath ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the service VDB (may be <code>null</code> if not defined)
     * @throws KException
     *         if an error occurs
     */
    Vdb getServiceVdb( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the Data sources (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Datasource[] getDataSources( final UnitOfWork uow, final String... namePatterns ) throws KException;

    /**
     * 
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param sourceName
     *        the name of the data source to create (cannot be empty)
     * @return the Datasource (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Datasource addDatasource( final UnitOfWork uow, final String sourceName)  throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the Drivers (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Driver[] getDrivers( final UnitOfWork uow, final String... namePatterns) throws KException;

    /**
     *
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param driverName
     *        the name of the driver being created (cannot be empty)
     * @return the Driver (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Driver addDriver( final UnitOfWork uow, final String driverName, final byte[] content) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the deployment plan for the drivers
     * @throws KException if an error occurs
     */
    String[] getDriverPlan(UnitOfWork uow) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the deployment plan for the data sources
     * @throws KException if an error occurs
     */
    String[] getDataSourcePlan(UnitOfWork uow) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the deployment plan for the vdbs
     * @throws KException if an error occurs
     */
    String[] getVdbPlan(UnitOfWork uow) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the deployment status of this data service to the given teiid
     */
    DeployStatus deploy(UnitOfWork uow, Teiid teiid);
}