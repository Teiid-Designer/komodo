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

import java.util.Calendar;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.resource.DdlFile;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.resource.ResourceFile;
import org.komodo.relational.resource.UdfFile;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * A model of a dataservice instance
 */
public interface Dataservice extends Exportable, RelationalObject, VdbEntryContainer {

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
    Dataservice[] NO_DATASERVICES = new Dataservice[ 0 ];

    /**
     * The resolver of a {@link Dataservice}.
     */
    TypeResolver< Dataservice > RESOLVER = new TypeResolver< Dataservice >() {

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
            return ObjectImpl.validateType( transaction,
                                            kobject.getRepository(),
                                            kobject,
                                            DataVirtLexicon.DataService.NODE_TYPE );
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
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param vdb
     *        the VDB being added (cannot be <code>null</code>)
     * @return the VDB entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry addVdb( final UnitOfWork transaction,
                     final Vdb vdb ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param vdbEntryName
     *        the name of the VDB entry to create (cannot be empty)
     * @return the VDB entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry addVdbEntry( final UnitOfWork transaction,
                          final String vdbEntryName ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param serviceVdb
     *        the service VDB being set (can be <code>null</code> when deleting current value)
     * @return the service VDB being replaced or <code>null</code> if one is not being replaced
     * @throws KException
     *         if an error occurs
     */
    Vdb setServiceVdb( final UnitOfWork uow,
                       final Vdb serviceVdb ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the service VDB (may be <code>null</code> if not defined)
     * @throws KException
     *         if an error occurs
     */
    Vdb getServiceVdb( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the service VDB entry (may be <code>null</code> if not defined)
     * @throws KException
     *         if an error occurs
     */
    ServiceVdbEntry getServiceVdbEntry( final UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the name of the dataservice view (may be <code>null</code> if not found)
     * @throws KException
     *         if an error occurs
     */
    String getServiceViewName( UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the name of the dataservice view model (may be <code>null</code> if not found)
     * @throws KException
     *         if an error occurs
     */
    String getServiceViewModelName( UnitOfWork uow ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the driver entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    DriverEntry[] getDriverEntries( final UnitOfWork transaction,
                                    final String... namePatterns ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the paths of the driver files (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    String[] getDriverPlan( UnitOfWork uow ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the driver files referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Driver[] getDrivers( final UnitOfWork transaction,
                         final String... namePatterns ) throws KException;

    /**
     * This does not include the service VDB.
     *
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the paths of the VDB resources referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    String[] getVdbPlan( UnitOfWork uow ) throws KException;

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param teiid
     *        the Teiid instance
     * @return the deployment status of this data service to the given teiid
     */
    DeployStatus deploy( UnitOfWork uow,
                         Teiid teiid );

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param connection
     *        the connection being added to the data service (cannot be <code>null</code> or empty)
     * @return the connection entry (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ConnectionEntry addConnection( final UnitOfWork transaction,
                                   final Connection connection ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param connectionEntryName
     *        the name of the connection entry to create (cannot be <code>null</code> or empty)
     * @return the connection entry (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ConnectionEntry addConnectionEntry( final UnitOfWork transaction,
                                        final String connectionEntryName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param ddlEntryName
     *        the name of the DDL entry to create (cannot be empty)
     * @return the DDL entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    DdlEntry addDdlEntry( final UnitOfWork transaction,
                          final String ddlEntryName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param ddlFile
     *        the DDL file being added to the data service (cannot be <code>null</code>)
     * @return the DDL entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    DdlEntry addDdlFile( final UnitOfWork transaction,
                         final DdlFile ddlFile ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param driverFile
     *        the driver file being added to the data service (cannot be <code>null</code>)
     * @return the driver entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    DriverEntry addDriverFile( final UnitOfWork transaction,
                               final Driver driverFile ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param driverEntryName
     *        the name of the driver entry to create (cannot be empty)
     * @return the driver entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    DriverEntry addDriverEntry( final UnitOfWork transaction,
                                final String driverEntryName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param resourceFile
     *        the resource file being added to the data service (cannot be <code>null</code>)
     * @return the resource entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ResourceEntry addResourceFile( final UnitOfWork transaction,
                                   final ResourceFile resourceFile ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param resourceEntryName
     *        the name of the resource entry to create (cannot be empty)
     * @return the resource entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ResourceEntry addResourceEntry( final UnitOfWork transaction,
                                    final String resourceEntryName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param udfEntryName
     *        the name of the UDF entry to create (cannot be empty)
     * @return the UDF entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    UdfEntry addUdfEntry( final UnitOfWork transaction,
                          final String udfEntryName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param udfFile
     *        the UDF being added to the data service (cannot be empty)
     * @return the UDF entry that was created (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    UdfEntry addUdfFile( final UnitOfWork transaction,
                         final UdfFile udfFile ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    DataServiceEntry< ? > getChild( final UnitOfWork transaction,
                                    final String name ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String,
     *      java.lang.String)
     */
    @Override
    DataServiceEntry< ? > getChild( final UnitOfWork transaction,
                                    final String name,
                                    final String typeName ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    DataServiceEntry< ? >[] getChildren( final UnitOfWork transaction,
                                         final String... namePatterns ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String[])
     */
    @Override
    DataServiceEntry< ? >[] getChildrenOfType( final UnitOfWork transaction,
                                               final String type,
                                               final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the connection entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    ConnectionEntry[] getConnectionEntries( final UnitOfWork transaction,
                                            final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the connections referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Connection[] getConnections( final UnitOfWork transaction,
                                 final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the paths of the connections referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    String[] getConnectionPlan( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the DDL entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    DdlEntry[] getDdlEntries( final UnitOfWork transaction,
                              final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the DDL files referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    DdlFile[] getDdlFiles( final UnitOfWork transaction,
                           final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the paths of the DDL file resources referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    String[] getDdlPlan( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the last time the manifest was modified (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Calendar getLastModified( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the name of the user who last modified the data service (can be <code>null</code> or empty)
     * @throws KException
     *         if an error occurs
     */
    String getModifiedBy( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the paths of the miscellaneous resources referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    String[] getResourcePlan( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the resource entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    ResourceEntry[] getResourceEntries( final UnitOfWork transaction,
                                        final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the resource files referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    ResourceFile[] getResourceFiles( final UnitOfWork transaction,
                                     final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return the paths of the UDF resources referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    String[] getUdfPlan( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the UDF entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    UdfEntry[] getUdfEntries( final UnitOfWork transaction,
                              final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the UDF files referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    UdfFile[] getUdfFiles( final UnitOfWork transaction,
                           final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the VDB entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    VdbEntry[] getVdbEntries( final UnitOfWork transaction,
                              final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the VDBs referenced by the entries (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Vdb[] getVdbs( final UnitOfWork transaction,
                   final String... namePatterns ) throws KException;

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
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newLastModified
     *        the new value of the <code>last modified date</code> property
     * @throws KException
     *         if an error occurs
     */
    void setLastModified( final UnitOfWork transaction,
                          final Calendar newLastModified ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newModifiedBy
     *        the new value of the <code>modified by</code> property
     * @throws KException
     *         if an error occurs
     */
    void setModifiedBy( final UnitOfWork transaction,
                        final String newModifiedBy ) throws KException;

    /**
     * Copies the {@link Dataservice}'s properties to the given {@link Dataservice}
     *
     * @param transaction
     *          the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param dataservice
     *          the destination data service
     * @throws KException
     *          if an error occurs
     */
    void clone(UnitOfWork transaction, Dataservice dataservice) throws KException;

}
