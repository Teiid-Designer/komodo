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
package org.komodo.relational.dataservice.internal;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import org.komodo.relational.DeployStatus;
import org.komodo.relational.Messages;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.dataservice.ConnectionEntry;
import org.komodo.relational.dataservice.DataServiceEntry;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.DdlEntry;
import org.komodo.relational.dataservice.DriverEntry;
import org.komodo.relational.dataservice.ResourceEntry;
import org.komodo.relational.dataservice.ServiceVdbEntry;
import org.komodo.relational.dataservice.UdfEntry;
import org.komodo.relational.dataservice.VdbEntry;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Model.Type;
import org.komodo.relational.model.View;
import org.komodo.relational.resource.DdlFile;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.resource.ResourceFile;
import org.komodo.relational.resource.UdfFile;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.repository.RepositoryTools;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

/**
 * Implementation of data service instance model.
 */
public class DataserviceImpl extends RelationalObjectImpl implements Dataservice {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { VdbEntry.IDENTIFIER,
                                                                       ConnectionEntry.IDENTIFIER,
                                                                       DriverEntry.IDENTIFIER,
                                                                       DdlEntry.IDENTIFIER,
                                                                       ResourceEntry.IDENTIFIER,
                                                                       UdfEntry.IDENTIFIER };

    private static < T extends Exportable & RelationalObject > String[] getPathsOfReferences( final UnitOfWork transaction,
                                                                                              final DataServiceEntry< T >[] entries ) throws KException {
        if ( entries.length == 0 ) {
            return StringConstants.EMPTY_ARRAY;
        }

        final List< String > paths = new ArrayList<>( entries.length );

        for ( final DataServiceEntry< T > entry : entries ) {
            T ref = null;

            if ( ( ref = entry.getReference( transaction ) ) != null ) {
                paths.add( ref.getAbsolutePath() );
            }
        }

        return paths.toArray( new String[ paths.size() ] );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository
     * @param path
     *        the path
     * @throws KException
     *         if error occurs
     */
    public DataserviceImpl(final UnitOfWork transaction,
                      final Repository repository,
                      final String path ) throws KException {
        super(transaction, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork transaction) {
        return Dataservice.IDENTIFIER;
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.repository.Exportable#export(org.komodo.spi.repository.Repository.UnitOfWork, java.util.Properties)
     */
    @Override
    public byte[] export(UnitOfWork transaction, Properties exportProperties) throws KException {
        DataserviceConveyor conveyor = new DataserviceConveyor(getRepository());
        return conveyor.export(transaction, this, exportProperties);
    }

    @Override
    public DocumentType getDocumentType(UnitOfWork transaction) {
        return DocumentType.ZIP;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildTypes()
     */
    @Override
    public KomodoType[] getChildTypes() {
        return CHILD_TYPES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public DataServiceEntry< ? > getChild( final UnitOfWork transaction,
                                           final String name ) throws KException {
        // check service VDB
        final ServiceVdbEntry entry = getServiceVdbEntry( transaction );

        if ( ( entry != null ) && name.equals( entry.getName( transaction ) ) ) {
            return entry;
        }

        // check VDBs
        DataServiceEntry< ? >[] kids = getVdbEntries( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check connections
        kids = getConnectionEntries( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check drivers
        kids = getDriverEntries( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check DDL files
        kids = getDdlEntries( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check resources
        kids = getResourceEntries( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // check UDFs
        kids = getUdfEntries( transaction, name );

        if ( kids.length != 0 ) {
            return kids[ 0 ];
        }

        // child does not exist
        throw new KException( Messages.getString( org.komodo.repository.Messages.Komodo.CHILD_NOT_FOUND,
                                                  name,
                                                  getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public DataServiceEntry< ? > getChild( final UnitOfWork transaction,
                                           final String name,
                                           final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( DataVirtLexicon.VdbEntry.NODE_TYPE.equals( typeName ) ) {
            final VdbEntry[] entries = getVdbEntries( transaction, name );

            if ( entries.length != 0 ) {
                return entries[ 0 ];
            }
        } else if ( DataVirtLexicon.ConnectionEntry.NODE_TYPE.equals( typeName ) ) {
            final ConnectionEntry[] entries = getConnectionEntries( transaction, name );

            if ( entries.length != 0 ) {
                return entries[ 0 ];
            }
        } else if ( DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE.equals( typeName ) ) {
            final DriverEntry[] entries = getDriverEntries( transaction, name );

            if ( entries.length != 0 ) {
                return entries[ 0 ];
            }
        } else if ( DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE.equals( typeName ) ) {
            final DdlEntry[] entries = getDdlEntries( transaction, name );

            if ( entries.length != 0 ) {
                return entries[ 0 ];
            }
        } else if ( DataVirtLexicon.ResourceEntry.NODE_TYPE.equals( typeName ) ) {
            final ResourceEntry[] entries = getResourceEntries( transaction, name );

            if ( entries.length != 0 ) {
                return entries[ 0 ];
            }
        } else if ( DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE.equals( typeName ) ) {
            final UdfEntry[] entries = getUdfEntries( transaction, name );

            if ( entries.length != 0 ) {
                return entries[ 0 ];
            }
        } else if ( DataVirtLexicon.ServiceVdbEntry.NODE_TYPE.equals( typeName ) ) {
            return getServiceVdbEntry( transaction );
        }

        // child does not exist
        throw new KException( Messages.getString( org.komodo.repository.Messages.Komodo.CHILD_NOT_FOUND,
                                                  name,
                                                  getAbsolutePath() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public DataServiceEntry< ? >[] getChildren( final UnitOfWork transaction,
                                                final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final ServiceVdbEntry serviceVdb = getServiceVdbEntry( transaction );
        final VdbEntry[] vdbs = getVdbEntries( transaction, namePatterns );
        final ConnectionEntry[] connections = getConnectionEntries( transaction, namePatterns );
        final DriverEntry[] drivers = getDriverEntries( transaction, namePatterns );
        final DdlEntry[] ddls = getDdlEntries( transaction, namePatterns );
        final ResourceEntry[] resources = getResourceEntries( transaction, namePatterns );
        final UdfEntry[] udfs = getUdfEntries( transaction, namePatterns );

        final DataServiceEntry< ? >[] result = new DataServiceEntry< ? >[ ( ( serviceVdb == null ) ? 0 : 1 )
                                                                          + vdbs.length
                                                                          + connections.length
                                                                          + drivers.length
                                                                          + ddls.length
                                                                          + resources.length
                                                                          + udfs.length ];
        System.arraycopy( vdbs, 0, result, 0, vdbs.length );
        System.arraycopy( connections, 0, result, vdbs.length, connections.length );
        System.arraycopy( drivers, 0, result, vdbs.length + connections.length, drivers.length );
        System.arraycopy( ddls, 0, result, vdbs.length + connections.length + drivers.length, ddls.length );
        System.arraycopy( resources,
                          0,
                          result,
                          vdbs.length + connections.length + drivers.length + ddls.length,
                          resources.length );
        System.arraycopy( udfs,
                          0,
                          result,
                          vdbs.length + connections.length + drivers.length + ddls.length + resources.length,
                          udfs.length );

        if ( serviceVdb != null ) {
            result[ result.length - 1 ] = serviceVdb;
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String[])
     */
    @Override
    public DataServiceEntry< ? >[] getChildrenOfType( final UnitOfWork transaction,
                                                      final String type,
                                                      final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        if ( DataVirtLexicon.ServiceVdbEntry.NODE_TYPE.equals( type ) ) {
            final ServiceVdbEntry entry = getServiceVdbEntry( transaction );

            if ( entry == null ) {
                return VdbEntry.NO_ENTRIES;
            }

            return new ServiceVdbEntry[] { entry };
        }

        if ( DataVirtLexicon.VdbEntry.NODE_TYPE.equals( type ) ) {
            return getVdbEntries( transaction, namePatterns );
        }

        if ( DataVirtLexicon.ConnectionEntry.NODE_TYPE.equals( type ) ) {
            return getConnectionEntries( transaction, namePatterns );
        }

        if ( DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE.equals( type ) ) {
            return getDriverEntries( transaction, namePatterns );
        }

        if ( DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE.equals( type ) ) {
            return getDdlEntries( transaction, namePatterns );
        }

        if ( DataVirtLexicon.ResourceEntry.NODE_TYPE.equals( type ) ) {
            return getResourceEntries( transaction, namePatterns );
        }

        if ( DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE.equals( type ) ) {
            return getUdfEntries( transaction, namePatterns );
        }

        return VdbEntry.NO_ENTRIES;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$

        return ( ( ( getServiceVdbEntry( transaction ) != null )
                   && name.equals( getServiceVdbEntry( transaction ).getName( transaction ) ) )
                 || ( getVdbEntries( transaction, name ).length != 0 )
                 || ( getConnectionEntries( transaction, name ).length != 0 )
                 || ( getDriverEntries( transaction, name ).length != 0 )
                 || ( getDdlEntries( transaction, name ).length != 0 )
                 || ( getResourceEntries( transaction, name ).length != 0 )
                 || ( getUdfEntries( transaction, name ).length != 0 ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChild(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String)
     */
    @Override
    public boolean hasChild( final UnitOfWork transaction,
                             final String name,
                             final String typeName ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state must be NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( typeName, "typeName" ); //$NON-NLS-1$

        if ( DataVirtLexicon.ServiceVdbEntry.NODE_TYPE.equals( typeName ) ) {
            return ( ( getServiceVdbEntry( transaction ) != null )
                     && name.equals( getServiceVdbEntry( transaction ).getName( transaction ) ) );
        }

        if (DataVirtLexicon.VdbEntry.NODE_TYPE.equals(typeName)) {
            return (getVdbEntries(transaction, name).length != 0);
        }

        if (DataVirtLexicon.ConnectionEntry.NODE_TYPE.equals(typeName)) {
            return (getConnectionEntries(transaction, name).length != 0);
        }

        if (DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE.equals(typeName)) {
            return (getDriverEntries(transaction, name).length != 0);
        }

        if ( DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE.equals( typeName ) ) {
            return ( getDdlEntries( transaction, name ).length != 0 );
        }

        if ( DataVirtLexicon.ResourceEntry.NODE_TYPE.equals( typeName ) ) {
            return ( getResourceEntries( transaction, name ).length != 0 );
        }

        if ( DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE.equals( typeName ) ) {
            return ( getUdfEntries( transaction, name ).length != 0 );
        }

        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#hasChildren(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean hasChildren( final UnitOfWork transaction ) throws KException {
        return ( ( getServiceVdbEntry( transaction ) != null )
                 || ( getVdbEntries( transaction ).length != 0 )
                 || ( getConnectionEntries( transaction ).length != 0 )
                 || ( getDriverEntries( transaction ).length != 0 )
                 || ( getDdlEntries( transaction ).length != 0 )
                 || ( getResourceEntries( transaction ).length != 0 )
                 || ( getUdfEntries( transaction ).length != 0 ) );
    }

    @Override
    public String[] getVdbPlan(UnitOfWork transaction) throws KException {
        final VdbEntry[] entries = getVdbEntries( transaction );

        if ( entries.length == 0 ) {
            return StringConstants.EMPTY_ARRAY;
        }

        return getPathsOfReferences( transaction, entries );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getServiceVdb(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Vdb getServiceVdb( final UnitOfWork transaction ) throws KException {
        final ServiceVdbEntry entry = getServiceVdbEntry( transaction );
        return ( ( entry == null ) ? null : entry.getReference( transaction ) );
    }

    @Override
    public DeployStatus deploy(UnitOfWork uow, Teiid teiid) {
        DataserviceConveyor conveyor = new DataserviceConveyor(getRepository());
        return conveyor.deploy(uow, this, teiid);
    }

    /* (non-Javadoc)
     * @see org.komodo.relational.dataservice.Dataservice#getDataserviceView(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getServiceViewName(UnitOfWork uow) throws KException {
        String viewName = null;
        // Only ONE virtual model should exist in the dataservice vdb.
        // The returned view name is the first view in the first virtual model found - or null if none found.
        Vdb serviceVdb = getServiceVdb(uow);
        if( serviceVdb != null ) {
            Model[] models = serviceVdb.getModels(uow);
            for(Model model : models) {
                Model.Type modelType = model.getModelType(uow);
                if(modelType == Type.VIRTUAL) {
                    View[] views = model.getViews(uow);
                    for(View view : views) {
                        viewName = view.getName(uow);
                        break;
                    }
                }
            }
        }
        return viewName;
    }

    /* (non-Javadoc)
     * @see org.komodo.relational.dataservice.Dataservice#getDataserviceViewModel(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getServiceViewModelName(UnitOfWork uow) throws KException {
        String viewModelName = null;
        // Only ONE virtual model should exist in the dataservice vdb.
        // The returned view model is the first virtual model found - or null if none found.
        Vdb serviceVdb = getServiceVdb(uow);
        if( serviceVdb != null ) {
            Model[] models = serviceVdb.getModels(uow);
            for(Model model : models) {
                Model.Type modelType = model.getModelType(uow);
                if(modelType == Type.VIRTUAL) {
                    viewModelName = model.getName(uow);
                    break;
                }
            }
        }
        return viewModelName;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addConnection(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.connection.Connection)
     */
    @Override
    public ConnectionEntry addConnection( final UnitOfWork uow,
                                          final Connection connection ) throws KException {
        final ConnectionEntry entry = RelationalModelFactory.createConnectionEntry( uow,
                                                                                    getRepository(),
                                                                                    this,
                                                                                    Objects.requireNonNull( connection,
                                                                                                            "connection" ) //$NON-NLS-1$
                                                                                           .getName( uow ) );
        entry.setReference( uow, connection );
        entry.setJndiName( uow, connection.getJndiName( uow ) );
        return entry;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addConnectionEntry(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public ConnectionEntry addConnectionEntry( final UnitOfWork transaction,
                                               final String connectionEntryName ) throws KException {
        return RelationalModelFactory.createConnectionEntry( transaction, getRepository(), this, connectionEntryName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addDdlEntry(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public DdlEntry addDdlEntry( final UnitOfWork transaction,
                                 final String ddlEntryName ) throws KException {
        return RelationalModelFactory.createDdlEntry( transaction, getRepository(), this, ddlEntryName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addDdlFile(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.resource.DdlFile)
     */
    @Override
    public DdlEntry addDdlFile( final UnitOfWork uow,
                                final DdlFile ddlFile ) throws KException {
        final DdlEntry entry = RelationalModelFactory.createDdlEntry( uow,
                                                                      getRepository(),
                                                                      this,
                                                                      Objects.requireNonNull( ddlFile, "ddlFile" ) //$NON-NLS-1$
                                                                             .getName( uow ) );
        entry.setReference( uow, ddlFile );
        return entry;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addDriverEntry(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public DriverEntry addDriverEntry( final UnitOfWork transaction,
                                       final String driverEntryName ) throws KException {
        return RelationalModelFactory.createDriverEntry( transaction, getRepository(), this, driverEntryName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addDriverFile(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.resource.Driver)
     */
    @Override
    public DriverEntry addDriverFile( final UnitOfWork uow,
                                      final Driver driverFile ) throws KException {
        final DriverEntry entry = RelationalModelFactory.createDriverEntry( uow,
                                                                            getRepository(),
                                                                            this,
                                                                            Objects.requireNonNull( driverFile, "driverFile" ) //$NON-NLS-1$
                                                                                   .getName( uow ) );
        entry.setReference( uow, driverFile );
        return entry;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addResourceEntry(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public ResourceEntry addResourceEntry( final UnitOfWork transaction,
                                           final String resourceEntryName ) throws KException {
        return RelationalModelFactory.createResourceEntry( transaction, getRepository(), this, resourceEntryName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addResourceFile(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.resource.ResourceFile)
     */
    @Override
    public ResourceEntry addResourceFile( final UnitOfWork uow,
                                          final ResourceFile resourceFile ) throws KException {
        final ResourceEntry entry = RelationalModelFactory.createResourceEntry( uow,
                                                                                getRepository(),
                                                                                this,
                                                                                Objects.requireNonNull( resourceFile,
                                                                                                        "resourceFile" ) //$NON-NLS-1$
                                                                                       .getName( uow ) );
        entry.setReference( uow, resourceFile );
        return entry;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addUdfEntry(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public UdfEntry addUdfEntry( final UnitOfWork transaction,
                                 final String udfEntryName ) throws KException {
        return RelationalModelFactory.createUdfEntry( transaction, getRepository(), this, udfEntryName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addUdfFile(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.resource.UdfFile)
     */
    @Override
    public UdfEntry addUdfFile( final UnitOfWork uow,
                                final UdfFile udfFile ) throws KException {
        final UdfEntry entry = RelationalModelFactory.createUdfEntry( uow,
                                                                      getRepository(),
                                                                      this,
                                                                      Objects.requireNonNull( udfFile, "udfFile" ) //$NON-NLS-1$
                                                                             .getName( uow ) );
        entry.setReference( uow, udfFile );
        return entry;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addVdb(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.vdb.Vdb)
     */
    @Override
    public VdbEntry addVdb( final UnitOfWork uow,
                            final Vdb vdb ) throws KException {
        final VdbEntry entry = RelationalModelFactory.createVdbEntry( uow,
                                                                      getRepository(),
                                                                      this,
                                                                      Objects.requireNonNull( vdb, "vdb" ).getName( uow ) ); //$NON-NLS-1$
        entry.setVdbName( uow, vdb.getName( uow ) );
        entry.setVdbVersion( uow, Integer.toString( vdb.getVersion( uow ) ) );
        entry.setReference( uow, vdb );
        return entry;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#addVdbEntry(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public VdbEntry addVdbEntry( final UnitOfWork transaction,
                            final String vdbEntryName ) throws KException {
        return RelationalModelFactory.createVdbEntry( transaction, getRepository(), this, vdbEntryName );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getConnectionEntries(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String[])
     */
    @Override
    public ConnectionEntry[] getConnectionEntries( final UnitOfWork transaction,
                                                   final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< ConnectionEntry > result = new ArrayList<>();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction,
                                                                    DataVirtLexicon.ConnectionEntry.NODE_TYPE,
                                                                    namePatterns ) ) {
            final ConnectionEntry entry = new ConnectionEntryImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( entry );
        }

        if ( result.isEmpty() ) {
            return ConnectionEntry.NO_ENTRIES;
        }

        return result.toArray( new ConnectionEntry[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getConnections(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public Connection[] getConnections( final UnitOfWork transaction,
                                        final String... namePatterns ) throws KException {
        final ConnectionEntry[] entries = getConnectionEntries( transaction, namePatterns );

        if ( entries.length == 0 ) {
            return Connection.NO_CONNECTIONS;
        }

        final List< Connection > connections = new ArrayList<>( entries.length );

        for ( final ConnectionEntry entry : entries ) {
            Connection ref = null;

            if ( ( ref = entry.getReference( transaction ) ) != null ) {
                connections.add( ref );
            }
        }

        return connections.toArray( new Connection[ connections.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getConnectionPlan(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getConnectionPlan( final UnitOfWork transaction ) throws KException {
        final ConnectionEntry[] entries = getConnectionEntries( transaction );

        if ( entries.length == 0 ) {
            return StringConstants.EMPTY_ARRAY;
        }

        return getPathsOfReferences( transaction, entries );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getDdlEntries(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public DdlEntry[] getDdlEntries( final UnitOfWork transaction,
                                     final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< DdlEntry > result = new ArrayList<>();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction,
                                                                    DataVirtLexicon.ResourceEntry.DDL_ENTRY_NODE_TYPE,
                                                                    namePatterns ) ) {
            final DdlEntry entry = new DdlEntryImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( entry );
        }

        if ( result.isEmpty() ) {
            return DdlEntry.NO_ENTRIES;
        }

        return result.toArray( new DdlEntry[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getDdlFiles(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public DdlFile[] getDdlFiles( final UnitOfWork transaction,
                                  final String... namePatterns ) throws KException {
        final DdlEntry[] entries = getDdlEntries( transaction, namePatterns );

        if ( entries.length == 0 ) {
            return DdlFile.NO_DDLS;
        }

        final List< DdlFile > ddls = new ArrayList<>( entries.length );

        for ( final DdlEntry entry : entries ) {
            DdlFile ref = null;

            if ( ( ref = entry.getReference( transaction ) ) != null ) {
                ddls.add( ref );
            }
        }

        return ddls.toArray( new DdlFile[ ddls.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getDdlPlan(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getDdlPlan( final UnitOfWork transaction ) throws KException {
        final DdlEntry[] entries = getDdlEntries( transaction );

        if ( entries.length == 0 ) {
            return StringConstants.EMPTY_ARRAY;
        }

        return getPathsOfReferences( transaction, entries );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getDescription", //$NON-NLS-1$
                                  DataVirtLexicon.DataService.DESCRIPTION );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getDriverEntries(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public DriverEntry[] getDriverEntries( final UnitOfWork uow,
                                           final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< DriverEntry > result = new ArrayList<>();

        for ( final KomodoObject kobject : super.getChildrenOfType( uow, DataVirtLexicon.ResourceEntry.DRIVER_ENTRY_NODE_TYPE, namePatterns ) ) {
            final DriverEntry entry = new DriverEntryImpl( uow, getRepository(), kobject.getAbsolutePath() );
            result.add( entry );
        }

        if ( result.isEmpty() ) {
            return DriverEntry.NO_ENTRIES;
        }

        return result.toArray( new DriverEntry[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getDriverPlan(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getDriverPlan( final UnitOfWork transaction ) throws KException {
        final DriverEntry[] entries = getDriverEntries( transaction );

        if ( entries.length == 0 ) {
            return StringConstants.EMPTY_ARRAY;
        }

        return getPathsOfReferences( transaction, entries );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getDrivers(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public Driver[] getDrivers( final UnitOfWork transaction,
                                    final String... namePatterns ) throws KException {
        final DriverEntry[] entries = getDriverEntries( transaction, namePatterns );

        if ( entries.length == 0 ) {
            return Driver.NO_DRIVERS;
        }

        final List< Driver > drivers = new ArrayList<>( entries.length );

        for ( final DriverEntry entry : entries ) {
            Driver ref = null;

            if ( ( ref = entry.getReference( transaction ) ) != null ) {
                drivers.add( ref );
            }
        }

        return drivers.toArray( new Driver[ drivers.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getLastModified(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Calendar getLastModified( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.CALENDAR,
                                  "getLastModified", //$NON-NLS-1$
                                  DataVirtLexicon.DataService.LAST_MODIFIED );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getModifiedBy(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getModifiedBy( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction,
                                  PropertyValueType.STRING,
                                  "getModifiedBy", //$NON-NLS-1$
                                  DataVirtLexicon.DataService.MODIFIED_BY );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getResourceEntries(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public ResourceEntry[] getResourceEntries( final UnitOfWork uow,
                                               final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< ResourceEntry > result = new ArrayList<>();

        for ( final KomodoObject kobject : super.getChildrenOfType( uow, DataVirtLexicon.ResourceEntry.NODE_TYPE, namePatterns ) ) {
            final ResourceEntry entry = new ResourceEntryImpl( uow, getRepository(), kobject.getAbsolutePath() );
            result.add( entry );
        }

        if ( result.isEmpty() ) {
            return ResourceEntry.NO_ENTRIES;
        }

        return result.toArray( new ResourceEntry[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getResourceFiles(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public ResourceFile[] getResourceFiles( final UnitOfWork transaction,
                                            final String... namePatterns ) throws KException {
        final ResourceEntry[] entries = getResourceEntries( transaction, namePatterns );

        if ( entries.length == 0 ) {
            return ResourceFile.NO_RESOURCES;
        }

        final List< ResourceFile > resources = new ArrayList<>( entries.length );

        for ( final ResourceEntry entry : entries ) {
            ResourceFile ref = null;

            if ( ( ref = entry.getReference( transaction ) ) != null ) {
                resources.add( ref );
            }
        }

        return resources.toArray( new ResourceFile[ resources.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getResourcePlan(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getResourcePlan( final UnitOfWork transaction ) throws KException {
        final ResourceEntry[] entries = getResourceEntries( transaction );

        if ( entries.length == 0 ) {
            return StringConstants.EMPTY_ARRAY;
        }

        return getPathsOfReferences( transaction, entries );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getServiceVdbEntry(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ServiceVdbEntry getServiceVdbEntry( final UnitOfWork uow ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final KomodoObject[] kobjects = super.getChildrenOfType( uow, DataVirtLexicon.ServiceVdbEntry.NODE_TYPE );

        if ( kobjects.length == 0 ) {
            return null;
        }

        return new ServiceVdbEntryImpl( uow, getRepository(), kobjects[ 0 ].getAbsolutePath() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getUdfEntries(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public UdfEntry[] getUdfEntries( final UnitOfWork uow,
                                     final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< UdfEntry > result = new ArrayList<>();

        for ( final KomodoObject kobject : super.getChildrenOfType( uow, DataVirtLexicon.ResourceEntry.UDF_ENTRY_NODE_TYPE, namePatterns ) ) {
            final UdfEntry entry = new UdfEntryImpl( uow, getRepository(), kobject.getAbsolutePath() );
            result.add( entry );
        }

        if ( result.isEmpty() ) {
            return UdfEntry.NO_ENTRIES;
        }

        return result.toArray( new UdfEntry[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getUdfFiles(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public UdfFile[] getUdfFiles( final UnitOfWork transaction,
                                  final String... namePatterns ) throws KException {
        final UdfEntry[] entries = getUdfEntries( transaction, namePatterns );

        if ( entries.length == 0 ) {
            return UdfFile.NO_UDFS;
        }

        final List< UdfFile > udfs = new ArrayList<>( entries.length );

        for ( final UdfEntry entry : entries ) {
            UdfFile ref = null;

            if ( ( ref = entry.getReference( transaction ) ) != null ) {
                udfs.add( ref );
            }
        }

        return udfs.toArray( new UdfFile[ udfs.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getUdfPlan(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String[] getUdfPlan( final UnitOfWork transaction ) throws KException {
        final UdfEntry[] entries = getUdfEntries( transaction );

        if ( entries.length == 0 ) {
            return StringConstants.EMPTY_ARRAY;
        }

        return getPathsOfReferences( transaction, entries );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getVdbEntries(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public VdbEntry[] getVdbEntries( final UnitOfWork uow,
                                     final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( uow, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( uow.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< VdbEntry > result = new ArrayList<>();

        for ( final KomodoObject kobject : super.getChildrenOfType( uow, DataVirtLexicon.VdbEntry.NODE_TYPE, namePatterns ) ) {
            final VdbEntry entry = new VdbEntryImpl( uow, getRepository(), kobject.getAbsolutePath() );
            result.add( entry );
        }

        if ( result.isEmpty() ) {
            return VdbEntry.NO_ENTRIES;
        }

        return result.toArray( new VdbEntry[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#getVdbs(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String[])
     */
    @Override
    public Vdb[] getVdbs( final UnitOfWork transaction,
                          final String... namePatterns ) throws KException {
        final VdbEntry[] entries = getVdbEntries( transaction, namePatterns );

        if ( entries.length == 0 ) {
            return Vdb.NO_VDBS;
        }

        final List< Vdb > vdbs = new ArrayList<>( entries.length );

        for ( final VdbEntry entry : entries ) {
            Vdb ref = null;

            if ( ( ref = entry.getReference( transaction ) ) != null ) {
                vdbs.add( ref );
            }
        }

        return vdbs.toArray( new Vdb[ vdbs.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#setDescription(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setDescription( final UnitOfWork transaction,
                                final String newDescription ) throws KException {
        setObjectProperty( transaction,
                           "setDescription", //$NON-NLS-1$
                           DataVirtLexicon.DataService.DESCRIPTION,
                           newDescription );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#setLastModified(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.util.Calendar)
     */
    @Override
    public void setLastModified( final UnitOfWork transaction,
                                 final Calendar newLastModified ) throws KException {
        setObjectProperty( transaction,
                           "setLastModified", //$NON-NLS-1$
                           DataVirtLexicon.DataService.LAST_MODIFIED,
                           newLastModified );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#setModifiedBy(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public void setModifiedBy( final UnitOfWork transaction,
                               final String newModifiedBy ) throws KException {
        setObjectProperty( transaction,
                           "setModifiedBy", //$NON-NLS-1$
                           DataVirtLexicon.DataService.MODIFIED_BY,
                           newModifiedBy );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.dataservice.Dataservice#setServiceVdb(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.relational.vdb.Vdb)
     */
    @Override
    public Vdb setServiceVdb( final UnitOfWork uow,
                              final Vdb serviceVdb ) throws KException {
        Vdb oldServiceVdb = null;
        final KomodoObject[] kobjects = getChildrenOfType( uow, DataVirtLexicon.ServiceVdbEntry.NODE_TYPE );

        if ( kobjects.length != 0 ) {
            if ( kobjects[ 0 ].hasProperty( uow, DataVirtLexicon.ServiceVdbEntry.VDB_REF ) ) {
                final String refId = kobjects[ 0 ].getProperty( uow, DataVirtLexicon.ServiceVdbEntry.VDB_REF )
                                                  .getStringValue( uow );
                final KomodoObject kobj = getRepository().getUsingId( uow, refId );

                if ( kobj != null ) {
                    oldServiceVdb = new VdbImpl( uow, getRepository(), kobj.getAbsolutePath() );
                }
            }

            // always delete current
            kobjects[ 0 ].remove( uow );
        }

        // add new service VDB if necessary
        if ( serviceVdb != null ) {
            final ServiceVdbEntry entry = RelationalModelFactory.createServiceVdbEntry( uow,
                                                                                        getRepository(),
                                                                                        this,
                                                                                        serviceVdb.getName( uow ) );
            entry.setVdbName( uow, serviceVdb.getName( uow ) );
            entry.setVdbVersion( uow, Integer.toString( serviceVdb.getVersion( uow ) ) );
            entry.setReference( uow, serviceVdb );
        }

        return oldServiceVdb;
    }

    @Override
    public void clone(UnitOfWork transaction, Dataservice dataservice) throws KException {
        dataservice.setDescription(transaction, getDescription(transaction));
        dataservice.setModifiedBy(transaction, getModifiedBy(transaction));
        dataservice.setLastModified(transaction, getLastModified(transaction));

        for (VdbEntry vdbEntry : getVdbEntries(transaction)) {
            VdbEntry entry = RelationalModelFactory.createVdbEntry(transaction,
                                                                          getRepository(),
                                                                          dataservice,
                                                                           vdbEntry.getName(transaction));
            RepositoryTools.copyProperties(transaction, vdbEntry, entry);
        }

        for (UdfEntry udfEntry : getUdfEntries(transaction)) {
            UdfEntry entry = RelationalModelFactory.createUdfEntry(transaction,
                                                                          getRepository(),
                                                                          dataservice,
                                                                          udfEntry.getName(transaction));
            RepositoryTools.copyProperties(transaction, udfEntry, entry);
        }

        for (ConnectionEntry connEntry : getConnectionEntries(transaction)) {
            ConnectionEntry entry = RelationalModelFactory.createConnectionEntry(transaction,
                                                                          getRepository(),
                                                                          dataservice,
                                                                          connEntry.getName(transaction));
            RepositoryTools.copyProperties(transaction, connEntry, entry);
        }

        for (DdlEntry ddlEntry : getDdlEntries(transaction)) {
            DdlEntry entry = RelationalModelFactory.createDdlEntry(transaction,
                                                                          getRepository(),
                                                                          dataservice,
                                                                          ddlEntry.getName(transaction));
            RepositoryTools.copyProperties(transaction, ddlEntry, entry);
        }

        for (ResourceEntry resourceEntry : getResourceEntries(transaction)) {
            ResourceEntry entry = RelationalModelFactory.createResourceEntry(transaction,
                                                                          getRepository(),
                                                                          dataservice,
                                                                          resourceEntry.getName(transaction));
            RepositoryTools.copyProperties(transaction, resourceEntry, entry);
        }

        for (DriverEntry driverEntry : getDriverEntries(transaction)) {
            DriverEntry entry = RelationalModelFactory.createDriverEntry(transaction,
                                                                          getRepository(),
                                                                          dataservice,
                                                                          driverEntry.getName(transaction));
            RepositoryTools.copyProperties(transaction, driverEntry, entry);
        }

        ServiceVdbEntry serviceVdbEntry = getServiceVdbEntry(transaction);
        ServiceVdbEntry entry = RelationalModelFactory.createServiceVdbEntry(transaction,
                                                                                    getRepository(),
                                                                                    dataservice,
                                                                                    serviceVdbEntry.getName(transaction));
        RepositoryTools.copyProperties(transaction, serviceVdbEntry, entry);
    }

}
