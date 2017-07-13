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
package org.komodo.relational.folder.internal;

import java.util.ArrayList;
import java.util.List;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.connection.internal.ConnectionImpl;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.folder.Folder;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.internal.SchemaImpl;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.resource.internal.DriverImpl;
import org.komodo.relational.template.Template;
import org.komodo.relational.template.internal.TemplateImpl;
import org.komodo.relational.vdb.Translator;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.TranslatorImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionConfigurationEvent;
import org.komodo.spi.runtime.ExecutionConfigurationListener;
import org.komodo.utils.ArgCheck;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * Implementation of Folder instance model
 */
public class FolderImpl extends RelationalObjectImpl implements Folder, EventManager {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { Connection.IDENTIFIER, Vdb.IDENTIFIER, Schema.IDENTIFIER, 
                                                                       Dataservice.IDENTIFIER, Translator.IDENTIFIER, Driver.IDENTIFIER, 
                                                                       Folder.IDENTIFIER };
    
    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository
     * @param path
     *        the path
     * @throws KException
     *         if error occurs
     */
    public FolderImpl( final UnitOfWork uow,
                       final Repository repository,
                       final String path ) throws KException {
        super(uow, repository, path);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return Folder.IDENTIFIER;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    @Override
    public Dataservice addDataservice( final UnitOfWork uow,
                                       final String serviceName ) throws KException {
         return RelationalModelFactory.createDataservice( uow, getRepository(), this.getAbsolutePath(), serviceName );
    }

    @Override
    public Connection addConnection( final UnitOfWork uow,
                                     final String connectionName ) throws KException {
         return RelationalModelFactory.createConnection( uow, getRepository(), this.getAbsolutePath(), connectionName );
    }

    @Override
    public Folder addFolder( final UnitOfWork uow,
                             final String folderName ) throws KException {
        return RelationalModelFactory.createFolder( uow, getRepository(), this.getAbsolutePath(), folderName );
    }

    @Override
    public Schema addSchema( final UnitOfWork uow,
                             final String schemaName ) throws KException {
         return RelationalModelFactory.createSchema( uow, getRepository(), this.getAbsolutePath(), schemaName );
    }

    @Override
    public Vdb addVdb( final UnitOfWork uow,
                       final String vdbName,
                       final String externalFilePath ) throws KException {
        return RelationalModelFactory.createVdb( uow, getRepository(), this.getAbsolutePath(), vdbName, externalFilePath );
    }

    @Override
    public Dataservice[] getDataservices( final UnitOfWork transaction,
                                          final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Dataservice > result = new ArrayList< >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, DataVirtLexicon.DataService.NODE_TYPE, namePatterns ) ) {
            final Dataservice service = new DataserviceImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( service );
        }

        if ( result.isEmpty() ) {
            return Dataservice.NO_DATASERVICES;
        }

        return result.toArray( new Dataservice[ result.size() ] );
    }

    @Override
    public Connection[] getConnections( final UnitOfWork transaction,
                                        final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Connection > result = new ArrayList< >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, DataVirtLexicon.Connection.NODE_TYPE, namePatterns ) ) {
            final Connection ds = new ConnectionImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( ds );
        }

        if ( result.isEmpty() ) {
            return Connection.NO_CONNECTIONS;
        }

        return result.toArray( new Connection[ result.size() ] );
    }

    @Override
    public Vdb[] getVdbs( final UnitOfWork transaction,
                          final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Vdb > result = new ArrayList< >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, VdbLexicon.Vdb.VIRTUAL_DATABASE, namePatterns ) ) {
            final Vdb vdb = new VdbImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( vdb );
        }

        if ( result.isEmpty() ) {
            return Vdb.NO_VDBS;
        }

        return result.toArray( new Vdb[ result.size() ] );
    }

    @Override
    public Schema[] getSchemas( final UnitOfWork transaction,
                                final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Schema > result = new ArrayList< >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, KomodoLexicon.Schema.NODE_TYPE, namePatterns ) ) {
            final Schema schema = new SchemaImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( schema );
        }

        if ( result.isEmpty() ) {
            return Schema.NO_SCHEMAS;
        }

        return result.toArray( new Schema[ result.size() ] );
    }
    
    @Override
    public Translator[] getTranslators( final UnitOfWork transaction,
                                        final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Translator > result = new ArrayList< Translator >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, VdbLexicon.Translator.TRANSLATOR, namePatterns ) ) {
            final Translator translator = new TranslatorImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( translator );
        }

        if ( result.isEmpty() ) {
            return Translator.NO_TRANSLATORS;
        }

        return result.toArray( new Translator[ result.size() ] );
    }

    @Override
    public Driver[] getDrivers( final UnitOfWork transaction,
                                final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Driver > result = new ArrayList< Driver >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE, namePatterns ) ) {
            final DriverImpl driver = new DriverImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( driver );
        }

        if ( result.isEmpty() ) {
            return Driver.NO_DRIVERS;
        }

        return result.toArray( new Driver[ result.size() ] );
    }

    @Override
    public Template[] getTemplates( final UnitOfWork transaction,
                                        final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Template > result = new ArrayList< Template >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, DataVirtLexicon.Template.NODE_TYPE, namePatterns ) ) {
            final Template template = new TemplateImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( template );
        }

        if ( result.isEmpty() ) {
            return Template.NO_TEMPLATES;
        }

        return result.toArray( new Template[ result.size() ] );
    }

    @Override
    public Folder[] getFolders( final UnitOfWork transaction,
                                final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Folder > result = new ArrayList< >();

        for ( final KomodoObject kobject : super.getChildrenOfType( transaction, KomodoLexicon.Folder.NODE_TYPE, namePatterns ) ) {
            final Folder folder = new FolderImpl( transaction, getRepository(), kobject.getAbsolutePath() );
            result.add( folder );
        }

        if ( result.isEmpty() ) {
            return Folder.NO_FOLDERS;
        }

        return result.toArray( new Folder[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String, java.lang.String[])
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork transaction,
                                             final String type,
                                             final String... namePatterns ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject[] result = null;

        if ( KomodoLexicon.Folder.NODE_TYPE.equals( type ) ) {
            result = getFolders( transaction, namePatterns );
        } else if ( KomodoLexicon.Schema.NODE_TYPE.equals( type ) ) {
            result = getSchemas( transaction, namePatterns );
        } else if ( DataVirtLexicon.Connection.NODE_TYPE.equals( type ) ) {
            result = getConnections( transaction, namePatterns );
        } else if ( DataVirtLexicon.DataService.NODE_TYPE.equals( type ) ) {
            result = getDataservices( transaction, namePatterns );
        } else if ( VdbLexicon.Vdb.VIRTUAL_DATABASE.equals( type ) ) {
            result = getVdbs( transaction, namePatterns );
        } else if ( VdbLexicon.Translator.TRANSLATOR.equals( type ) ) {
            result = getTranslators( transaction, namePatterns );
        } else if ( DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE.equals( type ) ) {
            result = getDrivers( transaction, namePatterns );
        } else if ( DataVirtLexicon.Template.NODE_TYPE.equals( type ) ) {
            result = getTemplates( transaction, namePatterns );
        } else {
            result = KomodoObject.EMPTY_ARRAY;
        }

        return result;
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

    @Override
    public boolean addListener( ExecutionConfigurationListener listener ) {
        return false;
    }

    @Override
    public void permitListeners( boolean enable ) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public void notifyListeners( ExecutionConfigurationEvent event ) {
        // TODO
        // Consider whether this is still required.
    }

    @Override
    public boolean removeListener( ExecutionConfigurationListener listener ) {
        return false;
    }

}
