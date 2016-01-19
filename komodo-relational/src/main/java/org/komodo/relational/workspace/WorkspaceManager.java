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

import java.util.List;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.RelationalProperty;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.datasource.Datasource;
import org.komodo.relational.datasource.internal.DatasourceImpl;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.TypeResolverRegistry;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.model.internal.SchemaImpl;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.Id;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 *
 */
public class WorkspaceManager extends ObjectImpl implements RelationalObject {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { Datasource.IDENTIFIER, Vdb.IDENTIFIER, 
                                                                       Schema.IDENTIFIER, Teiid.IDENTIFIER };

    /**
     * The type identifier.
     */
    public static final int TYPE_ID = WorkspaceManager.class.hashCode();

    // @formatter:off
    private static final String FIND_ALL_QUERY_PATTERN = "SELECT [jcr:path] FROM [%s]" //$NON-NLS-1$
                                                         + " WHERE ISDESCENDANTNODE('%s')" //$NON-NLS-1$
                                                         + " ORDER BY [jcr:path] ASC"; //$NON-NLS-1$

    private static final String FIND_MATCHING_QUERY_PATTERN = "SELECT [jcr:path] FROM [%s]"  //$NON-NLS-1$
                                                              + " WHERE ISDESCENDANTNODE('%s')" //$NON-NLS-1$
                                                              + " AND [jcr:name] LIKE '%s'" //$NON-NLS-1$
                                                              + " ORDER BY [jcr:path] ASC"; //$NON-NLS-1$
    // @formatter:on

    private static class WskpMgrAdapter implements KeyFromValueAdapter< Repository.Id, WorkspaceManager > {

        @Override
        public Id getKey( WorkspaceManager value ) {
            Repository repository = value.getRepository();
            return repository.getId();
        }
    }

    private static KeyFromValueAdapter< Repository.Id, WorkspaceManager > adapter = new WskpMgrAdapter();

    private static KeyInValueHashMap< Repository.Id, WorkspaceManager > instances = new KeyInValueHashMap< Repository.Id, WorkspaceManager >(
                                                                                                                                             adapter);

    /**
     * @param repository
     *        the repository whose workspace manager is being requested (cannot be <code>null</code>)
     * @return the singleton instance for the given repository (never <code>null</code>)
     * @throws KException
     *         if there is an error obtaining the workspace manager
     */
    public static WorkspaceManager getInstance( Repository repository ) throws KException {
        WorkspaceManager instance = instances.get(repository.getId());

        if ( instance == null ) {
            // We must create a transaction here so that it can be passed on to the constructor. Since the
            // node associated with the WorkspaceManager always exists we don't have to create it.
            final UnitOfWork uow = repository.createTransaction( "createWorkspaceManager", false, null ); //$NON-NLS-1$
            instance = new WorkspaceManager( uow, repository );
            uow.commit();

            instances.add( instance );
        }

        return instance;
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
     * @see org.komodo.relational.RelationalObject#getFilters()
     */
    @Override
    public Filter[] getFilters() {
        return RelationalObject.NO_FILTERS;
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
        return KomodoType.WORKSPACE;
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

    private WorkspaceManager(UnitOfWork uow, Repository repository ) throws KException {
        super( repository, RepositoryImpl.WORKSPACE_ROOT, 0 );

        repository.addObserver(new RepositoryObserver() {

            @Override
            public void eventOccurred() {
                // Disposal observer
                if (getRepository() == null || State.NOT_REACHABLE == getRepository().getState() || !(getRepository().ping())) {
                    instances.remove(WorkspaceManager.this);
                }
            }
        });
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param vdb
     *        the parent of the model object being created (cannot be <code>null</code>)
     * @param modelName
     *        the name of the model to create (cannot be empty)
     * @return the model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Model createModel( final UnitOfWork uow,
                              final Vdb vdb,
                              final String modelName ) throws KException {
        return RelationalModelFactory.createModel( uow, getRepository(), vdb, modelName );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the datasource object being created (can be <code>null</code>)
     * @param sourceName
     *        the name of the datasource to create (cannot be empty)
     * @return the Datasource object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Datasource createDatasource( final UnitOfWork uow,
                                        final KomodoObject parent,
                                        final String sourceName ) throws KException {
        final String path = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ).getAbsolutePath()
                                                 : parent.getAbsolutePath() );
         return RelationalModelFactory.createDatasource( uow, getRepository(), path, sourceName );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the schema object being created (can be <code>null</code>)
     * @param schemaName
     *        the name of the schema to create (cannot be empty)
     * @return the schema object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Schema createSchema( final UnitOfWork uow,
                                final KomodoObject parent,
                                final String schemaName ) throws KException {
        final String path = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ).getAbsolutePath()
                                                 : parent.getAbsolutePath() );
         return RelationalModelFactory.createSchema( uow, getRepository(), path, schemaName );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the teiid object being created (cannot be <code>null</code>)
     * @param id
     *        the id of the teiid instance (cannot be empty)
     * @return the teiid object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Teiid createTeiid( final UnitOfWork uow,
                              final KomodoObject parent,
                              final String id ) throws KException {
        final String path = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ).getAbsolutePath()
                                                : parent.getAbsolutePath() );
        return RelationalModelFactory.createTeiid( uow, getRepository(), path, id );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the model object being created (can be <code>null</code> if VDB should be created at the workspace
     *        root)
     * @param vdbName
     *        the name of the VDB to create (cannot be empty)
     * @param externalFilePath
     *        the VDB file path on the local file system (cannot be empty)
     * @return the VDB (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Vdb createVdb( final UnitOfWork uow,
                          final KomodoObject parent,
                          final String vdbName,
                          final String externalFilePath ) throws KException {
        final String path = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ).getAbsolutePath()
                                                : parent.getAbsolutePath() );
        return RelationalModelFactory.createVdb( uow, getRepository(), path, vdbName, externalFilePath );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the new object (cannot be <code>null</code>)
     * @param id
     *        the identifier of the object (cannot be <code>null</code>)
     * @param type
     *        the type of the object (cannot be <code>null</code>)
     * @param properties
     *        any additional properties required for construction
     *
     * @return new object
     * @throws KException if an error occurs
     */
    public KomodoObject create( UnitOfWork transaction,
                                KomodoObject parent,
                                String id,
                                KomodoType type,
                                RelationalProperty... properties ) throws KException {

        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( id, "id" ); //$NON-NLS-1$
        ArgCheck.isNotNull( type );

        RelationalProperties relProperties = new RelationalProperties();
        if ( ( properties != null ) && ( properties.length != 0 ) ) {
            for ( RelationalProperty property : properties ) {
                relProperties.add( property );
            }
        }

        TypeResolverRegistry registry = TypeResolverRegistry.getInstance();
        TypeResolver< ? > resolver = registry.getResolver( type );
        if ( resolver == null ) {
            if ( parent == null ) {
                return getRepository().komodoWorkspace( transaction ).addChild( transaction, id, JcrConstants.NT_UNSTRUCTURED );
            }

            return parent.addChild( transaction, id, JcrConstants.NT_UNSTRUCTURED );
        }

        return resolver.create( transaction, getRepository(), parent, id, relProperties );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param kobjects
     *        the object(s) being deleted (cannot be <code>null</code>, empty, or have a <code>null</code> element)
     * @throws KException
     *         if an error occurs or if an object does not exist
     */
    public void delete( final UnitOfWork transaction,
                        final KomodoObject... kobjects ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( kobjects, "kobjects" ); //$NON-NLS-1$

        for ( final KomodoObject kobject : kobjects ) {
            ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$
            validateWorkspaceMember( transaction, kobject );
            kobject.remove( transaction );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param type
     *        the lexicon node type name of objects being found (cannot be empty)
     * @param parentPath
     *        the parent path whose children recursively will be checked (can be empty if searching from the workspace root)
     * @param namePattern
     *        the regex used to match object names (can be empty if all objects of the given type are being requested)
     * @return the paths of all the objects under the specified parent path with the specified type (never <code>null</code> but
     *         can be empty)
     * @throws KException
     *         if an error occurs
     */
    public String[] findByType( final UnitOfWork transaction,
                                final String type,
                                String parentPath,
                                final String namePattern ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state must be NOT_STARTED and was " + transaction.getState() ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$

        if ( StringUtils.isBlank( parentPath ) ) {
            parentPath = RepositoryImpl.WORKSPACE_ROOT;
        }

        try {
            String queryText = null;

            if ( StringUtils.isBlank( namePattern ) ) {
                queryText = String.format( FIND_ALL_QUERY_PATTERN, type, parentPath );
            } else {
                queryText = String.format( FIND_MATCHING_QUERY_PATTERN, type, parentPath, namePattern );
            }

            final List< KomodoObject > results = getRepository().query( transaction, queryText );
            final int numPaths = results.size();

            if ( numPaths == 0 ) {
                return StringConstants.EMPTY_ARRAY;
            }

            final String[] result = new String[ numPaths ];
            int i = 0;

            for ( final KomodoObject kObject : results ) {
                result[ i++ ] = kObject.getAbsolutePath();
            }

            return result;
        } catch ( final Exception e ) {
            throw handleError( e );
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param type
     *        the lexicon node type name of objects being found
     * @return the paths of all the objects in the workspace with the specified type (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public String[] findByType( final UnitOfWork transaction,
                                final String type ) throws KException {
        return findByType( transaction, type, RepositoryImpl.WORKSPACE_ROOT, null );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return all {@link Model}s in the workspace (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public Model[] findModels( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] paths = findByType(transaction, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        Model[] result = null;

        if (paths.length == 0) {
            result = Model.NO_MODELS;
        } else {
            result = new Model[paths.length];
            int i = 0;

            for (final String path : paths) {
                result[i++] = new ModelImpl(transaction, getRepository(), path);
            }
        }

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return all {@link Model}s in the workspace (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public Schema[] findSchemas( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] paths = findByType(transaction, KomodoLexicon.Schema.NODE_TYPE);
        Schema[] result = null;

        if (paths.length == 0) {
            result = Schema.NO_SCHEMAS;
        } else {
            result = new Schema[paths.length];
            int i = 0;

            for (final String path : paths) {
                result[i++] = new SchemaImpl(transaction, getRepository(), path);
            }
        }

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return all {@link Datasource}s in the workspace
     * @throws KException
     *         if an error occurs
     */
    public Datasource[] findDatasources( UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] paths = findByType(transaction, KomodoLexicon.DataSource.NODE_TYPE);
        Datasource[] result = null;

        if (paths.length == 0) {
            result = Datasource.NO_DATASOURCES;
        } else {
            result = new Datasource[paths.length];
            int i = 0;

            for (final String path : paths) {
                result[i++] = new DatasourceImpl(transaction, getRepository(), path);
            }
        }

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return all {@link Teiid}s in the workspace
     * @throws KException
     *         if an error occurs
     */
    public Teiid[] findTeiids( UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] paths = findByType(transaction, KomodoLexicon.Teiid.NODE_TYPE);
        Teiid[] result = null;

        if (paths.length == 0) {
            result = Teiid.NO_TEIIDS;
        } else {
            result = new Teiid[paths.length];
            int i = 0;

            for (final String path : paths) {
                result[i++] = new TeiidImpl(transaction, getRepository(), path);
            }
        }

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return all {@link Vdb}s in the workspace (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public Vdb[] findVdbs( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] paths = findByType(transaction, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        Vdb[] result = null;

        if (paths.length == 0) {
            result = Vdb.NO_VDBS;
        } else {
            result = new Vdb[paths.length];
            int i = 0;

            for (final String path : paths) {
                result[i++] = new VdbImpl(transaction, getRepository(), path);
            }
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#remove(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void remove( final UnitOfWork transaction ) {
        throw new UnsupportedOperationException( Messages.getString( Relational.REMOVE_NOT_ALLOWED, getAbsolutePath() ) );
    }

    /**
     * <strong><em>Rename is not allowed!!</em></strong>
     *
     * @see org.komodo.spi.repository.KomodoObject#rename(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     * @throws UnsupportedOperationException if called
     */
    @Override
    public final void rename( final UnitOfWork transaction,
                              final String newName ) throws UnsupportedOperationException {
        throw new UnsupportedOperationException( Messages.getString( Relational.RENAME_NOT_ALLOWED, getAbsolutePath() ) );
    }

    /**
     * Attempts to adapt the given object to a relational model typed class.
     * If the object is not an instance of {@link KomodoObject} then null is
     * returned.
     *
     * The type id of the {@link KomodoObject} is extracted and the correct
     * relational model object created. If the latter is not assignable from the
     * given adapted class then it is concluded the adaption should fail and
     * null is returned, otherwise the new object is returned.
     *
     * @param <T>
     *        the desired outcome class
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param object
     *        the object being resolved
     * @param resolvedClass
     *        the class the object should be resolved to (cannot be <code>null</code>)
     * @return the strong typed object of the desired type (can be <code>null</code> if not resolvable)
     * @throws KException
     *         if a resolver could not be found or if an error occurred
     */
    public < T extends KomodoObject > T resolve( final UnitOfWork transaction,
                                                 final Object object,
                                                 final Class< T > resolvedClass ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        AdapterFactory adapter = new AdapterFactory( );
        T kobject = adapter.adapt(transaction, object, resolvedClass);
        return kobject;
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

    private void validateWorkspaceMember( final UnitOfWork uow,
                                          final KomodoObject kobject ) throws KException {
        if (!getRepository().equals(kobject.getRepository())) {
            throw new KException(Messages.getString(Relational.OBJECT_BEING_DELETED_HAS_WRONG_REPOSITORY,
                                                    kobject.getAbsolutePath(),
                                                    kobject.getRepository().getId().getUrl(),
                                                    getRepository().getId().getUrl()));
        }

        if (!kobject.getAbsolutePath().startsWith(getRepository().komodoWorkspace(uow).getAbsolutePath())) {
            throw new KException(Messages.getString(Relational.OBJECT_BEING_DELETED_HAS_NULL_PARENT, kobject.getAbsolutePath()));
        }
    }

}
