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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import org.komodo.core.KomodoLexicon;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.osgi.PluginService;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.connection.internal.ConnectionImpl;
import org.komodo.relational.RelationalModelFactory;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.dataservice.internal.DataserviceConveyor;
import org.komodo.relational.dataservice.internal.DataserviceImpl;
import org.komodo.relational.folder.Folder;
import org.komodo.relational.importer.connection.ConnectionImporter;
import org.komodo.relational.importer.ddl.DdlImporter;
import org.komodo.relational.importer.vdb.VdbImporter;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.model.internal.SchemaImpl;
import org.komodo.relational.resource.DdlFile;
import org.komodo.relational.resource.Driver;
import org.komodo.relational.resource.ResourceFile;
import org.komodo.relational.resource.UdfFile;
import org.komodo.relational.resource.internal.DriverImpl;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageReference;
import org.komodo.spi.storage.StorageService;
import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 *
 */
public class WorkspaceManager extends ObjectImpl implements RelationalObject {

    /**
     * The allowed child types.
     */
    private static final KomodoType[] CHILD_TYPES = new KomodoType[] { Connection.IDENTIFIER, Vdb.IDENTIFIER,
                                                                       Schema.IDENTIFIER, Teiid.IDENTIFIER,
                                                                       Dataservice.IDENTIFIER, Folder.IDENTIFIER };

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

    private static class CacheKey {
        private final Repository.Id repoId;

        private final String user;

        public CacheKey(Repository.Id repoId, String user) {
            this.repoId = repoId;
            this.user = user;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((repoId == null) ? 0 : repoId.hashCode());
            result = prime * result + ((user == null) ? 0 : user.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            CacheKey other = (CacheKey)obj;
            if (repoId == null) {
                if (other.repoId != null)
                    return false;
            } else if (!repoId.equals(other.repoId))
                return false;
            if (user == null) {
                if (other.user != null)
                    return false;
            } else if (!user.equals(other.user))
                return false;
            return true;
        }
    }

    private static class WskpMgrAdapter implements KeyFromValueAdapter< CacheKey, WorkspaceManager > {

        @Override
        public CacheKey getKey( WorkspaceManager value ) {
            Repository repository = value.getRepository();
            String user = value.getOwner();

            return new CacheKey(repository.getId(), user);
        }
    }

    private static KeyFromValueAdapter< CacheKey, WorkspaceManager > adapter = new WskpMgrAdapter();

    private static KeyInValueHashMap< CacheKey, WorkspaceManager > instances = 
                                                                        new KeyInValueHashMap< CacheKey, WorkspaceManager >(adapter);

    private final String owner;

    /**
     * @param repository
     *        the repository whose workspace manager is being requested (cannot be <code>null</code>)
     * @param transaction
     *        the transaction containing the user name of the owner of this workspace manager
     *        (if <code>null</code> then this manager is owner by the system user and has the workspace root as its path)
     *
     * @return the singleton instance for the given repository (never <code>null</code>)
     * @throws KException
     *         if there is an error obtaining the workspace manager
     */
    public static WorkspaceManager getInstance( Repository repository, UnitOfWork transaction) throws KException {
        boolean txNotProvided = transaction == null;

        if (txNotProvided)
            transaction = repository.createTransaction(Repository.SYSTEM_USER, "createWorkspaceManager", false, null ); //$NON-NLS-1$

        WorkspaceManager instance = instances.get(repository.getId());

        if ( instance == null ) {
            // We must create a transaction here so that it can be passed on to the constructor. Since the
            // node associated with the WorkspaceManager always exists we don't have to create it.
            instance = new WorkspaceManager(repository, transaction);

            if (txNotProvided)
                transaction.commit();

            instances.add( instance );
        }

        return instance;
    }

    /**
     * @return the owner of this workspace manager
     */
    public String getOwner() {
        return this.owner;
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
    public static void uncacheInstance(final Repository repository, final String owner) {
        if (repository == null)
            return;

        instances.remove(new CacheKey(repository.getId(), owner));
    }

    private WorkspaceManager(Repository repository, UnitOfWork uow ) throws KException {
        super( repository, RepositoryImpl.komodoWorkspacePath(uow), 0 );
        this.owner = uow.getUserName();

        repository.addObserver(new RepositoryObserver() {

            @Override
            public void eventOccurred() {
                // Disposal observer
                if (getRepository() == null || State.NOT_REACHABLE == getRepository().getState() || !(getRepository().ping())) {
                    instances.remove(WorkspaceManager.this);
                }
            }

            @Override
            public void errorOccurred(Throwable e) {
                // Nothing to do
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
     *        the parent of the dataservice object being created (can be <code>null</code>)
     * @param serviceName
     *        the name of the dataservice to create (cannot be empty)
     * @return the Dataservice object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Dataservice createDataservice( final UnitOfWork uow,
                                        final KomodoObject parent,
                                        final String serviceName ) throws KException {
        final String path = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ).getAbsolutePath()
                                                 : parent.getAbsolutePath() );
         return RelationalModelFactory.createDataservice( uow, getRepository(), path, serviceName );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the connection object being created (can be <code>null</code>)
     * @param sourceName
     *        the name of the connection to create (cannot be empty)
     * @return the connection object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Connection createConnection( final UnitOfWork uow,
                                        final KomodoObject parent,
                                        final String sourceName ) throws KException {
        final String path = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ).getAbsolutePath()
                                                 : parent.getAbsolutePath() );
         return RelationalModelFactory.createConnection( uow, getRepository(), path, sourceName );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the DDL file object being created (can be <code>null</code>)
     * @param ddlFileName
     *        the name of the DDL file to create (cannot be empty)
     * @param content
     *        the file content (cannot be <code>null</code>)
     * @return the DDL file object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public DdlFile createDdlFile( final UnitOfWork uow,
                                  final KomodoObject parent,
                                  final String ddlFileName,
                                  final byte[] content ) throws KException {
        final KomodoObject kobjParent = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ) : parent );
        return RelationalModelFactory.createDdlFile( uow, getRepository(), kobjParent, ddlFileName, content );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the driver object being created (can be <code>null</code>)
     * @param driverName
     *        the name of the driver to create (cannot be empty)
     * @param content
     *        the file content (cannot be <code>null</code>)
     * @return the Driver object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Driver createDriver( UnitOfWork uow,
                                KomodoObject parent,
                                String driverName,
                                final byte[] content ) throws KException {
        parent = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ) : parent );
        return RelationalModelFactory.createDriver( uow, getRepository(), parent, driverName, content );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the driver object being created (can be <code>null</code>)
     * @param driverName
     *        the name of the driver to create (cannot be empty)
     * @return the Driver object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Driver createDriver( UnitOfWork uow,
                                KomodoObject parent,
                                String driverName) throws KException {
        parent = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ) : parent );
        return RelationalModelFactory.createDriver( uow, getRepository(), parent, driverName);
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the folder object being created (can be <code>null</code>)
     * @param folderName
     *        the name of the folder to create (cannot be empty)
     * @return the Folder object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Folder createFolder( final UnitOfWork uow,
                                final KomodoObject parent,
                                final String folderName ) throws KException {
        final String path = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ).getAbsolutePath()
            : parent.getAbsolutePath() );
        return RelationalModelFactory.createFolder( uow, getRepository(), path, folderName );
    }

    /**
     * @param uow
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the resource file object being created (can be <code>null</code>)
     * @param resourceFileName
     *        the name of the resource file to create (cannot be empty)
     * @param content
     *        the file content (cannot be <code>null</code>)
     * @return the resource file object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public ResourceFile createResourceFile( final UnitOfWork uow,
                                            final KomodoObject parent,
                                            final String resourceFileName,
                                            final byte[] content ) throws KException {
        final KomodoObject kobjParent = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ) : parent );
        return RelationalModelFactory.createResourceFile( uow, getRepository(), kobjParent, resourceFileName, content );
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
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent
     *        the parent of the UDF file object being created (can be <code>null</code>)
     * @param udfFileName
     *        the name of the UDF file to create (cannot be empty)
     * @param content
     *        the file content (cannot be <code>null</code>)
     * @return the resource file object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public UdfFile createUdfFile( final UnitOfWork uow,
                                  final KomodoObject parent,
                                  final String udfFileName,
                                  final byte[] content ) throws KException {
        final KomodoObject kobjParent = ( ( parent == null ) ? getRepository().komodoWorkspace( uow ) : parent );
        return RelationalModelFactory.createUdfFile( uow, getRepository(), kobjParent, udfFileName, content );
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
     * @param includeSubTypes
     *        determines whether sub types are included in the return
     * @return the paths of all the objects under the specified parent path with the specified type (never <code>null</code> but
     *         can be empty)
     * @throws KException
     *         if an error occurs
     */
    public String[] findByType( final UnitOfWork transaction,
                                final String type,
                                String parentPath,
                                final String namePattern,
                                boolean includeSubTypes) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state must be NOT_STARTED and was " + transaction.getState() ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( type, "type" ); //$NON-NLS-1$

        if ( StringUtils.isBlank( parentPath ) ) {
            parentPath = RepositoryImpl.komodoWorkspacePath(transaction);
        }

        try {
            String queryText = null;

            if ( StringUtils.isBlank( namePattern ) ) {
                queryText = String.format( FIND_ALL_QUERY_PATTERN, type, parentPath );
            } else {
                queryText = String.format( FIND_MATCHING_QUERY_PATTERN, type, parentPath, namePattern );
            }

            final List< KomodoObject > kObjs = getRepository().query( transaction, queryText );
            List< KomodoObject > results = new ArrayList< > ();
            for( final KomodoObject kObj : kObjs ) {
                if(includeSubTypes) {
                    results.add(kObj);
                } else if ( type.equals(kObj.getPrimaryType(transaction).getName()) ) {
                    results.add(kObj);
                }
            }

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
                                final String type) throws KException {
        return findByType( transaction, type, RepositoryImpl.komodoWorkspacePath(transaction), null, false );
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> and must have a state of
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param type
     *        the lexicon node type name of objects being found
     * @param includeSubTypes
     *        determines whether sub types are included in the return
     * @return the paths of all the objects in the workspace with the specified type (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public String[] findByType( final UnitOfWork transaction,
                                final String type,
                                boolean includeSubTypes) throws KException {
        return findByType( transaction, type, RepositoryImpl.komodoWorkspacePath(transaction), null, includeSubTypes );
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
     * @return all {@link Dataservice}s in the workspace
     * @throws KException
     *         if an error occurs
     */
    public Dataservice[] findDataservices( UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] paths = findByType(transaction, DataVirtLexicon.DataService.NODE_TYPE);
        Dataservice[] result = null;

        if (paths.length == 0) {
            result = Dataservice.NO_DATASERVICES;
        } else {
            result = new Dataservice[paths.length];
            int i = 0;

            for (final String path : paths) {
                result[i++] = new DataserviceImpl(transaction, getRepository(), path);
            }
        }

        return result;
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return all {@link Connection}s in the workspace
     * @throws KException
     *         if an error occurs
     */
    public Connection[] findConnections( UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] paths = findByType(transaction, DataVirtLexicon.Connection.NODE_TYPE);
        Connection[] result = null;

        if (paths.length == 0) {
            result = Connection.NO_CONNECTIONS;
        } else {
            result = new Connection[paths.length];
            int i = 0;

            for (final String path : paths) {
                result[i++] = new ConnectionImpl(transaction, getRepository(), path);
            }
        }

        return result;
    }
    
    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @return all {@link Driver}s in the workspace
     * @throws KException
     *         if an error occurs
     */
    public Driver[] findDrivers( UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final String[] paths = findByType(transaction, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE);
        Driver[] result = null;

        if (paths.length == 0) {
            result = Driver.NO_DRIVERS;
        } else {
            result = new Driver[paths.length];
            int i = 0;

            for (final String path : paths) {
                result[i++] = new DriverImpl(transaction, getRepository(), path);
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

        final String[] paths = findByType(transaction, VdbLexicon.Vdb.VIRTUAL_DATABASE, false);
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
    public <T> T resolve( final UnitOfWork transaction,
                                                 final Object object,
                                                 final Class<T> resolvedClass ) throws KException {
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

    /**
    *
    * @param transaction
    *        the transaction (cannot be <code>null</code> or have a state that is not
    *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
    * @param parent the parent of the imported vdb
    * @param storageRef the reference to the destination within the storage
    * @return the import messages (never <code>null</code>)
    * @throws KException if error occurs
    */
    public ImportMessages importArtifact(final UnitOfWork transaction,
                                         final KomodoObject parent,
                                         StorageReference storageRef) throws KException {
        
        return importArtifact(transaction,parent,storageRef,new ImportOptions());
    }
    
    /**
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param parent the parent of the imported vdb
     * @param storageRef the reference to the destination within the storage
     * @param importOptions options for the import
     * @return the import messages (never <code>null</code>)
     * @throws KException if error occurs
     */
    public ImportMessages importArtifact(final UnitOfWork transaction,
                                         final KomodoObject parent, 
                                         StorageReference storageRef,
                                         ImportOptions importOptions) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(parent, "parent"); //$NON-NLS-1$
        ArgCheck.isNotNull(storageRef, "storageRef"); //$NON-NLS-1$

        StorageConnector connector = null;
        InputStream stream = null;
        try {
            StorageService storageService = PluginService.getInstance().getStorageService(storageRef.getStorageType());
            if (storageService == null)
                throw new KException(Messages.getString(Relational.STORAGE_TYPE_INVALID,
                                                        storageRef.getStorageType()));

            connector = storageService.getConnector(storageRef.getParameters());
            stream = connector.read(storageRef.getParameters());

            ImportMessages importMessages = new ImportMessages();

            if (DocumentType.VDB_XML.equals(storageRef.getDocumentType())) {
                VdbImporter importer = new VdbImporter(getRepository());
                importer.importVdb(transaction, stream, parent, importOptions, importMessages);
            }
            else if (DocumentType.CONNECTION.equals(storageRef.getDocumentType())) {
                ConnectionImporter importer = new ConnectionImporter(getRepository());
                importer.importDS(transaction, stream, parent, importOptions, importMessages);
            }
            else if (DocumentType.DDL.equals(storageRef.getDocumentType())) {
                DdlImporter importer = new DdlImporter(getRepository());
                importer.importDdl(transaction, stream, parent, importOptions, importMessages);
            }
            else if (DocumentType.ZIP.equals(storageRef.getDocumentType())) {
                DataserviceConveyor conveyor = new DataserviceConveyor(getRepository());
                conveyor.dsImport(transaction, stream, parent, importOptions, importMessages);
            }
            else if (DocumentType.JAR.equals(storageRef.getDocumentType())) {
                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                final byte[] buf = new byte[8192];
                int length;

                while ((length = stream.read(buf, 0, buf.length)) >= 0) {
                    bos.write(buf, 0, length);
                }

                byte[] content = bos.toByteArray();
                
                String driverName = storageRef.getParameters().getProperty(StorageReference.DRIVER_NAME_KEY);
                if(StringUtils.isBlank(driverName)) {
                    driverName = StorageReference.DRIVER_NAME_DEFAULT;
                }

                importOptions.setOption(OptionKeys.NAME, driverName);
                boolean doImport = DataserviceConveyor.handleExistingNode(transaction, parent, importOptions, importMessages);
                if (! doImport) {
                    // Handling existing node advises not to continue
                    return importMessages;
                }

                Driver driver = RelationalModelFactory.createDriver(transaction, getRepository(), parent, driverName);
                driver.setContent(transaction, content);
            }
            else {
                throw new KException(Messages.getString(Relational.STORAGE_DOCUMENT_TYPE_INVALID,
                                                        storageRef.getDocumentType()));
            }

            return importMessages;

        } catch (Exception e) {
            throw handleError(e);
        } finally {
            if (stream != null) {
                try {
                    stream.close();
                } catch (IOException e) {
                    // Nothing required
                }
            }

            if (connector != null) {
                connector.dispose();
            }
        }
    }

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not
     *        {@link org.komodo.spi.repository.Repository.UnitOfWork.State#NOT_STARTED})
     * @param artifact the vdb to be exported
     * @param storageType the type of storage to export to
     * @param parameters the parameters for the storage, appropriate to the storage type
     *
     * @return a path to the downloadable file if appropriate for the defined storage type.
     *                  Otherwise <code>null</code>
     * @throws KException if error occurs
     */
    public String exportArtifact(final UnitOfWork transaction, final Exportable artifact,
                                       final String storageType, final Properties parameters) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == org.komodo.spi.repository.Repository.UnitOfWork.State.NOT_STARTED ),
                         "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull(artifact, "artifact"); //$NON-NLS-1$

        try {
            StorageService storageService = PluginService.getInstance().getStorageService(storageType);
            if (storageService == null)
                throw new KException(Messages.getString(Relational.STORAGE_TYPE_INVALID, storageType));

            StorageConnector connector = storageService.getConnector(parameters);
            connector.write(artifact, transaction, parameters);
            return parameters.getProperty(StorageConnector.DOWNLOADABLE_PATH_PROPERTY);

        } catch (Exception e) {
            throw handleError(e);
        }
    }
}
