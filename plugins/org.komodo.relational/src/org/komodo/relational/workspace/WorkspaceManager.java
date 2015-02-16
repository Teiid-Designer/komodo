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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.jcr.Session;
import javax.jcr.query.Query;
import javax.jcr.query.QueryManager;
import javax.jcr.query.QueryResult;
import javax.jcr.query.Row;
import javax.jcr.query.RowIterator;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.model.internal.SchemaImpl;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.repository.LocalRepository;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.Id;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;
import org.komodo.utils.ArgCheck;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 *
 */
public class WorkspaceManager {

    private static final String FIND_QUERY_PATTERN = "SELECT [jcr:path] FROM [%s] WHERE ISDESCENDANTNODE('" //$NON-NLS-1$
                                                     + RepositoryImpl.WORKSPACE_ROOT + "') ORDER BY [jcr:name] ASC"; //$NON-NLS-1$

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

    private final Repository repository;

    /**
     * @param repository
     *        the repository
     * @return singleton instance for the given repository
     */
    public static WorkspaceManager getInstance( Repository repository ) {
        WorkspaceManager instance = instances.get(repository.getId());
        if (instance == null) {
            instance = new WorkspaceManager(repository);
            instances.add(instance);
        }

        return instance;
    }

    /**
     * Primarily used in tests to remove the workspace manager instance
     * from the instances cache.
     *
     * @param repository remove instance with given repository
     */
    public static void uncacheInstance(LocalRepository repository) {
        if (repository == null)
            return;

        instances.remove(repository.getId());
    }

    private WorkspaceManager( Repository repository ) {
        this.repository = repository;
        this.repository.addObserver(new RepositoryObserver() {

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
     * @return the repository
     */
    public Repository getRepository() {
        return this.repository;
    }

    /**
     * Only one of the {@link UnitOfWork transactions} passed in should be non-<code>null</code>. Ensures that a transaction
     * rollback occurs if the transaction was constructed within the method.
     *
     * @param transactionParameter
     *        the transaction passed into the method (can be <code>null</code>)
     * @param transactionVariable
     *        the transaction constructed within the method (can be <code>null</code>)
     * @param e
     *        the error being handled (cannot be <code>null</code>)
     * @return the error passed in if already a {@link KException} or the error passed in wrapped in a {@link KException}
     */
    private static KException handleError( final UnitOfWork transactionParameter,
                                           final UnitOfWork transactionVariable,
                                           final Exception e ) {
        assert (e != null);
        assert ((transactionParameter == null) && (transactionVariable != null))
               || ((transactionParameter != null) && (transactionVariable == null))
               || ((transactionParameter == transactionVariable));

        if (transactionParameter == null) {
            transactionVariable.rollback();
        }

        if (e instanceof KException) {
            return (KException)e;
        }

        return new KException(e);
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param parent
     *        the parent of the model object being created (can be <code>null</code> if creating at workspace root)
     * @param modelName
     *        the name of the model to create (cannot be empty)
     * @return the model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Model createModel( UnitOfWork uow,
                              KomodoObject parent,
                              String modelName ) throws KException {
        ArgCheck.isNotEmpty(modelName, "modelName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-createModel", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String parentPath = null;

            if (parent == null) {
                parentPath = this.repository.komodoWorkspace(transaction).getAbsolutePath();
            } else {
                validateWorkspaceMember(transaction, parent);
                parentPath = parent.getAbsolutePath();
            }

            final KomodoObject kobject = getRepository().add(transaction,
                                                             parentPath,
                                                             modelName,
                                                             VdbLexicon.Vdb.DECLARATIVE_MODEL);
            final Model result =  new ModelImpl(transaction, getRepository(), kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param parent
     *        the parent of the schema object being created (cannot be <code>null</code>)
     * @param schemaName
     *        the name of the schema to create (cannot be empty)
     * @return the schema object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Schema createSchema( UnitOfWork uow,
                                KomodoObject parent,
                                String schemaName ) throws KException {
        ArgCheck.isNotNull(parent, "parent"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(schemaName, "schemaName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-createSchema", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = getRepository().add(transaction,
                                                             parent.getAbsolutePath(),
                                                             schemaName,
                                                             KomodoLexicon.Schema.NODE_TYPE);
            final Schema result = new SchemaImpl(transaction, getRepository(), kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param parent
     *        the parent of the teiid object being created (cannot be <code>null</code>)
     * @param id
     *        the id of the teiid instance (cannot be empty)
     * @return the teiid object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Teiid createTeiid( UnitOfWork uow,
                              KomodoObject parent,
                              String id ) throws KException {
        ArgCheck.isNotNull(parent, "parent"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(id, "id"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-createTeiid", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = getRepository().add(transaction,
                                                             parent.getAbsolutePath(),
                                                             id,
                                                             KomodoLexicon.Teiid.NODE_TYPE);
            final Teiid result = new TeiidImpl(transaction, getRepository(), kobject.getAbsolutePath());

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-createvdb", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            String parentPath = null;

            if (parent != null) {
                validateWorkspaceMember(transaction, parent);
                parentPath = parent.getAbsolutePath();
            }

            final Vdb result = RelationalModelFactory.createVdb(transaction,
                                                                this.repository,
                                                                parentPath,
                                                                vdbName,
                                                                externalFilePath);

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param kobjects
     *        the object(s) being deleted (cannot be <code>null</code>, empty, or have a <code>null</code> element)
     * @throws KException
     *         if an error occurs or if an object does not exist
     */
    public void delete( final UnitOfWork uow,
                        final KomodoObject... kobjects ) throws KException {
        ArgCheck.isNotEmpty(kobjects, "kobjects"); //$NON-NLS-1$
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-delete", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            for (final KomodoObject kobject : kobjects) {
                ArgCheck.isNotNull(kobject, "kobject"); //$NON-NLS-1$
                validateWorkspaceMember(uow, kobject);
                kobject.getParent(transaction).removeChild(transaction, kobject.getName(transaction));
            }

            if (uow == null) {
                transaction.commit();
            }
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    private String[] findByType( final UnitOfWork uow,
                                 final String type ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-findByType", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        String[] result = null;

        try {
            final String queryText = String.format(FIND_QUERY_PATTERN, type);
            final Session session = ((UnitOfWorkImpl)transaction).getSession();
            final QueryManager queryMgr = session.getWorkspace().getQueryManager();
            final Query query = queryMgr.createQuery(queryText, Query.JCR_SQL2);
            final QueryResult resultSet = query.execute();
            final RowIterator itr = resultSet.getRows();
            final String columnName = resultSet.getColumnNames()[0];
            int numPaths = (int)itr.getSize();

            if (numPaths == 0) {
                result = StringConstants.EMPTY_ARRAY;
            } else {
                result = new String[numPaths];
                int i = 0;

                while (itr.hasNext()) {
                    final Row row = itr.nextRow();
                    result[i++] = row.getValue(columnName).getString();
                }
            }

            if (uow == null) {
                transaction.commit();
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return all {@link Model}s in the workspace (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public Model[] findModels( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-findModels", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

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

        if (uow == null) {
            transaction.commit();
        }

        return result;
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return all {@link Model}s in the workspace (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public Schema[] findSchemas( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-findSchemas", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

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

        if (uow == null) {
            transaction.commit();
        }

        return result;
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return all {@link Teiid}s in the workspace
     * @throws KException
     *         if an error occurs
     */
    public List< Teiid > findTeiids( UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-findTeiids", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        final String[] paths = findByType(transaction, KomodoLexicon.Teiid.NODE_TYPE);
        List< Teiid > result = null;

        if (paths.length == 0) {
            result = Collections.emptyList();
        } else {
            result = new ArrayList<>(paths.length);

            for (final String path : paths) {
                result.add(new TeiidImpl(transaction, getRepository(), path));
            }
        }

        if (uow == null) {
            transaction.commit();
        }

        return result;
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @return all {@link Vdb}s in the workspace (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    public Vdb[] findVdbs( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-findVdbs", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

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

        if (uow == null) {
            transaction.commit();
        }

        return result;
    }

    private void validateWorkspaceMember( final UnitOfWork uow,
                                          final KomodoObject kobject ) throws KException {
        if (!this.repository.equals(kobject.getRepository())) {
            throw new KException(Messages.getString(Relational.OBJECT_BEING_DELETED_HAS_WRONG_REPOSITORY,
                                                    kobject.getAbsolutePath(),
                                                    kobject.getRepository().getId().getUrl(),
                                                    this.repository.getId().getUrl()));
        }

        if (!kobject.getAbsolutePath().startsWith(this.repository.komodoWorkspace(uow).getAbsolutePath())) {
            throw new KException(Messages.getString(Relational.OBJECT_BEING_DELETED_HAS_NULL_PARENT, kobject.getAbsolutePath()));
        }
    }

}
