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
import java.util.List;
import org.komodo.core.KomodoLexicon;
import org.komodo.relational.internal.model.ModelImpl;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.internal.SchemaImpl;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.Id;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;
import org.komodo.utils.ArgCheck;

/**
 *
 */
public class WorkspaceManager {

    private static class WskpMgrAdapter implements KeyFromValueAdapter<Repository.Id, WorkspaceManager> {

        @Override
        public Id getKey(WorkspaceManager value) {
            Repository repository = value.getRepository();
            return repository.getId();
        }
    }

    private static KeyFromValueAdapter<Repository.Id, WorkspaceManager> adapter = new WskpMgrAdapter();

    private static KeyInValueHashMap<Repository.Id, WorkspaceManager> instances = new KeyInValueHashMap<Repository.Id, WorkspaceManager>(adapter);

    private final Repository repository;

    /**
     * @param repository the repository
     * @return singleton instance for the given repository
     */
    public static WorkspaceManager getInstance(Repository repository) {
        WorkspaceManager instance = instances.get(repository.getId());
        if (instance == null) {
            instance = new WorkspaceManager(repository);
            instances.add(instance);
        }

        return instance;
    }

    private WorkspaceManager(Repository repository) {
        this.repository = repository;
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
        || ((transactionParameter != null) && (transactionVariable == null));

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
     *        the parent of the model object being created (cannot be <code>null</code>)
     * @param modelName
     *        the name of the model to create (cannot be empty)
     * @return the model object (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public Model createModel(UnitOfWork uow, KomodoObject parent, String modelName) throws KException {
        ArgCheck.isNotNull(parent, "parent"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(modelName, "modelName"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-createModel", false, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject kobject = getRepository().add(transaction,
                                                        parent.getAbsolutePath(),
                                                        modelName,
                                                        KomodoLexicon.VdbModel.NODE_TYPE);

            if (uow == null) {
                transaction.commit();
            }

            return new ModelImpl(getRepository(), kobject.getAbsolutePath());
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
    public Schema createSchema(UnitOfWork uow, KomodoObject parent, String schemaName) throws KException {
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

            if (uow == null) {
                transaction.commit();
            }

            return new SchemaImpl(getRepository(), kobject.getAbsolutePath());
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
    public Teiid createTeiid(UnitOfWork uow, KomodoObject parent, String id) throws KException {
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

            if (uow == null) {
                transaction.commit();
            }

            return new TeiidImpl(getRepository(), kobject.getAbsolutePath());
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
    * @param parent
     *        the parent to search under (cannot be <code>null</code>)
     * @return all {@link Teiid}s under the given parent
     * @throws KException
     *         if an error occurs
     */
    public List<Teiid> findTeiids(UnitOfWork uow, KomodoObject parent) throws KException {
        ArgCheck.isNotNull(parent, "parent"); //$NON-NLS-1$

        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("workspacemanager-findobjectsoftype", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        List<Teiid> children = new ArrayList<Teiid>();

        try {
            // TODO
            // Want to use search API to find the teiid instances but for now check the parent's children

            KomodoObject[] kChildren = parent.getChildrenOfType(transaction, KomodoLexicon.Teiid.NODE_TYPE);
            if (kChildren != null) {

                for (KomodoObject kobject : kChildren) {
                    children.add(new TeiidImpl(getRepository(), kobject.getAbsolutePath()));
                }
            }

            if (uow == null) {
                transaction.commit();
            }

        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }

        return children;
    }
}
