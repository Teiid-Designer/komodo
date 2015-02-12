/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal;

import java.util.HashMap;
import java.util.Map;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.internal.AccessPatternImpl;
import org.komodo.relational.model.internal.ColumnImpl;
import org.komodo.relational.model.internal.ForeignKeyImpl;
import org.komodo.relational.model.internal.FunctionImpl;
import org.komodo.relational.model.internal.IndexImpl;
import org.komodo.relational.model.internal.ModelImpl;
import org.komodo.relational.model.internal.ParameterImpl;
import org.komodo.relational.model.internal.PrimaryKeyImpl;
import org.komodo.relational.model.internal.ProcedureImpl;
import org.komodo.relational.model.internal.ProcedureResultSetImpl;
import org.komodo.relational.model.internal.SchemaImpl;
import org.komodo.relational.model.internal.StatementOptionImpl;
import org.komodo.relational.model.internal.TableImpl;
import org.komodo.relational.model.internal.UniqueConstraintImpl;
import org.komodo.relational.model.internal.ViewImpl;
import org.komodo.relational.teiid.internal.TeiidImpl;
import org.komodo.relational.vdb.internal.ConditionImpl;
import org.komodo.relational.vdb.internal.DataRoleImpl;
import org.komodo.relational.vdb.internal.EntryImpl;
import org.komodo.relational.vdb.internal.MaskImpl;
import org.komodo.relational.vdb.internal.PermissionImpl;
import org.komodo.relational.vdb.internal.TranslatorImpl;
import org.komodo.relational.vdb.internal.VdbImpl;
import org.komodo.relational.vdb.internal.VdbImportImpl;
import org.komodo.repository.Messages;
import org.komodo.repository.Messages.Komodo;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.KLog;

/**
 * A base implementation of a relational object.
 */
public abstract class RelationalObjectImpl extends ObjectImpl implements RelationalObject {

    private static Map< Class< ? extends RelationalObject >, TypeResolver > _resolvers;
    protected static final KLog LOGGER = KLog.getLogger();

    /**
     * Indicates if the initial state after construction should be validated.
     */
    public static final boolean VALIDATE_INITIAL_STATE = true;

    protected RelationalObjectImpl( final UnitOfWork uow,
                                    final Repository repository,
                                    final String path ) throws KException {
        this(uow, repository, path, 0);
    }

    protected RelationalObjectImpl( final UnitOfWork uow,
                                    final Repository repository,
                                    final String path,
                                    final int index ) throws KException {
        super(repository, path, index);
        internalValidateInitialState(uow, this);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChild(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject getChild( final UnitOfWork uow,
                                  final String name ) throws KException {
        final KomodoObject kobject = super.getChild(uow, name);
        return resolveType(uow, kobject);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildren(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject[] getChildren( final UnitOfWork uow,
                                       final String name ) throws KException {
        final KomodoObject[] kobjects = super.getChildren(uow, name);

        if (kobjects.length == 0) {
            return kobjects;
        }

        final KomodoObject[] result = new KomodoObject[kobjects.length];
        int i = 0;

        for (final KomodoObject kobject : kobjects) {
            result[i++] = resolveType(uow, kobject);
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                             final String type ) throws KException {
        final KomodoObject[] kobjects = super.getChildrenOfType(uow, type);

        if (kobjects.length == 0) {
            return kobjects;
        }

        final KomodoObject[] result = new KomodoObject[kobjects.length];
        int i = 0;

        for (final KomodoObject kobject : kobjects) {
            result[i++] = resolveType(uow, kobject);
        }

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork uow ) throws KException {
        final KomodoObject kobject = super.getParent(uow);

        if (kobject == null) {
            return null;
        }

        return resolveType(uow, kobject);
    }

    private Map< Class< ? extends RelationalObject >, TypeResolver > getResolvers() {
        if (_resolvers == null) {
            _resolvers = new HashMap<>();

            // Relational
            _resolvers.put(AccessPatternImpl.class, AccessPatternImpl.RESOLVER);
            _resolvers.put(ColumnImpl.class, ColumnImpl.RESOLVER);
            _resolvers.put(ForeignKeyImpl.class, ForeignKeyImpl.RESOLVER);
            _resolvers.put(FunctionImpl.class, FunctionImpl.RESOLVER);
            _resolvers.put(IndexImpl.class, IndexImpl.RESOLVER);
            _resolvers.put(ModelImpl.class, ModelImpl.RESOLVER);
            _resolvers.put(ParameterImpl.class, ParameterImpl.RESOLVER);
            _resolvers.put(PrimaryKeyImpl.class, PrimaryKeyImpl.RESOLVER);
            _resolvers.put(ProcedureImpl.class, ProcedureImpl.RESOLVER);
            _resolvers.put(ProcedureResultSetImpl.class, ProcedureResultSetImpl.RESOLVER);
            _resolvers.put(SchemaImpl.class, SchemaImpl.RESOLVER);
            _resolvers.put(StatementOptionImpl.class, StatementOptionImpl.RESOLVER);
            _resolvers.put(TableImpl.class, TableImpl.RESOLVER);
            _resolvers.put(UniqueConstraintImpl.class, UniqueConstraintImpl.RESOLVER);
            _resolvers.put(ViewImpl.class, ViewImpl.RESOLVER);

            // VDB
            _resolvers.put(ConditionImpl.class, ConditionImpl.RESOLVER);
            _resolvers.put(DataRoleImpl.class, DataRoleImpl.RESOLVER);
            _resolvers.put(EntryImpl.class, EntryImpl.RESOLVER);
            _resolvers.put(MaskImpl.class, MaskImpl.RESOLVER);
            _resolvers.put(PermissionImpl.class, PermissionImpl.RESOLVER);
            _resolvers.put(TranslatorImpl.class, TranslatorImpl.RESOLVER);
            _resolvers.put(VdbImpl.class, VdbImpl.RESOLVER);
            _resolvers.put(VdbImportImpl.class, VdbImportImpl.RESOLVER);

            // Teiid
            _resolvers.put(TeiidImpl.class, TeiidImpl.RESOLVER);
        }

        return _resolvers;
    }

    private final void internalValidateInitialState( final UnitOfWork uow,
                                                     final KomodoObject kobject ) throws KException {
        if (VALIDATE_INITIAL_STATE) {
            UnitOfWork transaction = uow;

            if (transaction == null) {
                transaction = getRepository().createTransaction(getClass().getSimpleName() + "-internalValidateInitialState", //$NON-NLS-1$
                                                                true,
                                                                null);
            }

            assert (transaction != null);

            validateInitialState(transaction, kobject);

            if (uow == null) {
                transaction.commit();
            }
        }
    }

    protected KomodoObject resolveType( final UnitOfWork transaction,
                                      final KomodoObject kobject ) throws KException {
        final Repository repository = getRepository();

        for (final TypeResolver resolver : getResolvers().values()) {
            if (resolver.resolvable(transaction, repository, kobject)) {
                return resolver.resolve(transaction, repository, kobject);
            }
        }

        return kobject;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#setPrimaryType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     * @throws UnsupportedOperationException
     *         if called
     */
    @Override
    public final void setPrimaryType( final UnitOfWork uow,
                                      final String typeName ) {
        throw new UnsupportedOperationException();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#toString()
     */
    @Override
    public String toString() {
        return getAbsolutePath();
    }

    /**
     * @param uow
     *        the rollback only transaction (never <code>null</code>)
     * @param kobject
     *        the object being checked (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    protected void validateInitialState( final UnitOfWork transaction,
                                         final KomodoObject kobject ) throws KException {
        final TypeResolver resolver = getResolvers().get(kobject.getClass());

        if ((resolver != null) && !resolver.resolvable(transaction, getRepository(), kobject)) {
            throw new KException(Messages.getString(Komodo.INCORRECT_TYPE,
                                                    kobject.getAbsolutePath(),
                                                    kobject.getClass().getSimpleName()));
        }
    }

}
