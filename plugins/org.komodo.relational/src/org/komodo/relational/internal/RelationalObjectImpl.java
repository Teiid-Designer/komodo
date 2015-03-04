/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal;

import org.komodo.relational.model.RelationalObject;
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

    private static TypeResolverRegistry _resolverRegistry;

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
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("relationalobjectimpl-getChildren", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            KomodoObject[] result = null;
            final KomodoObject[] kids = super.getChildren(transaction, name);

            if (kids.length == 0) {
                result = kids;
            } else {
                result = new KomodoObject[kids.length];
                int i = 0;

                for (final KomodoObject kobject : kids) {
                    result[i++] = resolveType(transaction, kobject);
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
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getChildrenOfType(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public KomodoObject[] getChildrenOfType( final UnitOfWork uow,
                                             final String type ) throws KException {
        UnitOfWork transaction = uow;

        if (uow == null) {
            transaction = getRepository().createTransaction("relationalobjectimpl-getChildrenOfType", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            KomodoObject[] result = null;
            final KomodoObject[] kobjects = super.getChildrenOfType(uow, type);

            if (kobjects.length == 0) {
                result = kobjects;
            } else {
                result = new KomodoObject[kobjects.length];
                int i = 0;

                for (final KomodoObject kobject : kobjects) {
                    result[i++] = resolveType(uow, kobject);
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
     * {@inheritDoc}
     *
     * @see org.komodo.repository.ObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("relationalobjectimpl-getParent", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            KomodoObject result = super.getParent(uow);

            if (result != null) {
                result = resolveType(uow, result);
            }

            return result;
        } catch (final Exception e) {
            throw handleError(uow, transaction, e);
        }
    }

    private TypeResolverRegistry getResolverRegistry() {
        if (_resolverRegistry == null)
            _resolverRegistry = TypeResolverRegistry.getInstance();

        return _resolverRegistry;
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
        TypeResolver resolver = getResolverRegistry().getResolver(kobject.getTypeIdentifier(transaction));
        if (resolver != null && resolver.resolvable(transaction, kobject))
            return resolver.resolve(transaction, kobject);

        // Failed with the type identifier so try to be safe than sorry
        // and iterate through all resolvers to check this object is really
        // not resolvable.
        for (final TypeResolver aResolver : getResolverRegistry().getResolvers()) {
            if (aResolver.resolvable(transaction, kobject)) {
                return aResolver.resolve(transaction, kobject);
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
        final TypeResolver resolver = getResolverRegistry().getResolver(kobject.getClass());

        if ((resolver != null) && !resolver.resolvable(transaction, kobject)) {
            throw new KException(Messages.getString(Komodo.INCORRECT_TYPE,
                                                    kobject.getAbsolutePath(),
                                                    kobject.getClass().getSimpleName()));
        }
    }

}
