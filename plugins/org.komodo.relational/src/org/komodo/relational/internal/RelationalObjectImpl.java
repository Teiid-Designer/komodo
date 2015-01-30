/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.internal;

import org.komodo.relational.model.RelationalObject;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.utils.KLog;

/**
 * A base implementation of a relational object.
 */
public abstract class RelationalObjectImpl extends ObjectImpl implements RelationalObject {

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
        internalValidateInitialState(uow, path);
    }

    private final void internalValidateInitialState( final UnitOfWork uow,
                                                     final String path ) throws KException {
        if (VALIDATE_INITIAL_STATE) {
            UnitOfWork transaction = uow;

            if (transaction == null) {
                transaction = getRepository().createTransaction(getClass().getSimpleName() + "-internalValidateInitialState", //$NON-NLS-1$
                                                                true,
                                                                null);
            }

            assert (transaction != null);

            validateInitialState(transaction, path);

            if (uow == null) {
                transaction.commit();
            }
        }
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
     * @param path
     *        the workspace path (never empty)
     * @throws KException
     *         if an error occurs
     */
    protected abstract void validateInitialState( final UnitOfWork uow,
                                                  final String path ) throws KException;

}
