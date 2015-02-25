/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import org.komodo.relational.RelationalProperties;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.internal.TypeResolver;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.Permission;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a VDB permission condition.
 */
public final class ConditionImpl extends RelationalObjectImpl implements Condition {

    /**
     * The resolver of a {@link Condition}.
     */
    public static final TypeResolver RESOLVER = new TypeResolver() {

        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        @Override
        public Class<? extends KomodoObject> owningClass() {
            return ConditionImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final Repository repository,
                                   final KomodoObject kobject ) {
            try {
                ObjectImpl.validateType(transaction, repository, kobject, VdbLexicon.DataRole.Permission.Condition.CONDITION);
                return true;
            } catch (final Exception e) {
                // not resolvable
            }

            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Condition resolve( final UnitOfWork transaction,
                                  final Repository repository,
                                  final KomodoObject kobject ) throws KException {
            return new ConditionImpl(transaction, repository, kobject.getAbsolutePath());
        }

        @Override
        public Condition create(UnitOfWork transaction,
                                                      KomodoObject parent,
                                                      String id,
                                                      RelationalProperties properties) throws KException {
            AdapterFactory adapter = new AdapterFactory(parent.getRepository());
            Permission parentPerm = adapter.adapt(transaction, parent, Permission.class);
            return RelationalModelFactory.createCondition(transaction, parent.getRepository(), parentPerm, id);
        }

    };

    /**
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param workspacePath
     *        the workspace path (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    public ConditionImpl( final UnitOfWork uow,
                          final Repository repository,
                          final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return KomodoType.VDB_CONDITION;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.internal.RelationalObjectImpl#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public KomodoObject getParent( final UnitOfWork uow ) throws KException {
        UnitOfWork transaction = uow;

        if (transaction == null) {
            transaction = getRepository().createTransaction("conditionimpl-getParent", true, null); //$NON-NLS-1$
        }

        assert (transaction != null);

        try {
            final KomodoObject grouping = super.getParent(transaction);
            final KomodoObject result = resolveType(transaction, grouping.getParent(transaction));

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
     * @see org.komodo.spi.repository.KomodoObject#getTypeId()
     */
    @Override
    public int getTypeId() {
        return TYPE_ID;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Condition#isConstraint(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isConstraint( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, Property.ValueType.BOOLEAN, "isConstraint", //$NON-NLS-1$
                                 VdbLexicon.DataRole.Permission.Condition.CONSTRAINT);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Condition#setConstraint(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setConstraint( final UnitOfWork uow,
                               final boolean newConstraint ) throws KException {
        setObjectProperty(uow, "setConstraint", VdbLexicon.DataRole.Permission.Condition.CONSTRAINT, newConstraint); //$NON-NLS-1$
    }

}
