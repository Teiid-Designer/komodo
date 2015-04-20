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
import org.komodo.relational.vdb.Mask;
import org.komodo.relational.vdb.Permission;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * An implementation of a VDB permission mask.
 */
public final class MaskImpl extends RelationalObjectImpl implements Mask {

    /**
     * The resolver of a {@link Mask}.
     */
    public static final TypeResolver< Mask > RESOLVER = new TypeResolver< Mask >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public Mask create( final UnitOfWork transaction,
                            final Repository repository,
                            final KomodoObject parent,
                            final String id,
                            final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( parent.getRepository() );
            final Permission parentPerm = adapter.adapt( transaction, parent, Permission.class );
            return RelationalModelFactory.createMask( transaction, parent.getRepository(), parentPerm, id );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#owningClass()
         */
        @Override
        public Class< MaskImpl > owningClass() {
            return MaskImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.internal.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) {
            try {
                ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, VdbLexicon.DataRole.Permission.Mask.MASK );
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
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Mask resolve( final UnitOfWork transaction,
                             final KomodoObject kobject ) throws KException {
            return new MaskImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
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
    public MaskImpl( final UnitOfWork uow,
                     final Repository repository,
                     final String workspacePath ) throws KException {
        super(uow, repository, workspacePath);
    }

    @Override
    public KomodoType getTypeIdentifier(UnitOfWork uow) {
        return KomodoType.VDB_MASK;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.vdb.Mask#getOrder(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getOrder( final UnitOfWork uow ) throws KException {
        return getObjectProperty(uow, PropertyValueType.STRING, "getOrder", VdbLexicon.DataRole.Permission.Mask.ORDER); //$NON-NLS-1$
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
            transaction = getRepository().createTransaction("maskimpl-getParent", true, null); //$NON-NLS-1$
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
     * @see org.komodo.relational.vdb.Mask#setOrder(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public void setOrder( final UnitOfWork uow,
                          final String newOrder ) throws KException {
        setObjectProperty(uow, "setOrder", VdbLexicon.DataRole.Permission.Mask.ORDER, newOrder); //$NON-NLS-1$
    }

}
