/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb;

import org.komodo.relational.Messages;
import org.komodo.relational.Messages.Relational;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.internal.AdapterFactory;
import org.komodo.relational.vdb.internal.MaskImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * Represents a VDB permission mask.
 */
public interface Mask extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Mask.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB_MASK;

    /**
     * An empty array of masks.
     */
    Mask[] NO_MASKS = new Mask[0];

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Permission getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link Mask}.
     */
    public static final TypeResolver< Mask > RESOLVER = new TypeResolver< Mask >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public Mask create( final UnitOfWork transaction,
                            final Repository repository,
                            final KomodoObject parent,
                            final String id,
                            final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( );
            final Permission parentPerm = adapter.adapt( transaction, parent, Permission.class );

            if ( parentPerm == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          Mask.class.getSimpleName() ) );
            }

            return parentPerm.addMask( transaction, id );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class< MaskImpl > owningClass() {
            return MaskImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction,
                                            kobject.getRepository(),
                                            kobject,
                                            VdbLexicon.DataRole.Permission.Mask.MASK );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Mask resolve( final UnitOfWork transaction,
                             final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Mask.TYPE_ID ) {
                return ( Mask )kobject;
            }

            return new MaskImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * A name used by Teiid to reference this VDB.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>order</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getOrder( final UnitOfWork transaction ) throws KException;

    /**
     * Sets the name used by Teiid to reference this VDB.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newOrder
     *        the new value of the <code>order</code> property
     * @throws KException
     *         if an error occurs
     */
    void setOrder( final UnitOfWork transaction,
                   final String newOrder ) throws KException;

}
