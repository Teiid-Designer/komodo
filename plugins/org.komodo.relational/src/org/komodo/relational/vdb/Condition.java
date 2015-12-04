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
import org.komodo.relational.vdb.internal.ConditionImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * Represents a VDB permission condition.
 */
public interface Condition extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Condition.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB_CONDITION;

    /**
     * The default value indicating if this condition is a constraint. Value is {@value} .
     */
    boolean DEFAULT_CONSTRAINT = true;

    /**
     * An empty array of conditions.
     */
    Condition[] NO_CONDITIONS = new Condition[0];

    /**
     * The resolver of a {@link Condition}.
     */
    public static final TypeResolver< Condition > RESOLVER = new TypeResolver< Condition >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#create(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.Repository, org.komodo.spi.repository.KomodoObject, java.lang.String,
         *      org.komodo.relational.RelationalProperties)
         */
        @Override
        public Condition create( final UnitOfWork transaction,
                                 final Repository repository,
                                 final KomodoObject parent,
                                 final String id,
                                 final RelationalProperties properties ) throws KException {
            final AdapterFactory adapter = new AdapterFactory( );
            final Permission parentPerm = adapter.adapt( transaction, parent, Permission.class );

            if ( parentPerm == null ) {
                throw new KException( Messages.getString( Relational.INVALID_PARENT_TYPE,
                                                          parent.getAbsolutePath(),
                                                          Condition.class.getSimpleName() ) );
            }

            return parentPerm.addCondition( transaction, id );
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
        public Class< ConditionImpl > owningClass() {
            return ConditionImpl.class;
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
                                            VdbLexicon.DataRole.Permission.Condition.CONDITION );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Condition resolve( final UnitOfWork transaction,
                                  final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Condition.TYPE_ID ) {
                return ( Condition )kobject;
            }

            return new ConditionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this condition is a constraint
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CONSTRAINT
     */
    boolean isConstraint( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newConstraint
     *        the new value for the <code>constraint</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CONSTRAINT
     */
    void setConstraint( final UnitOfWork transaction,
                        final boolean newConstraint ) throws KException;

}
