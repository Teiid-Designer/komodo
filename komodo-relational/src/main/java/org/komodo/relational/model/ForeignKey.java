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
package org.komodo.relational.model;

import org.komodo.relational.TypeResolver;
import org.komodo.relational.model.internal.ForeignKeyImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.Constraint;

/**
 * Represents a relational model foreign key.
 */
public interface ForeignKey extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = ForeignKey.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.FOREIGN_KEY;

    /**
     * The constraint type for a foreign key. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.FOREIGN_KEY;

    /**
     * An empty collection of foreign key constraints.
     */
    ForeignKey[] NO_FOREIGN_KEYS = new ForeignKey[0];

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Table getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link ForeignKey}.
     */
    TypeResolver< ForeignKey > RESOLVER = new TypeResolver< ForeignKey >() {

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
        public Class< ForeignKeyImpl > owningClass() {
            return ForeignKeyImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, Constraint.FOREIGN_KEY_CONSTRAINT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        kobject.getRepository(),
                                                        kobject,
                                                        Constraint.TYPE,
                                                        CONSTRAINT_TYPE.toValue() );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public ForeignKey resolve( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == ForeignKey.TYPE_ID ) {
                return ( ForeignKey )kobject;
            }

            return new ForeignKeyImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newReferencesColumn
     *        the references table columns being added (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void addReferencesColumn( final UnitOfWork transaction,
                              final Column newReferencesColumn ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the columns referenced from the references table (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Column[] getReferencesColumns( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the references table (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Table getReferencesTable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param referencesColumnToRemove
     *        the references table column being removed (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void removeReferencesColumn( final UnitOfWork transaction,
                                 final Column referencesColumnToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newReferencesTable
     *        the new value for the references table (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setReferencesTable( final UnitOfWork transaction,
                             final Table newReferencesTable ) throws KException;

}
