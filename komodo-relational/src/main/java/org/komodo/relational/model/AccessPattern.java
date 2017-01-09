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
import org.komodo.relational.model.internal.AccessPatternImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.Constraint;


/**
 * Represents a relational model access pattern.
 */
public interface AccessPattern extends TableConstraint {

    /**
     * The type identifier.
     */
    int TYPE_ID = AccessPattern.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.ACCESS_PATTERN;

    /**
     * The constraint type for an access pattern. Value is {@value} .
     */
    ConstraintType CONSTRAINT_TYPE = ConstraintType.ACCESS_PATTERN;

    /**
     * An empty collection of access pattern constraints.
     */
    AccessPattern[] NO_ACCESS_PATTERNS = new AccessPattern[0];

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Table getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link AccessPattern}.
     */
    TypeResolver< AccessPattern > RESOLVER = new TypeResolver< AccessPattern >() {

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
        public Class< AccessPatternImpl > owningClass() {
            return AccessPatternImpl.class;
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
            final Repository repository = kobject.getRepository();

            return ObjectImpl.validateType( transaction, repository, kobject, Constraint.TABLE_ELEMENT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        repository,
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
        public AccessPattern resolve( final UnitOfWork transaction,
                                      final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == AccessPattern.TYPE_ID ) {
                return ( AccessPattern )kobject;
            }

            return new AccessPatternImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }
    };
}
