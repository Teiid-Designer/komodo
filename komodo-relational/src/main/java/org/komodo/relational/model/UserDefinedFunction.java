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
import org.komodo.relational.model.internal.UserDefinedFunctionImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateProcedure;

/**
 * Represents a user-defined function (CREATE VIRTUAL FUNCTION).
 */
public interface UserDefinedFunction extends Function {

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.USER_DEFINED_FUNCTION;

    /**
     * An empty array of UDFs.
     */
    UserDefinedFunction[] NO_UDFS = new UserDefinedFunction[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = UserDefinedFunction.class.hashCode();

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Model getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link UserDefinedFunction}.
     */
    TypeResolver< UserDefinedFunction > RESOLVER = new TypeResolver< UserDefinedFunction >() {

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
        public Class< UserDefinedFunctionImpl > owningClass() {
            return UserDefinedFunctionImpl.class;
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
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateProcedure.FUNCTION_STATEMENT )
                   && ObjectImpl.validatePropertyValue( transaction,
                                                        kobject.getRepository(),
                                                        kobject,
                                                        org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.SchemaElement.TYPE,
                                                        SchemaElementType.VIRTUAL.name() );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public UserDefinedFunction resolve( final UnitOfWork transaction,
                                            final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == UserDefinedFunction.TYPE_ID ) {
                return ( UserDefinedFunction )kobject;
            }

            return new UserDefinedFunctionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>category</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getCategory( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>Java class name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getJavaClass( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>Java method name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getJavaMethod( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newCategory
     *        the new value of the <code>category</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setCategory( final UnitOfWork transaction,
                      final String newCategory ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newJavaClass
     *        the new value of the <code>Java class name</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setJavaClass( final UnitOfWork transaction,
                       final String newJavaClass ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newJavaMethod
     *        the new value of the <code>Java method name</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setJavaMethod( final UnitOfWork transaction,
                        final String newJavaMethod ) throws KException;

}
