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
package org.komodo.relational.vdb;

import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.vdb.internal.PermissionImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 * Represents a VDB data policy permission.
 */
public interface Permission extends RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Permission.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.VDB_PERMISSION;

    /**
     * The default value indicating if this permission allows alter. Value is {@value} .
     */
    boolean DEFAULT_ALLOW_ALTER = false;

    /**
     * The default value indicating if this permission allows create. Value is {@value} .
     */
    boolean DEFAULT_ALLOW_CREATE = false;

    /**
     * The default value indicating if this permission allows delete. Value is {@value} .
     */
    boolean DEFAULT_ALLOW_DELETE = false;

    /**
     * The default value indicating if this permission allows execute. Value is {@value} .
     */
    boolean DEFAULT_ALLOW_EXECUTE = false;

    /**
     * The default value indicating if this permission allows language. Value is {@value} .
     */
    boolean DEFAULT_ALLOW_LANGUAGE = false;

    /**
     * The default value indicating if this permission allows read. Value is {@value} .
     */
    boolean DEFAULT_ALLOW_READ = false;

    /**
     * The default value indicating if this permission allows update. Value is {@value} .
     */
    boolean DEFAULT_ALLOW_UPDATE = false;

    /**
     * An empty array of permissions.
     */
    Permission[] NO_PERMISSIONS = new Permission[0];

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    DataRole getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link Permission}.
     */
    TypeResolver< Permission > RESOLVER = new TypeResolver< Permission >() {

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
        public Class< PermissionImpl > owningClass() {
            return PermissionImpl.class;
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
                                            VdbLexicon.DataRole.Permission.PERMISSION );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Permission resolve( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Permission.TYPE_ID ) {
                return ( Permission )kobject;
            }

            return new PermissionImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param conditionName
     *        the name of the condition being added (cannot be empty)
     * @return the new condition (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Condition addCondition( final UnitOfWork transaction,
                            final String conditionName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param maskName
     *        the name of the mask being added (cannot be empty)
     * @return the new mask (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Mask addMask( final UnitOfWork transaction,
                  final String maskName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the conditions (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Condition[] getConditions( final UnitOfWork transaction,
                               final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param namePatterns
     *        optional name patterns (can be <code>null</code> or empty but cannot have <code>null</code> or empty elements)
     * @return the masks (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Mask[] getMasks( final UnitOfWork transaction,
                     final String... namePatterns ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the target resource name (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getResourceName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this permission allows alter
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_ALTER
     */
    boolean isAllowAlter( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this permission allows create
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_CREATE
     */
    boolean isAllowCreate( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this permission allows delete
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_DELETE
     */
    boolean isAllowDelete( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this permission allows execute
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_EXECUTE
     */
    boolean isAllowExecute( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this permission allows language
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_LANGUAGE
     */
    boolean isAllowLanguage( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this permission allows read
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_READ
     */
    boolean isAllowRead( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this permission allows update
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_UPDATE
     */
    boolean isAllowUpdate( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param conditionToRemove
     *        the name of the condition being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeCondition( final UnitOfWork transaction,
                          final String conditionToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param maskToRemove
     *        the name of the mask being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeMask( final UnitOfWork transaction,
                     final String maskToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAllowAlter
     *        the new value for the <code>allow alter</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_ALTER
     */
    void setAllowAlter( final UnitOfWork transaction,
                        final boolean newAllowAlter ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAllowCreate
     *        the new value for the <code>allow create</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_CREATE
     */
    void setAllowCreate( final UnitOfWork transaction,
                         final boolean newAllowCreate ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAllowDelete
     *        the new value for the <code>allow delete</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_DELETE
     */
    void setAllowDelete( final UnitOfWork transaction,
                         final boolean newAllowDelete ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAllowExecute
     *        the new value for the <code>allow execute</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_EXECUTE
     */
    void setAllowExecute( final UnitOfWork transaction,
                          final boolean newAllowExecute ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAllowLanguage
     *        the new value for the <code>allow language</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_LANGUAGE
     */
    void setAllowLanguage( final UnitOfWork transaction,
                           final boolean newAllowLanguage ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAllowRead
     *        the new value for the <code>allow read</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_READ
     */
    void setAllowRead( final UnitOfWork transaction,
                       final boolean newAllowRead ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newAllowUpdate
     *        the new value for the <code>allow update</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_UPDATE
     */
    void setAllowUpdate( final UnitOfWork transaction,
                         final boolean newAllowUpdate ) throws KException;

}
