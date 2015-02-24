/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb;

import org.komodo.relational.model.RelationalObject;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

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
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the conditions (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Condition[] getConditions( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the masks (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Mask[] getMasks( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the target resource name (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getResourceName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this permission allows alter
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_ALTER
     */
    boolean isAllowAlter( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this permission allows create
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_CREATE
     */
    boolean isAllowCreate( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this permission allows delete
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_DELETE
     */
    boolean isAllowDelete( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this permission allows execute
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_EXECUTE
     */
    boolean isAllowExecute( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this permission allows language
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_LANGUAGE
     */
    boolean isAllowLanguage( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this permission allows read
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_READ
     */
    boolean isAllowRead( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this permission allows update
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_UPDATE
     */
    boolean isAllowUpdate( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param conditionToRemove
     *        the name of the condition being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeCondition( final UnitOfWork transaction,
                          final String conditionToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param maskToRemove
     *        the name of the mask being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeMask( final UnitOfWork transaction,
                     final String maskToRemove ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newAllowUpdate
     *        the new value for the <code>allow update</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_ALLOW_UPDATE
     */
    void setAllowUpdate( final UnitOfWork transaction,
                         final boolean newAllowUpdate ) throws KException;

}
