/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a stored procedure (CREATE FOREIGN PROCEDURE).
 */
public interface StoredProcedure extends Procedure {

    /**
     * The default value for the <code>non-prepared</code> property. Value is {@value} .
     */
    boolean DEFAULT_NON_PREPARED = false;

    /**
     * Identifier of this object.
     */
    KomodoType IDENTIFIER = KomodoType.STORED_PROCEDURE;

    /**
     * An empty array of stored procedures.
     */
    StoredProcedure[] NO_PROCEDURES = new StoredProcedure[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = StoredProcedure.class.hashCode();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>native query</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getNativeQuery( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the result set (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ProcedureResultSet getResultSet( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if non-prepared
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_NON_PREPARED
     */
    boolean isNonPrepared( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @throws KException
     *         if a result set does not exist or an error occurs
     */
    void removeResultSet( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNativeQuery
     *        the new value of the <code>native query</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setNativeQuery( final UnitOfWork transaction,
                         final String newNativeQuery ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNonPrepared
     *        the new value for the <code>non-prepared</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_NON_PREPARED
     */
    void setNonPrepared( final UnitOfWork transaction,
                         final boolean newNonPrepared ) throws KException;

    /**
     * Deletes the current result set and returns a new one of the requested type.
     *
     * @param <T>
     *        the type of result set
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param resultSetType
     *        the type of result set being requested (cannot be <code>null</code>)
     * @return the new result set (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see TabularResultSet
     * @see DataTypeResultSet
     */
    < T extends ProcedureResultSet > T setResultSet( final UnitOfWork transaction,
                                                     final Class< T > resultSetType ) throws KException;

}
