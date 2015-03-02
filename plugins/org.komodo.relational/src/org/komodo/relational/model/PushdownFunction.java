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

/**
 * Represents a pushdown function (CREATE FOREIGN FUNCTION).
 */
public interface PushdownFunction extends Function {

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.PUSHDOWN_FUNCTION;

    /**
     * An empty array of pushdown functions.
     */
    PushdownFunction[] NO_PUSHDOWNS = new PushdownFunction[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = PushdownFunction.class.hashCode();

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the result set (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ProcedureResultSet getResultSet( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @throws KException
     *         if a result set does not exist or an error occurs
     */
    void removeResultSet( final UnitOfWork transaction ) throws KException;

    /**
     * Deletes the current result set and returns a new one of the requested type.
     *
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param tabularResultSet
     *        <code>true</code> if the result set should be tabular; <code>false</code> if the result set is a data type
     * @return the new result set (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see TabularResultSet
     * @see DataTypeResultSet
     */
    ProcedureResultSet setResultSet( final UnitOfWork transaction,
                                     final boolean tabularResultSet ) throws KException;

}
