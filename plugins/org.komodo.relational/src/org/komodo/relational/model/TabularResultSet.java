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
 * Represents a tabular result set.
 */
public interface TabularResultSet extends ProcedureResultSet {

    /**
     * Identifier of this object.
     */
    KomodoType IDENTIFIER = KomodoType.TABULAR_RESULT_SET;

    /**
     * The type identifier.
     */
    int TYPE_ID = TabularResultSet.class.hashCode();

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param columnName
     *        the name of the column being added (cannot be empty)
     * @return the new column (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ResultSetColumn addColumn( final UnitOfWork transaction,
                               final String columnName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the columns (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    ResultSetColumn[] getColumns( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param columnToRemove
     *        the name of the column being removed (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeColumn( final UnitOfWork transaction,
                       final String columnToRemove ) throws KException;

}
