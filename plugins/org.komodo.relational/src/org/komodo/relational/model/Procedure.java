/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a relational model procedure.
 */
public interface Procedure extends OptionContainer, RelationalObject, SchemaElement {

    /**
     * An empty array of procedures.
     */
    Procedure[] NO_PROCEDURES = new Procedure[0];

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param parameterName
     *        the name of the parameter being added (cannot be <code>null</code>)
     * @return the new parameter (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Parameter addParameter( final UnitOfWork transaction,
                            final String parameterName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the AS clause <code>statement</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getAsClauseStatement( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the input parameters (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    Parameter[] getParameters( final UnitOfWork transaction ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Model getParent( final UnitOfWork transaction ) throws KException;

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
     * @return <code>true</code> if a function
     * @throws KException
     *         if an error occurs
     */
    boolean isFunction( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param parameterName
     *        the name of the parameter to remove (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void removeParameter( final UnitOfWork transaction,
                          final String parameterName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newStatement
     *        the new AS clause statement (cannot be empty)
     * @throws KException
     *         if an error occurs
     */
    void setAsClauseStatement( final UnitOfWork transaction,
                               final String newStatement ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newFunction
     *        the new value for the <code>function</code> property
     * @throws KException
     *         if an error occurs
     */
    void setFunction( final UnitOfWork transaction,
                      final boolean newFunction ) throws KException;

}
