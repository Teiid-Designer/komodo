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
 * Represents a relational model procedure or function.
 */
public interface AbstractProcedure extends OptionContainer, RelationalObject, SchemaElement {

    /**
     * The default value of this table's update count. Value is {@value} .
     */
    int DEFAULT_UPDATE_COUNT = 0;

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
     * @return the value of the <code>annotation</code> option (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the query should be automatically committed)
     * @return the value of the <code>name in source</code> option (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getNameInSource( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>update count</code> option
     * @throws KException
     *         if an error occurs
     */
    int getUpdateCount( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>UUID</code> option (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getUuid( final UnitOfWork transaction ) throws KException;

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
     * @param newDescription
     *        the new value of the <code>annotation</code> option (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the update should be automatically committed)
     * @param newNameInSource
     *        the new name in source option (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setNameInSource( final UnitOfWork transaction,
                          final String newNameInSource ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newUpdateCount
     *        the new value of the <code>update count</code> option
     * @throws KException
     *         if an error occurs
     */
    void setUpdateCount( final UnitOfWork transaction,
                         final int newUpdateCount ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newUuid
     *        the new value of the <code>UUID</code> option (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setUuid( final UnitOfWork transaction,
                  final String newUuid ) throws KException;

}
