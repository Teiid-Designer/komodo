/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.RelationalConstants.Direction;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a relational model procedure parameter.
 */
public interface Parameter extends OptionContainer, RelationalObject {

    /**
     * The default value for the <code>default value</code> property. Value is {@value} .
     */
    String DEFAULT_DEFAULT_VALUE = null;

    /**
     * The default length for a string parameter. Value is {@value} .
     */
    int DEFAULT_STRING_LENGTH = 4000;

    /**
     * An empty array of parameters.
     */
    Parameter[] NO_PARAMETERS = new Parameter[0];

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>data type name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDatatypeName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>default value</code> (can be empty)
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_DEFAULT_VALUE
     */
    String getDefaultValue( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>direction</code> property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Direction#DEFAULT_VALUE
     */
    Direction getDirection( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>length</code> property
     * @throws KException
     *         if an error occurs
     */
    long getLength( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>nullable</code> property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Nullable#DEFAULT_VALUE
     */
    Nullable getNullable( final UnitOfWork transaction ) throws KException;

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Procedure getParent( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>precision</code> property
     * @throws KException
     *         if an error occurs
     */
    int getPrecision( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the parent <code>procedure</code> (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Procedure getProcedure( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>scale</code> property
     * @throws KException
     *         if an error occurs
     */
    int getScale( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newTypeName
     *        the new datatype name (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setDatatypeName( final UnitOfWork transaction,
                          final String newTypeName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newDefaultValue
     *        the new default value (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_DEFAULT_VALUE
     */
    void setDefaultValue( final UnitOfWork transaction,
                          final String newDefaultValue ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newDirection
     *        the new direction (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Direction#DEFAULT_VALUE
     */
    void setDirection( final UnitOfWork transaction,
                       final Direction newDirection ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newLength
     *        the new length
     * @throws KException
     *         if an error occurs
     */
    void setLength( final UnitOfWork transaction,
                    final long newLength ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newNullable
     * @throws KException
     *         if an error occurs
     * @see Nullable#DEFAULT_VALUE
     */
    void setNullable( final UnitOfWork transaction,
                      final Nullable newNullable ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newPrecision
     *        the new precision
     * @throws KException
     *         if an error occurs
     */
    void setPrecision( final UnitOfWork transaction,
                       final int newPrecision ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newScale
     *        the new scale
     * @throws KException
     *         if an error occurs
     */
    void setScale( final UnitOfWork transaction,
                   final int newScale ) throws KException;

}
