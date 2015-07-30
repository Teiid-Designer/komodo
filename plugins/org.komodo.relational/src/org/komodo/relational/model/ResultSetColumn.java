/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a relational model result set column.
 */
public interface ResultSetColumn extends OptionContainer, RelationalObject {

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.RESULT_SET_COLUMN;

    /**
     * An empty array of columns.
     */
    ResultSetColumn[] NO_COLUMNS = new ResultSetColumn[0];

    /**
     * The type identifier.
     */
    int TYPE_ID = ResultSetColumn.class.hashCode();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_DATATYPE_NAME
     */
    String getDatatypeName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>default value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDefaultValue( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype length</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    long getLength( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>name in source</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getNameInSource( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>nullable</code> property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Nullable#DEFAULT_VALUE
     */
    Nullable getNullable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype precision</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_PRECISION
     */
    long getPrecision( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>datatype scale</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_SCALE
     */
    long getScale( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>UUID</code> option (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getUuid( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newTypeName
     *        the new value of the <code>datatype name</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_DATATYPE_NAME
     */
    void setDatatypeName( final UnitOfWork transaction,
                          final String newTypeName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDefaultValue
     *        the new value of the <code>default value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setDefaultValue( final UnitOfWork transaction,
                          final String newDefaultValue ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newDescription
     *        the new value of the <code>description</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newLength
     *        the new value of the <code>datatype length</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    void setLength( final UnitOfWork transaction,
                    final long newLength ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNameInSource
     *        the new name in source (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setNameInSource( final UnitOfWork transaction,
                          final String newNameInSource ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newNullable
     *        the new value of the <code>nullable</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Nullable#DEFAULT_VALUE
     */
    void setNullable( final UnitOfWork transaction,
                      final Nullable newNullable ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newPrecision
     *        the new value of the <code>datatype precision</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_PRECISION
     */
    void setPrecision( final UnitOfWork transaction,
                       final long newPrecision ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newScale
     *        the new value of the <code>datatype scale</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_SCALE
     */
    void setScale( final UnitOfWork transaction,
                   final long newScale ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newUuid
     *        the new value of the <code>UUID</code> option (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setUuid( final UnitOfWork transaction,
                  final String newUuid ) throws KException;

}
