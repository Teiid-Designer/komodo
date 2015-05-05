/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.RelationalConstants;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

/**
 * Represents a data type result set.
 */
public interface DataTypeResultSet extends ProcedureResultSet {

    /**
     * The valid data types.
     */
    enum Type {

        BIGDECIMAL,
        BIGINT,
        BIGINTEGER,
        BLOB,
        BOOLEAN,
        BYTE,
        CHAR,
        CLOB,
        DATE,
        DECIMAL,
        DOUBLE,
        FLOAT,
        INTEGER,
        LONG,
        OBJECT,
        REAL,
        SHORT,
        SMALLINT,
        STRING,
        TIME,
        TIMESTAMP,
        TINYINT,
        VARBINARY,
        VARCHAR,
        XML;

        /**
         * The default data type.
         */
        public static final Type DEFAULT_VALUE = STRING;
    }

    /**
     * Identifier of this object.
     */
    KomodoType IDENTIFIER = KomodoType.DATA_TYPE_RESULT_SET;

    /**
     * The type identifier.
     */
    int TYPE_ID = DataTypeResultSet.class.hashCode();

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the data type display string
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    String getDisplayString( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>data type length</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    long getLength( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>data type</code> property (never empty)
     * @throws KException
     *         if an error occurs
     * @see Type#DEFAULT_VALUE
     */
    Type getType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if an array type
     * @throws KException
     *         if an error occurs
     */
    boolean isArray( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newArray
     *        <code>true</code> if an array type
     * @throws KException
     *         if an error occurs
     */
    void setArray( final UnitOfWork transaction,
                   final boolean newArray ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newLength
     *        the new value of the <code>data type length</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    void setLength( final UnitOfWork transaction,
                    final long newLength ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newType
     *        the new value for the <code>data type</code> property (can be <code>null</code> when setting to default)
     * @throws KException
     *         if an error occurs
     * @see Type#DEFAULT_VALUE
     */
    void setType( final UnitOfWork transaction,
                  final Type newType ) throws KException;

}
