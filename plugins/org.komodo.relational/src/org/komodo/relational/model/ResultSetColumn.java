/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;

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

    /*
        [teiidddl:resultColumn] mixin
        - ddl:datatypeName (string)
        - ddl:datatypeLength (long)
        - ddl:datatypePrecision (long)
        - ddl:datatypeScale (long)
        - ddl:nullable (string) = 'NULL' mandatory autocreated < 'NULL', 'NOT NULL'
        + * (ddl:statementOption) = ddl:statementOption
     */

    /**
     * The type identifier.
     */
    int TYPE_ID = ResultSetColumn.class.hashCode();

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>datatype name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_DATATYPE_NAME
     */
    String getDatatypeName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>default value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDefaultValue( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>description</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>datatype length</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_LENGTH
     */
    long getLength( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the query should be automatically committed)
     * @return the value of the <code>name in source</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getNameInSource( final UnitOfWork transaction ) throws KException;

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
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>datatype precision</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_PRECISION
     */
    int getPrecision( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>datatype scale</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_SCALE
     */
    int getScale( final UnitOfWork transaction ) throws KException;

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
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newDefaultValue
     *        the new value of the <code>default value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setDefaultValue( final UnitOfWork transaction,
                          final String newDefaultValue ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newDescription
     *        the new value of the <code>description</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setDescription( final UnitOfWork transaction,
                         final String newDescription ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
     *        the transaction (can be <code>null</code> if the update should be automatically committed)
     * @param newNameInSource
     *        the new name in source (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setNameInSource( final UnitOfWork transaction,
                          final String newNameInSource ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
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
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newPrecision
     *        the new value of the <code>datatype precision</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_PRECISION
     */
    void setPrecision( final UnitOfWork transaction,
                       final int newPrecision ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newScale
     *        the new value of the <code>datatype scale</code> property
     * @throws KException
     *         if an error occurs
     * @see RelationalConstants#DEFAULT_SCALE
     */
    void setScale( final UnitOfWork transaction,
                   final int newScale ) throws KException;

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
