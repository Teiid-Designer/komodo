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
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * Represents a relational model column.
 */
public interface Column extends OptionContainer, RelationalObject {
    /*
      - teiidddl:autoIncrement (boolean) = 'false'
      - ddl:nullable (string) = 'NULL' mandatory autocreated < 'NULL', 'NOT NULL'
      - ddl:datatypeName (STRING)
      - ddl:datatypeLength (LONG)
      - ddl:datatypePrecision (LONG)
      - ddl:datatypeScale (LONG)
    TODO - ddl:defaultOption (STRING)
        < 'LITERAL', 'DATETIME', 'USER', 'CURRENT_USER', 'SESSION_USER', 'SYSTEM_USER', 'NULL'
      - ddl:defaultValue (STRING)
    TODO - ddl:defaultPrecision (LONG)
      - ddl:collationName (STRING)
    TODO + ddl:dropBehavior (ddl:simpleProperty) = ddl:simpleProperty
    TODO + ddl:columnAttribute (ddl:simpleProperty) = ddl:simpleProperty sns
     */

    /**
     * The values for a column's searchable property.
     */
    public enum Searchable {

        /**
         * The column is searchable only when NOT using LIKE.
         */
        ALL_EXCEPT_LIKE,

        /**
         * The column is searchable only when using LIKE.
         */
        LIKE_ONLY,

        /**
         * The column is searchable.
         */
        SEARCHABLE,

        /**
         * The column is not searchable.
         */
        UNSEARCHABLE;

        /**
         * The default value for the searchable property. Value is {@value} .
         */
        public static final Searchable DEFAULT_VALUE = SEARCHABLE;

    }

    /**
     * The default value for the <code>auto-incremented</code> property. Value is {@value} .
     */
    boolean DEFAULT_AUTO_INCREMENTED = false;

    /**
     * The default value for the <code>case-sensitive</code> property. Value is {@value} .
     */
    boolean DEFAULT_CASE_SENSITIVE = false;

    /**
     * The default value for the <code>character octet length</code> property. Value is {@value} .
     */
    int DEFAULT_CHAR_OCTET_LENGTH = 0;

    /**
     * The default value for the <code>currency</code> property. Value is {@value} .
     */
    boolean DEFAULT_CURRENCY = false;

    /**
     * The default value for the <code>distinct values</code> property. Value is {@value} .
     */
    long DEFAULT_DISTINCT_VALUES = -1;

    /**
     * The default value for the <code>fixed length</code> property. Value is {@value} .
     */
    boolean DEFAULT_FIXED_LENGTH = false;

    /**
     * The default value for the <code>null value count</code> property. Value is {@value} .
     */
    long DEFAULT_NULL_VALUE_COUNT = -1;

    /**
     * The default value for the <code>datatype radix</code> property. Value is {@value} .
     */
    int DEFAULT_RADIX = 0;

    /**
     * The default value indicating if this column is selectable. Value is {@value} .
     */
    boolean DEFAULT_SELECTABLE = true;

    /**
     * The default value for the <code>signed</code> property. Value is {@value} .
     */
    boolean DEFAULT_SIGNED = false;

    /**
     * The default value indicating if this column is updatable. Value is {@value} .
     */
    boolean DEFAULT_UPDATABLE = true;

    /**
     * An empty array of columns.
     */
    Column[] NO_COLUMNS = new Column[0];

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>char octet length</code> property
     * @throws KException
     *         if an error occurs
     */
    int getCharOctetLength( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>collation name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getCollationName( final UnitOfWork transaction ) throws KException;

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
     * @return the number of distinct values
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_DISTINCT_VALUES
     */
    long getDistinctValues( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>maximum value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getMaxValue( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the query should be automatically committed)
     * @return the value of the <code>minimum value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getMinValue( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>native type</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getNativeType( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>null value count</code> property
     * @throws KException
     *         if an error occurs
     */
    long getNullValueCount( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>datatype radix</code> property
     * @throws KException
     *         if an error occurs
     */
    int getRadix( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>searchable</code> property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Searchable#DEFAULT_VALUE
     */
    Searchable getSearchable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if auto-incremented
     * @throws KException
     *         if an error occurs
     * @see Column#DEFAULT_AUTO_INCREMENTED
     */
    boolean isAutoIncremented( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if case-sensitive
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CASE_SENSITIVE
     */
    boolean isCaseSensitive( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this column holds a currency value
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CURRENCY
     */
    boolean isCurrency( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this column has a fixed length
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_FIXED_LENGTH
     */
    boolean isFixedLength( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this column is selectable
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_SELECTABLE
     */
    boolean isSelectable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this column's value is signed
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_SIGNED
     */
    boolean isSigned( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this column is updatable
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_UPDATABLE
     */
    boolean isUpdatable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newAutoIncremented
     *        the new value for the <code>auto-incremented</code> property
     * @throws KException
     *         if an error occurs
     * @see Column#DEFAULT_AUTO_INCREMENTED
     */
    void setAutoIncremented( final UnitOfWork transaction,
                             final boolean newAutoIncremented ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newCaseSensitive
     *        the new value for the <code>case-sensitive</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CASE_SENSITIVE
     */
    void setCaseSensitive( final UnitOfWork transaction,
                           final boolean newCaseSensitive ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newCharOctetLength
     *        the new value of the <code>char octet length</code> property
     * @throws KException
     *         if an error occurs
     */
    void setCharOctetLength( final UnitOfWork transaction,
                             final int newCharOctetLength ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newCollationName
     *        the new value of the <code>collation name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setCollationName( final UnitOfWork transaction,
                           final String newCollationName ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newCurrency
     *        the new value for the <code>currency</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CURRENCY
     */
    void setCurrency( final UnitOfWork transaction,
                      final boolean newCurrency ) throws KException;

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
     * @param newDistinctValues
     *        the new value for the <code>distinct values</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_DISTINCT_VALUES
     */
    void setDistinctValues( final UnitOfWork transaction,
                            final long newDistinctValues ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newFixedLength
     *        the new value for the <code>fixed length</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_FIXED_LENGTH
     */
    void setFixedLength( final UnitOfWork transaction,
                         final boolean newFixedLength ) throws KException;

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
     * @param newMaxValue
     *        the new maximum value (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setMaxValue( final UnitOfWork transaction,
                      final String newMaxValue ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if the update should be automatically committed)
     * @param newMinValue
     *        the new minimum value (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setMinValue( final UnitOfWork transaction,
                      final String newMinValue ) throws KException;

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
     * @param newNativeType
     *        the new value of the <code>native type</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setNativeType( final UnitOfWork transaction,
                        final String newNativeType ) throws KException;

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
     * @param newNullValueCount
     *        the new value of the <code>null value count</code> property
     * @throws KException
     *         if an error occurs
     */
    void setNullValueCount( final UnitOfWork transaction,
                            final long newNullValueCount ) throws KException;

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
     * @param newRadix
     *        the new value of the <code>datatype radix</code> property
     * @throws KException
     *         if an error occurs
     */
    void setRadix( final UnitOfWork transaction,
                   final int newRadix ) throws KException;

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
     * @param newSearchable
     *        the new value of the <code>searchable</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Searchable#DEFAULT_VALUE
     */
    void setSearchable( final UnitOfWork transaction,
                        final Searchable newSearchable ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newSelectable
     *        the new value for the <code>selectable</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_SELECTABLE
     */
    void setSelectable( final UnitOfWork transaction,
                        final boolean newSelectable ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newSigned
     *        the new value for the <code>signed</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_SIGNED
     */
    void setSigned( final UnitOfWork transaction,
                    final boolean newSigned ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newUpdatable
     *        the new value for the <code>updatable</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_UPDATABLE
     */
    void setUpdatable( final UnitOfWork transaction,
                       final boolean newUpdatable ) throws KException;

}
