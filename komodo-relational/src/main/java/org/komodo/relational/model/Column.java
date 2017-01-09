/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.model;

import org.komodo.relational.RelationalConstants;
import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.relational.RelationalObject;
import org.komodo.relational.TypeResolver;
import org.komodo.relational.model.internal.ColumnImpl;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon.CreateTable;

/**
 * Represents a relational model column.
 */
public interface Column extends OptionContainer, RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Column.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.COLUMN;

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
    long DEFAULT_CHAR_OCTET_LENGTH = 0;

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
    long DEFAULT_RADIX = 0;

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
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.KNode#getParent(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    Table getParent( final UnitOfWork transaction ) throws KException;

    /**
     * The resolver of a {@link Column}.
     */
    TypeResolver< Column > RESOLVER = new TypeResolver< Column >() {

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#identifier()
         */
        @Override
        public KomodoType identifier() {
            return IDENTIFIER;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#owningClass()
         */
        @Override
        public Class< ColumnImpl > owningClass() {
            return ColumnImpl.class;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolvable(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public boolean resolvable( final UnitOfWork transaction,
                                   final KomodoObject kobject ) throws KException {
            return ObjectImpl.validateType( transaction, kobject.getRepository(), kobject, CreateTable.TABLE_ELEMENT );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.relational.TypeResolver#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
         *      org.komodo.spi.repository.KomodoObject)
         */
        @Override
        public Column resolve( final UnitOfWork transaction,
                               final KomodoObject kobject ) throws KException {
            if ( kobject.getTypeId() == Column.TYPE_ID ) {
                return ( Column )kobject;
            }

            return new ColumnImpl( transaction, kobject.getRepository(), kobject.getAbsolutePath() );
        }

    };

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>char octet length</code> property
     * @throws KException
     *         if an error occurs
     */
    long getCharOctetLength( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>collation name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getCollationName( final UnitOfWork transaction ) throws KException;

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
     * @return the number of distinct values
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_DISTINCT_VALUES
     */
    long getDistinctValues( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>maximum value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getMaxValue( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the value of the <code>minimum value</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getMinValue( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>native type</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getNativeType( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>null value count</code> property
     * @throws KException
     *         if an error occurs
     */
    long getNullValueCount( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>datatype radix</code> property
     * @throws KException
     *         if an error occurs
     */
    long getRadix( final UnitOfWork transaction ) throws KException;

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
     * @return the value of the <code>searchable</code> property (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see Searchable#DEFAULT_VALUE
     */
    Searchable getSearchable( final UnitOfWork transaction ) throws KException;

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
     * @return <code>true</code> if auto-incremented
     * @throws KException
     *         if an error occurs
     * @see Column#DEFAULT_AUTO_INCREMENTED
     */
    boolean isAutoIncremented( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if case-sensitive
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CASE_SENSITIVE
     */
    boolean isCaseSensitive( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this column holds a currency value
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_CURRENCY
     */
    boolean isCurrency( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this column has a fixed length
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_FIXED_LENGTH
     */
    boolean isFixedLength( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this column is selectable
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_SELECTABLE
     */
    boolean isSelectable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this column's value is signed
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_SIGNED
     */
    boolean isSigned( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if this column is updatable
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_UPDATABLE
     */
    boolean isUpdatable( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newCharOctetLength
     *        the new value of the <code>char octet length</code> property
     * @throws KException
     *         if an error occurs
     */
    void setCharOctetLength( final UnitOfWork transaction,
                             final long newCharOctetLength ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newCollationName
     *        the new value of the <code>collation name</code> property (can be empty)
     * @throws KException
     *         if an error occurs
     */
    void setCollationName( final UnitOfWork transaction,
                           final String newCollationName ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     * @param newMaxValue
     *        the new maximum value (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setMaxValue( final UnitOfWork transaction,
                      final String newMaxValue ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newMinValue
     *        the new minimum value (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setMinValue( final UnitOfWork transaction,
                      final String newMinValue ) throws KException;

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
     * @param newNativeType
     *        the new value of the <code>native type</code> property (can only be empty when removing)
     * @throws KException
     *         if an error occurs
     */
    void setNativeType( final UnitOfWork transaction,
                        final String newNativeType ) throws KException;

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
     * @param newNullValueCount
     *        the new value of the <code>null value count</code> property
     * @throws KException
     *         if an error occurs
     */
    void setNullValueCount( final UnitOfWork transaction,
                            final long newNullValueCount ) throws KException;

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
     * @param newRadix
     *        the new value of the <code>datatype radix</code> property
     * @throws KException
     *         if an error occurs
     */
    void setRadix( final UnitOfWork transaction,
                   final long newRadix ) throws KException;

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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newUpdatable
     *        the new value for the <code>updatable</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_UPDATABLE
     */
    void setUpdatable( final UnitOfWork transaction,
                       final boolean newUpdatable ) throws KException;

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
