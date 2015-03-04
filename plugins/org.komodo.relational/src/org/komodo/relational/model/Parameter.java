/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import org.komodo.relational.RelationalConstants.Nullable;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlConstants.TeiidNonReservedWord;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlConstants.TeiidReservedWord;

/**
 * Represents a relational model procedure parameter.
 */
public interface Parameter extends OptionContainer, RelationalObject {

    /**
     * The type identifier.
     */
    int TYPE_ID = Parameter.class.hashCode();

    /**
     * Identifier of this object
     */
    KomodoType IDENTIFIER = KomodoType.PARAMETER;

    /**
     * Represents a parameter direction.
     */
    public enum Direction {

        /**
         * Input parameters have this direction.
         */
        IN( TeiidReservedWord.IN.toDdl() ),

        /**
         * Input/output parameters have this direction.
         */
        IN_OUT( TeiidReservedWord.INOUT.toDdl() ),

        /**
         * Output parameters have this direction.
         */
        OUT( TeiidReservedWord.OUT.toDdl() ),

        /**
         * Variable number of parameters have this direction.
         */
        VARIADIC( TeiidNonReservedWord.VARIADIC.toDdl() );

        /**
         * The default direction. Value is {@value} .
         */
        public static final Direction DEFAULT_VALUE = IN;

        /**
         * @param value
         *        the value whose <code>Direction</code> is being requested (can be empty)
         * @return the corresponding <code>Direction</code> or the default value if not found
         * @see #DEFAULT_VALUE
         */
        public static Direction fromValue( final String value ) {
            for (final Direction nullable : values()) {
                if (nullable.value.equals(value)) {
                    return nullable;
                }
            }

            return DEFAULT_VALUE;
        }

        private final String value;

        private Direction( final String value ) {
            this.value = value;
        }

        /**
         * @return the Teiid direction value (never empty)
         */
        public String toValue() {
            return this.value;
        }

    }

    /**
     * The default value for the <code>is result</code> property. Value is {@value} .
     */
    boolean DEFAULT_RESULT = false;

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
     * @return the value of the <code>datatype length</code> property
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
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>datatype precision</code> property
     * @throws KException
     *         if an error occurs
     */
    int getPrecision( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the value of the <code>datatype scale</code> property
     * @throws KException
     *         if an error occurs
     */
    int getScale( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return <code>true</code> if this parameter is the procedure result
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_RESULT
     */
    boolean isResult( final UnitOfWork transaction ) throws KException;

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
     *        the new value of the <code>datatype length</code> property
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
     *        the new value of the <code>datatype precision</code> property
     * @throws KException
     *         if an error occurs
     */
    void setPrecision( final UnitOfWork transaction,
                       final int newPrecision ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newResult
     *        the new value for the <code>is result</code> property
     * @throws KException
     *         if an error occurs
     * @see #DEFAULT_RESULT
     */
    void setResult( final UnitOfWork transaction,
                    final boolean newResult ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newScale
     *        the new value of the <code>datatype scale</code> property
     * @throws KException
     *         if an error occurs
     */
    void setScale( final UnitOfWork transaction,
                   final int newScale ) throws KException;

}
