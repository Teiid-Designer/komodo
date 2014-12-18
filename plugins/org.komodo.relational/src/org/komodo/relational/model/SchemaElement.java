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
 * Represents an element that has a schema element type.
 */
public interface SchemaElement {

    /**
     * The schema element type.
     */
    public enum SchemaElementType {

        /**
         * A foreign or physical schema element.
         */
        FOREIGN,

        /**
         * A virtual schema element.
         */
        VIRTUAL;

        /**
         * The default type. Value is {@value} .
         */
        public static final SchemaElementType DEFAULT_VALUE = FOREIGN;

        /**
         * @param value
         *        the value whose <code>SchemaElementType</code> is being requested (can be empty)
         * @return the corresponding <code>SchemaElementType</code> or the default value if not found
         * @see #DEFAULT_VALUE
         */
        public static SchemaElementType fromValue( final String value ) {
            if (FOREIGN.name().equals(value)) {
                return FOREIGN;
            }

            if (VIRTUAL.name().equals(value)) {
                return VIRTUAL;
            }

            return DEFAULT_VALUE;
        }

    }

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @return the schema element type (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see SchemaElementType#DEFAULT_VALUE
     */
    SchemaElementType getSchemaElementType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param newSchemaElementType
     *        the new value for the <code>schema element type</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see SchemaElementType#DEFAULT_VALUE
     */
    void setSchemaElementType( final UnitOfWork transaction,
                               final SchemaElementType newSchemaElementType ) throws KException;

}
