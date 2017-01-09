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

import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

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
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the schema element type (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see SchemaElementType#DEFAULT_VALUE
     */
    SchemaElementType getSchemaElementType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newSchemaElementType
     *        the new value for the <code>schema element type</code> property (can be <code>null</code>)
     * @throws KException
     *         if an error occurs
     * @see SchemaElementType#DEFAULT_VALUE
     */
    void setSchemaElementType( final UnitOfWork transaction,
                               final SchemaElementType newSchemaElementType ) throws KException;

}
