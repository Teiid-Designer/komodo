/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational;


@SuppressWarnings( "javadoc" )
public interface RelationalConstants {

    enum Nullable {

        NO_NULLS( "NOT NULL" ), //$NON-NLS-1$
        NULLABLE( "NULL" ), //$NON-NLS-1$
        NULLABLE_UNKNOWN( "UNKNOWN" ); //$NON-NLS-1$

        public static final Nullable DEFAULT_VALUE = NULLABLE;

        /**
         * @param value
         *        the value whose <code>Nullable</code> is being requested (can be empty)
         * @return the corresponding <code>Nullable</code> or the default value if not found
         * @see #DEFAULT_VALUE
         */
        public static Nullable fromValue( final String value ) {
            for (final Nullable nullable : values()) {
                if (nullable.value.equals(value)) {
                    return nullable;
                }
            }

            return DEFAULT_VALUE;
        }

        private final String value;

        private Nullable( final String value ) {
            this.value = value;
        }

        /**
         * @return the Teiid nullable value (never empty)
         */
        public String toValue() {
            return this.value;
        }

    }

    /**
     * The default value for the <code>datatype name</code> property. Value is {@value} .
     */
    String DEFAULT_DATATYPE_NAME = "STRING"; //$NON-NLS-1$

    /**
     * The default value for the <code>datatype length</code> property. Value is {@value} .
     */
    long DEFAULT_LENGTH = 0;

    /**
     * The default value for the <code>datatype precision</code> property. Value is {@value} .
     */
    long DEFAULT_PRECISION = 0;

    /**
     * The default value for the <code>datatype scale</code> property. Value is {@value} .
     */
    long DEFAULT_SCALE = 0;

}
