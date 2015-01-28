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

import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlConstants.TeiidReservedWord;

@SuppressWarnings( "javadoc" )
public interface RelationalConstants {

    enum Direction {

        IN( TeiidReservedWord.IN.toDdl() ),
        IN_OUT( TeiidReservedWord.INOUT.toDdl() ),
        OUT( TeiidReservedWord.OUT.toDdl() ),
        RETURN( TeiidReservedWord.RETURN.toDdl() ),
        UNKNOWN( TeiidReservedWord.UNKNOWN.toDdl() );

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
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return this.value;
        }

    }

    enum Nullable {

        NO_NULLS( "NOT NULL" ), //$NON-NLS-1$
        NULLABLE( "NULL" ); //$NON-NLS-1$

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
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return this.value;
        }

    }

    /**
     * The on commit value.
     */
    enum OnCommit {

        /**
         * Delete rows on commit.
         */
        DELETE_ROWS( "DELETE ROWS" ), //$NON-NLS-1$

        /**
         * Preserve rows on commit.
         */
        PRESERVE_ROWS( "PRESERVE ROWS" ); //$NON-NLS-1$

        /**
         * @param value
         *        the value whose <code>OnCommit</code> is being requested (can be empty)
         * @return the corresponding <code>OnCommit</code> or <code>null</code> if not found
         */
        public static OnCommit fromValue( final String value ) {
            if (DELETE_ROWS.value.equals(value)) {
                return DELETE_ROWS;
            }

            if (PRESERVE_ROWS.value.equals(value)) {
                return PRESERVE_ROWS;
            }

            return null;
        }

        private final String value;

        private OnCommit( final String value ) {
            this.value = value;
        }

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return this.value;
        }

    }

    /**
     * Temporary table types.
     */
    enum TemporaryType {

        /**
         * A globally-scoped temporary table.
         */
        GLOBAL,

        /**
         * A locally-scoped temporary table.
         */
        LOCAL;

        /**
         * @param value
         *        the value whose <code>TemporaryType</code> is being requested (can be empty)
         * @return the corresponding <code>TemporaryType</code> or <code>null</code> if not found
         */
        public static TemporaryType fromValue( final String value ) {
            if (GLOBAL.name().equals(value)) {
                return GLOBAL;
            }

            if (LOCAL.name().equals(value)) {
                return LOCAL;
            }

            return null;
        }

    }

    /**
     * The default value for the <code>auto-incremented</code> property. Value is {@value} .
     */
    boolean DEFAULT_AUTO_INCREMENTED = false;

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
    int DEFAULT_PRECISION = 0;

    /**
     * The default value for the <code>datatype scale</code> property. Value is {@value} .
     */
    int DEFAULT_SCALE = 0;

}
