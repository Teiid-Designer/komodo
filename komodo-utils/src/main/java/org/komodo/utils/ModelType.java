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
package org.komodo.utils;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 
 *
 *
 */
public final class ModelType implements Serializable {

    /**
     *
     */
    private static final long serialVersionUID = 6569679435239246363L;

    /**
     *
     */
    public static final String copyright = "See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing."; //$NON-NLS-1$ // NO_UCD

    /**
     * Enumerator versions of the ModelType literals
     */
    public enum Type {
        /**
         * Enum version of {@link ModelType} literal
         */
        PHYSICAL(true),

        /**
         * Enum version of {@link ModelType} literal
         */
        VIRTUAL(true),

        /**
         * Enum version of {@link ModelType} literal
         */
        TYPE(true),

        /**
         * Enum version of {@link ModelType} literal
         */
        VDB_ARCHIVE(false),

        /**
         * Enum version of {@link ModelType} literal
         */
        UNKNOWN(false),

        /**
         * Enum version of {@link ModelType} literal
         */
        FUNCTION(false),

        /**
         * Enum version of {@link ModelType} literal
         */
        CONFIGURATION(false),

        /**
         * Enum version of {@link ModelType} literal
         */
        METAMODEL(true),

        /**
         * Enum version of {@link ModelType} literal
         */
        EXTENSION(true),

        /**
         * Enum version of {@link ModelType} literal
         */
        LOGICAL(true),

        /**
         * Enum version of {@link ModelType} literal
         */
        MATERIALIZATION(false);

        private final boolean sheddable;

        private static List<String> nameCache;

        /**
         * private constructor
         */
        private Type(boolean sheddable) {
            this.sheddable = sheddable;
        }

        /**
         * @return the sheddable
         */
        public boolean isSheddable() {
            return this.sheddable;
        }

        /**
         * @return the name.
         */
        public final String getName() {
            return name();
        }

        /**
         * @return CamelCase version of name
         */
        public final String getCamelCaseName() {
            return StringUtils.toCamelCase(getName());
        }

        /**
         * @return the value.
         */
        public final int getValue() {
            return ordinal();
        }

        /**
         * @return the literal.
         */
        public final String getLiteral() {
            return name();
        }

        /**
         * @return the literal.
         */
        @Override
        public final String toString() {
            return name();
        }

        /**
         * @return array of the type names
         */
        public static synchronized List<String> getNames() {
            if (nameCache == null) {
                nameCache = new ArrayList<String>();
                for (Type type : Type.values()) {
                    nameCache.add(type.getName());
                }
            }

            return Collections.unmodifiableList(nameCache);
        }

        /**
         * @param name
         * @return the type with the given name
         */
        public static Type getType(String name) {
            name = name.toUpperCase();

            if (! getNames().contains(name))
                throw new IllegalArgumentException(name + " is an unknown model type"); //$NON-NLS-1$

            return Type.valueOf(name);
        }

        /**
         * @param name
         * @return index of the given string once successfully parsed
         */
        public static int parseString(String name) {
            Type type = getType(name);
            return type.getValue();
        }

        /**
         * @param index
         * @return type for given index
         */
        public static Type getType(int index) {
            Type[] enumValues = values();
            if (index < 0 || index >= enumValues.length)
                throw new IllegalArgumentException(index + " denotes an unknown model type"); //$NON-NLS-1$

            return enumValues[index];
        }

        /**
         * @param index
         * @return literal for the given index
         */
        public static String getString(int index) {
            Type type = getType(index);
            return type.getLiteral();
        }
    }

    /**
     * The '<em><b>PHYSICAL</b></em>' literal value.
     */
    public static final int PHYSICAL = Type.PHYSICAL.getValue();

    /**
     * The '<em><b>VIRTUAL</b></em>' literal value.
     */
    public static final int VIRTUAL = Type.VIRTUAL.getValue();

    /**
     * The '<em><b>TYPE</b></em>' literal value.
     */
    public static final int TYPE = Type.TYPE.getValue();

    /**
     * The '<em><b>VDB ARCHIVE</b></em>' literal value.
     */
    public static final int VDB_ARCHIVE = Type.VDB_ARCHIVE.getValue();

    /**
     * The '<em><b>UNKNOWN</b></em>' literal value.
     */
    public static final int UNKNOWN = Type.UNKNOWN.getValue();

    /**
     * The '<em><b>FUNCTION</b></em>' literal value.
     */
    public static final int FUNCTION = Type.FUNCTION.getValue();

    /**
     * The '<em><b>CONFIGURATION</b></em>' literal value.
     */
    public static final int CONFIGURATION = Type.CONFIGURATION.getValue();

    /**
     * The '<em><b>METAMODEL</b></em>' literal value.
     */
    public static final int METAMODEL = Type.METAMODEL.getValue();

    /**
     * The '<em><b>EXTENSION</b></em>' literal value.
     */
    public static final int EXTENSION = Type.EXTENSION.getValue();

    /**
     * The '<em><b>LOGICAL</b></em>' literal value.
     */
    public static final int LOGICAL = Type.LOGICAL.getValue();

    /**
     * The '<em><b>MATERIALIZATION</b></em>' literal value.
     */
    public static final int MATERIALIZATION = Type.MATERIALIZATION.getValue();

    /**
     * @param itemType
     * @return whether given item type is shreddable
     */
    public static boolean isShredable(final int itemType) { // NO_UCD
        Type[] enumValues = Type.values();
        if (itemType < 0 || itemType >= enumValues.length)
            throw new IllegalArgumentException("Unknown model type"); //$NON-NLS-1$

        return enumValues[itemType].isSheddable();
    }
}
