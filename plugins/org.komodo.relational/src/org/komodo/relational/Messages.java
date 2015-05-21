/*************************************************************************************
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
 ************************************************************************************/
package org.komodo.relational;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import org.komodo.spi.constants.StringConstants;

/**
 * Messages for the org.komodo.relationa plugin
 */
public class Messages implements StringConstants {

    private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + DOT
                                              + Messages.class.getSimpleName().toLowerCase();

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

    /**
     * Messages relating to the relational model.
     */
    public enum Relational {

        /**
         * An error message indicating a column could not be removed because its ID was not found in a columns multi-valued
         * property.
         */
        COLUMN_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a permission condition could not be removed because it was not found.
         */
        CONDITION_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a constraint could not be removed.
         */
        CONSTRAINT_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a VDB data role could not be removed because it was not found.
         */
        DATA_ROLE_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a role name could not be added because it already exists.
         */
        DUPLICATE_ROLE_NAME,

        /**
         * An error message indicating a VDB entry could not be removed because it was not found.
         */
        ENTRY_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a function could not be removed.
         */
        FUNCTION_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating the proposed statement option value is invalid. Most likely this is a multi-valued value
         * which is not supported.
         */
        INVALID_STATEMENT_OPTION_VALUE,

        /**
         * An error message indicating a mapped role name could not be removed.
         */
        MAPPED_ROLE_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a permission mask could not be removed because it was not found.
         */
        MASK_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a VDB model could not be removed because it was not found.
         */
        MODEL_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a VDB model source could not be removed because it was not found.
         */
        MODEL_SOURCE_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating the object being deleted does not have a parent.
         */
        OBJECT_BEING_DELETED_HAS_NULL_PARENT,

        /**
         * An error message indicating the object being deleted is not from the repository doing the delete.
         */
        OBJECT_BEING_DELETED_HAS_WRONG_REPOSITORY,

        /**
         * An object could not be resolved by the type resolver.
         */
        OBJECT_NOT_RESOLVABLE,

        /**
         * An error message indicating a procedure parameter could not be removed.
         */
        PARAMETER_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a data role permission could not be removed.
         */
        PERMISSION_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a procedure could not be removed.
         */
        PROCEDURE_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a column with a specified JCR UUID could not be found.
         */
        REFERENCED_COLUMN_NOT_FOUND,

        /**
         * An error message indicating a table with a specified JCR UUID could not be found.
         */
        REFERENCED_TABLE_NOT_FOUND,

        /**
         * An error message indicating a result set could not be removed.
         */
        RESULT_SET_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a statement option could not be removed.
         */
        STATEMENT_OPTION_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a table could not be removed.
         */
        TABLE_NOT_FOUND_TO_REMOVE,

        /**
         * A type resolver for a relational object was not found.
         */
        TYPE_RESOLVER_NOT_FOUND,

        /**
         * An error message indicating a VDB translator could not be removed because it was not found.
         */
        TRANSLATOR_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating an unexpected procedure type was found.
         */
        UNEXPECTED_PROCEDURE_TYPE,

        /**
         * An error message indicating an unexpected result set type was found.
         */
        UNEXPECTED_RESULT_SET_TYPE,

        /**
         * An error message indicating an imported VDB could not be removed because it was not found.
         */
        VDB_IMPORT_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating a view could not be removed.
         */
        VIEW_NOT_FOUND_TO_REMOVE,

        /**
         * Indicates a table type other than a table or view was found.
         */
        UNEXPECTED_TABLE_TYPE;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }

    }

    private static String getEnumName( Enum< ? > enumValue ) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

    private Messages() {
    }

    /**
     * Get message string
     *
     * @param key
     * @return i18n string
     */
    private static String getString( Enum< ? > key ) {
        try {
            return RESOURCE_BUNDLE.getString(key.toString());
        } catch (final Exception err) {
            String msg;

            if (err instanceof NullPointerException) {
                msg = "<No message available>"; //$NON-NLS-1$
            } else if (err instanceof MissingResourceException) {
                msg = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                msg = err.getLocalizedMessage();
            }

            return msg;
        }
    }

    /**
     * Get message string with parameters
     *
     * @param key
     *        the enum key
     * @param parameters
     *        params
     * @return i18n string
     */
    public static String getString( Enum< ? > key,
                                    Object... parameters ) {
        String text = getString(key);

        // Check the trivial cases ...
        if (text == null) {
            return OPEN_ANGLE_BRACKET + key.toString() + CLOSE_ANGLE_BRACKET;
        }
        if (parameters == null || parameters.length == 0) {
            return text;
        }

        return MessageFormat.format(text, parameters);
    }
}
