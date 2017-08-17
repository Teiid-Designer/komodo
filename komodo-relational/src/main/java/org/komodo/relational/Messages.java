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
import org.komodo.spi.repository.KomodoObject;

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
         * An error message indicating the object lacks a UUID property, implying its not referenceable,
         * ie. does not extend mix:referenceable
         */
        NO_UUID_PROPERTY,

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
         * An error message indicating the connection className property was undefined.
         */
        CONNECTION_CLASSNAME_NOT_DEFINED,

        /**
         * An error message indicating the connection driverName property was undefined.
         */
        CONNECTION_DRIVERNAME_NOT_DEFINED,

        /**
         * An error message indicating the connection jndiName property was undefined.
         */
        CONNECTION_JNDINAME_NOT_DEFINED,

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
         * An error message indicating a empty resources was being exported.
         */
        EXPORT_FAILED_NO_CONTENT,

        /**
         * An error message indicating a function could not be removed.
         */
        FUNCTION_NOT_FOUND_TO_REMOVE,

        /**
         * An error message indicating grouping node type is invalice. Takes the grouping node name and the invalid type as
         * parameters.
         */
        INVALID_GROUPING_NODE_TYPE,

        /**
         * An error message indicating the type of the parent object is invalid.
         */
        INVALID_PARENT_TYPE,

        /**
         * An error message indicating the proposed statement option value is invalid. Most likely this is a multi-valued value
         * which is not supported.
         */
        INVALID_STATEMENT_OPTION_VALUE,

        /**
         * An error message indicating the column cannot be used as a foreign key column. The one parameter is the column path.
         */
        INVALID_TABLE_CONSTRAINT_COLUMN,

        /**
         * An error message indicating an object being set as the foreign key table reference is not a table or even a
         * {@link KomodoObject}. Takes the object class as a parameter.
         */
        INVALID_TABLE_REF,

        /**
         * An error message indicating the column cannot be used as a reference as its table is not the parent table of the
         * foreign key.
         */
        INVALID_TABLE_REF_COLUMN,

        /**
         * An error message indicating an object being set as the foreign key table reference is not a table. Takes the path and
         * type of the object as parameters.
         */
        INVALID_TABLE_REF_PATH,

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
         * An error message indicating more than one value is being used to set the table reference.
         */
        MULTIPLE_TABLE_REFS_NOT_ALLOWED,

        /**
         * An error message indicating the object being deleted does not have a parent.
         */
        OBJECT_BEING_DELETED_HAS_NULL_PARENT,

        /**
         * An error message indicating the object being deleted is not from the repository doing the delete.
         */
        OBJECT_BEING_DELETED_HAS_WRONG_REPOSITORY,

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
         * An error message indicating a property is not modifiable.
         */
        PROPERTY_NOT_MODIFIABLE,

        /**
         * An error message indicating a column with a specified JCR UUID could not be found.
         */
        REFERENCED_COLUMN_NOT_FOUND,

        /**
         * An error message indicating a data service referenced resource could not be found.
         */
        REFERENCED_RESOURCE_NOT_FOUND,

        /**
         * An error message indicating a table with a specified JCR UUID could not be found.
         */
        REFERENCED_TABLE_NOT_FOUND,

        /**
         * An error message indicating the object rename is not allowed.
         */
        RENAME_NOT_ALLOWED,

        /**
         * An error message indicating removing (deleting) the object is not allowed.
         */
        REMOVE_NOT_ALLOWED,

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
         * An error message indicating columns of a foreign key's table reference cannot be set since there is no table reference
         * set. The one parameter is the foreign key's name.
         */
        TABLE_REF_NOT_SET,

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
        UNEXPECTED_TABLE_TYPE,

        /**
         * An error indicating a teiid instance could not be generated
         */
        TEIID_INSTANCE_ERROR,

        /**
         * An error indicating a teiid instance could not connect
         */
        TEIID_INSTANCE_CONNECTION_ERROR,

        /**
         * An error indicating server manager failed to create the default teiid server model
         */
        SERVER_MANAGER_DEFAULT_TEIID_ERROR,

        /**
         * An error indicating there is no storage plugin for the given storage type
         */
        STORAGE_TYPE_INVALID,

        /**
         * An error indicating there document type of an import is invalid
         */
        STORAGE_DOCUMENT_TYPE_INVALID;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }

    }

    public enum DataserviceConveyor {

        /**
         * An error message indicating the driver cannot be found in the data service
         */
        DATA_SERVICE_DRIVER_NOT_FOUND,

        /**
         * An error message indicating the driver failed to deploy
         */
        DATA_SERVICE_DRIVER_FAILED_TO_DEPLOY,

        /**
         * An error message indicating the data source cannot be found in the data service
         */
        DATA_SERVICE_DATA_SOURCE_NOT_FOUND,

        /**
         * An error message indicating the data source failed to deploy
         */
        DATA_SERVICE_DATA_SOURCE_FAILED_TO_DEPLOY,

        /**
         * An error message indicating the vdb cannot be found in the data service
         */
        DATA_SERVICE_VDB_NOT_FOUND,

        /**
         * A message indicating a data service is being deployed.
         */
        DATA_SERVICE_START_DEPLOY,

        /**
         * A message indicating a data service has been successfully deployed.
         */
        DATA_SERVICE_SUCCESSFULLY_DEPLOYED,

        /**
         * A message indicating a connection is being deployed.
         */
        DATA_SERVICE_CONNECTION_START_DEPLOY,

        /**
         * A message indicating a connection has been successfully deployed.
         */
        DATA_SERVICE_CONNECTION_SUCCESSFULLY_DEPLOYED,

        /**
         * A message indicating a driver is being deployed.
         */
        DATA_SERVICE_DRIVER_START_DEPLOY,

        /**
         * A message indicating a driver has been successfully deployed.
         */
        DATA_SERVICE_DRIVER_SUCCESSFULLY_DEPLOYED,

        /**
         * A message indicating a VDB is being deployed.
         */
        DATA_SERVICE_VDB_START_DEPLOY,

        /**
         * A message indicating a VDB has been successfully deployed.
         */
        DATA_SERVICE_VDB_SUCCESSFULLY_DEPLOYED,

        /**
         * An error message indicating the vdb contents cannot be exported from the data service
         */
        DATA_SERVICE_VDB_CONTENTS_FAILURE;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    /**
     * Messages for CachedTeiid
     */
    public enum CachedTeiid {

        /**
         * An error message indicating get Vdbs from server failed.
         */
        GET_SERVER_VDBS_ERROR,

        /**
         * An error message indicating get a Vdb from server failed.
         */
        GET_SERVER_VDB_ERROR,

        /**
         * An error message indicating get DataSources from server failed.
         */
        GET_SERVER_DATA_SOURCES_ERROR,

        /**
         * An error message indicating get a DataSource from server failed.
         */
        GET_SERVER_DATA_SOURCE_ERROR,

        /**
         * An error message indicating get Drivers from server failed.
         */
        GET_SERVER_DRIVERS_ERROR,

        /**
         * An error message indicating get Translators from server failed.
         */
        GET_SERVER_TRANSLATORS_ERROR,

        /**
         * An error message indicating get a Translator from server failed.
         */
        GET_SERVER_TRANSLATOR_ERROR,

        /**
         * An error message indicating get a Template from server failed.
         */
        GET_SERVER_TEMPLATE_ERROR;



        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    /**
     * @param enumValue the value
     * @return the name
     */
    public static String getEnumName( Enum< ? > enumValue ) {
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

    /**
     * @param key
     *        the key of the localized message being requested (cannot be empty)
     * @return the message (never empty)
     */
    public static String getString( final String key ) {
        return getString( RESOURCE_BUNDLE, key );
    }

    /**
     * Look up a message in the i18n resource message bundle by key, then format the message with the given params and return the
     * result.
     *
     * @param bundle
     *        the resource bundle (cannot be <code>null</code>)
     * @param key
     *        the message key
     * @param parameters
     *        the parameters
     * @return the message
     */
    public static String getString(final ResourceBundle bundle,
                                   final String key,
                                   final Object ... parameters) {
        String text = null;

        try {
            text = bundle.getString(key);
        } catch (final Exception err) {
            if (err instanceof NullPointerException) {
                text = "<No message available>"; //$NON-NLS-1$
            } else if (err instanceof MissingResourceException) {
                text = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                text = err.getLocalizedMessage();
            }
        }

        // Check the trivial cases ...
        if (text == null) {
            return OPEN_ANGLE_BRACKET + key + CLOSE_ANGLE_BRACKET;
        }
        if (parameters == null || parameters.length == 0) {
            return text;
        }

        return MessageFormat.format(text, parameters);
    }

}
