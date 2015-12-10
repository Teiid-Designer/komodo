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
package org.komodo.rest.relational;

import static org.komodo.spi.constants.StringConstants.CLOSE_ANGLE_BRACKET;
import static org.komodo.spi.constants.StringConstants.DOT;
import static org.komodo.spi.constants.StringConstants.OPEN_ANGLE_BRACKET;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import org.komodo.spi.repository.KomodoType;

/**
 * Localized messages for the {@code server rest} project.
 */
public final class RelationalMessages {

    /**
     * Messages relating to errors.
     */
    public enum Error {

        /**
         * An error indicating the VDB descriptor JSON representation could not be created.
         */
        VDB_DESCRIPTOR_BUILDER_ERROR,

        /**
         * An error indicating a VDB could not be created.
         */
        VDB_SERVICE_CREATE_VDB_ERROR,

        /**
         * An error indicating a JSON document representing the VDBs in the workspace could not be retrieved.
         */
        VDB_SERVICE_GET_VDBS_ERROR,

        /**
         * An error indicating an error occurred trying to obtain the specified VDB.
         */
        VDB_SERVICE_GET_VDB_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's models.
         */
        VDB_SERVICE_GET_MODELS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a specific model from a VDB
         */
        VDB_SERVICE_GET_MODEL_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB Model's sources
         */
        VDB_SERVICE_GET_SOURCES_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB Model's source
         */
        VDB_SERVICE_GET_SOURCE_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's translators
         */
        VDB_SERVICE_GET_TRANSLATORS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's translator
         */
        VDB_SERVICE_GET_TRANSLATOR_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's imports
         */
        VDB_SERVICE_GET_IMPORTS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's import
         */
        VDB_SERVICE_GET_IMPORT_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's data roles
         */
        VDB_SERVICE_GET_DATA_ROLES_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's data role
         */
        VDB_SERVICE_GET_DATA_ROLE_ERROR,

        /**
         * An error indicating an error occurred trying to obain a VDB's data role permissions
         */
        VDB_SERVICE_GET_PERMISSIONS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a data role's permission
         */
        VDB_SERVICE_GET_PERMISSION_ERROR,

        /**
         * An error indicating an error occurred trying to obain a permission's conditions
         */
        VDB_SERVICE_GET_CONDITIONS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a permission's condition
         */
        VDB_SERVICE_GET_CONDITION_ERROR,

        /**
         * An error indicating an error occurred trying to obain a permission's masks
         */
        VDB_SERVICE_GET_MASKS_ERROR,

        /**
         * An error indicating an error occurred trying to obain a permission's mask
         */
        VDB_SERVICE_GET_MASK_ERROR,

        /**
         * An error when creating a VDB indicating the input VDB JSON document was missing.
         */
        VDB_SERVICE_MISSING_VDB,

        /**
         * An error indicating the VDB name is missing from the input JSON document.
         */
        VDB_SERVICE_MISSING_VDB_NAME,

        /**
         * An error indicating a VDB with the specified name already exists and therefore cannot be created.
         */
        VDB_SERVICE_VDB_EXISTS,

        /**
         * An error indicating the parameter and JSON VDB name does not match for a VDB being created.
         */
        VDB_SERVICE_VDB_NAME_ERROR,

        /**
         * An error indicating an exception occurred while importing a sample vdb
         */
        VDB_SERVICE_LOAD_SAMPLE_ERROR,

        /**
         * An error indicating the content of a sample vdb could not be loaded into a stream
         */
        VDB_SAMPLE_CONTENT_FAILURE,

        /**
         * Success indicator that the content of a sample vdb was loaded into a stream
         */
        VDB_SAMPLE_CONTENT_SUCCESS,

        /**
         * An error indicating the transaction timeout while awaiting the import of a sample vdb
         */
        VDB_SAMPLE_IMPORT_TIMEOUT,

        /**
         * Success indicator that the import of a sample vdb was successful
         */
        VDB_SAMPLE_IMPORT_SUCCESS,

        /**
         * Import errors occurred during sample vdb import
         */
        VDB_SAMPLE_IMPORT_ERRORS,

        /**
         * Vdb already exists during sample import
         */
        VDB_SAMPLE_IMPORT_VDB_EXISTS,

        /**
         * An error occurred while trying to obtain the teiid schema
         */
        SCHEMA_SERVICE_GET_SCHEMA_ERROR,

        /**
         * An unknown {@link KomodoType} was provided to the get schema operation
         */
        SCHEMA_SERVICE_GET_SCHEMA_UNKNOWN_KTYPE,

        /**
         * The schema was not found
         */
        SCHEMA_SERVICE_GET_SCHEMA_NOT_FOUND,

        /**
         * An error indicating a VDB search failed
         */
        SEARCH_SERVICE_GET_SEARCH_ERROR,

        /**
         * An error indicating a request for saved searched failed
         */
        SEARCH_SERVICE_WKSP_SEARCHES_ERROR,

        /**
         * An error indicating a request to save a search configuration failed
         */
        SEARCH_SERVICE_SAVE_SEARCH_ERROR,

        /**
         * The search service lacks at least one parameter
         */
        SEARCH_SERVICE_NO_PARAMETERS_ERROR,

        /**
         * The search service has both parent and ancestor parameters
         */
        SEARCH_SERVICE_PARENT_ANCESTOR_EXCLUSIVE_ERROR;

        /**
         * {@inheritDoc}
         *
         * @see java.lang.Enum#toString()
         */
        @Override
        public String toString() {
            return getEnumName( this ) + DOT + name();
        }

    }

    private static final String BUNDLE_NAME = RelationalMessages.class.getPackage().getName() + DOT
                                              + RelationalMessages.class.getSimpleName().toLowerCase();

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    private static String getEnumName( final Enum< ? > enumValue ) {
        final String className = enumValue.getClass().getName();
        final String[] components = className.split( "\\$" ); //$NON-NLS-1$
        return components[ components.length - 1 ];
    }

    private static String getString( final Enum< ? > key ) {
        try {
            return RESOURCE_BUNDLE.getString( key.toString() );
        } catch ( final Exception e ) {
            String msg;

            if ( e instanceof NullPointerException ) {
                msg = "<No message key>"; //$NON-NLS-1$
            } else if ( e instanceof MissingResourceException ) {
                msg = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                msg = e.getLocalizedMessage();
            }

            return msg;
        }
    }

    /**
     * @param key
     *        the message key (cannot be <code>null</code>)
     * @param parameters
     *        the substitution parameters (can be <code>null</code>)
     * @return the localized message (never empty)
     */
    public static String getString( final Enum< ? > key,
                                    final Object... parameters ) {
        final String text = getString( key );

        // return key if message not found
        if ( text == null ) {
            return OPEN_ANGLE_BRACKET + key.toString() + CLOSE_ANGLE_BRACKET;
        }

        // return if no parameters to format
        if ( ( parameters == null ) || ( parameters.length == 0 ) ) {
            return text;
        }

        // return formatted message
        return String.format( text, parameters );
    }

    /**
     * Don't allow construction outside of this class.
     */
    private RelationalMessages() {
        // nothing to do
    }

}
