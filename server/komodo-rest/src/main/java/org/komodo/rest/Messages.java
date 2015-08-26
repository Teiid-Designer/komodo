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
package org.komodo.rest;

import static org.komodo.spi.constants.StringConstants.CLOSE_ANGLE_BRACKET;
import static org.komodo.spi.constants.StringConstants.DOT;
import static org.komodo.spi.constants.StringConstants.OPEN_ANGLE_BRACKET;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 * Localized messages for the {@code komodo-rest} project.
 */
public final class Messages {

    /**
     * Messages relating to errors.
     */
    public enum Error {

        /**
         * An error message indicating a repository commit did not complete in the required amount of time.
         */
        COMMIT_TIMEOUT,

        /**
         * An error message indicating the Komodo Engine did not clear the repository due to an error.
         */
        KOMODO_ENGINE_CLEAR_ERROR,

        /**
         * An error message indicating the Komodo Engine did not clear the repository in the required amount of time.
         */
        KOMODO_ENGINE_CLEAR_TIMEOUT,

        /**
         * An error message indicating the Komodo Engine did not shutdown due to an error.
         */
        KOMODO_ENGINE_SHUTDOWN_ERROR,

        /**
         * An error message indicating the Komodo Engine did not shutdown in the required amount of time.
         */
        KOMODO_ENGINE_SHUTDOWN_TIMEOUT,

        /**
         * An error message indicating the Komodo Engine did not startup due to an error.
         */
        KOMODO_ENGINE_STARTUP_ERROR,

        /**
         * An error message indicating the Komodo Engine did not startup in the required amount of time.
         */
        KOMODO_ENGINE_STARTUP_TIMEOUT,

        /**
         * An error message indicating the Komodo Engine's workspace manager could not be obtained.
         */
        KOMODO_ENGINE_WORKSPACE_MGR_ERROR,

        /**
         * An error message indicating the requested resource was not found.
         */
        RESOURCE_NOT_FOUND,

        /**
         * An error message indicating a repository rollback had an error.
         */
        ROLLBACK_ERROR,

        /**
         * An error message indicating a repository rollback did not complete in the required amount of time.
         */
        ROLLBACK_TIMEOUT,

        /**
         * An error indicating the VDB descriptor JSON representation could not be created.
         */
        VDB_DESCRIPTOR_BUILDER_ERROR,

        /**
         * An error indicating a VDB could not be created.
         */
        VDB_SERVICE_CREATE_VDB_ERROR,

        /**
         * An error indicating an error occurred trying to obtain the specified VDB.
         */
        VDB_SERVICE_GET_VDB_ERROR,

        /**
         * An error indicating a JSON document representing the VDBs in the workspace could not be retrieved.
         */
        VDB_SERVICE_GET_VDBS_ERROR,

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
        VDB_SERVICE_VDB_NAME_ERROR;

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

    /**
     * General messages.
     */
    public enum General {

        /**
         * The name of the DELETE operation.
         */
        DELETE_OPERATION_NAME,

        /**
         * The name of the GET operation.
         */
        GET_OPERATION_NAME,

        /**
         * A message indicating a value is not present.
         */
        NO_VALUE;

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

    private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + DOT
                                              + Messages.class.getSimpleName().toLowerCase();

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
    private Messages() {
        // nothing to do
    }

}
