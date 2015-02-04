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
package org.komodo.repository;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import org.komodo.spi.constants.StringConstants;

/**
 *
 */
public class Messages implements StringConstants {

    private static final String BUNDLE_NAME = Messages.class.getPackage().getName()
    																					+ DOT
    																					+ Messages.class.getSimpleName().toLowerCase();

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

    @SuppressWarnings( "javadoc" )
    public enum LocalRepository {

        Commit_Timeout,
        Configuration_Problem,
        Configuration_Failure,
        Deployment_Failure,
        General_Exception,
        Engine_Is_Stopped,
        EngineThread_Died,
        Rollback_Timeout,
        Unable_To_Create_Session;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    /**
     * Messages associated with workspace and library objects and their properties.
     */
    @SuppressWarnings( "javadoc" )
    public enum Komodo {

        ARTIFACT_DOES_NOT_EXIST_ERROR,
        ARTIFACT_EXISTS_ERROR,
        DUPLICATE_OBJECT_ERROR,
        ERROR_ADDING_ARTIFACT,
        ERROR_SESSION_IS_CLOSED,
        ERROR_TRYING_TO_COMMIT,
        ERROR_TRYING_TO_ROLLBACK,

        /**
         * An error message indicating the type of the object is not incorrect.
         */
        INCORRECT_TYPE,

        /**
         * An error message indicating the object has an incorrect property value.
         */
        INVALID_PROPERTY_VALUE,

        NO_ARTIFACT_DESCRIPTION,
        REMOVE_WORKSPACE_OBJECT_ERROR,
        UNABLE_TO_CONSTRUCT_PROPERTY,
        UNABLE_TO_CONVERT_VALUE,
        UNABLE_TO_FIND_PROPERTY,
        UNABLE_TO_REMOVE_CHILD,
        UNABLE_TO_REMOVE_NON_EXISTENT_WORKSPACE_ITEM,
        UNABLE_TO_REMOVE_PROPERTY_THAT_DOES_NOT_EXIST,
        UNABLE_TO_REMOVE_SINGLE_VALUE_PROPERTY_WITH_EMPTY_ARRAY,
        UNABLE_TO_SET_SINGLE_VALUE_PROPERTY_WITH_MULTIPLE_VALUES,
        UNABLE_TO_UNPUBLISH_NON_EXISTENT_ARTIFACT,
        UNPUBLISH_ARTIFACT_ERROR,
        WORKSPACE_FIND_BY_TYPE_ERROR;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    private static String getEnumName(Enum<?> enumValue) {
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
     *
     * @return i18n string
     */
    private static String getString(Enum<?> key) {
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
     * @param key the key
     * @param parameters additional parameters
     *
     * @return i18n string
     */
    public static String getString(Enum<?> key, Object... parameters) {
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
