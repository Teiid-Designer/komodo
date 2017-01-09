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

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    @SuppressWarnings( "javadoc" )
    public enum LocalRepository {

        Commit_Timeout,
        Configuration_Problem,
        Configuration_Failure,
        Deployment_Failure,
        General_Exception,
        Engine_Not_Running,
        Repository_Not_Running,
        EngineThread_Died,
        Rollback_Timeout,
        Unable_To_Create_Session;

        @Override
        public String toString() {
            return getEnumName( this ) + DOT + name();
        }
    }

    /**
     * Messages associated with workspace and library objects and their properties.
     */
    @SuppressWarnings( "javadoc" )
    public enum Komodo {

        ARTIFACT_DOES_NOT_EXIST_ERROR,
        ARTIFACT_EXISTS_ERROR,

        /**
         * Indicates a child with a given name cannot be found.
         */
        CHILD_NOT_FOUND,

        /**
         * Indicates a node type or mixin with a given name cannot be found.
         */
        DESCRIPTOR_NOT_FOUND,

        DUPLICATE_OBJECT_ERROR,
        ERROR_ADDING_ARTIFACT,
        ERROR_CONSTRUCTING_VALIDATION_MANAGER,

        /**
         * Indicates an error occurred determining if a repository has unsaved changes.
         */
        ERROR_REPO_HAS_CHANGES,

        ERROR_SESSION_IS_CLOSED,

        /**
         * Indicates a transaction that had already been run is being run again.
         */
        ERROR_TRANSACTION_FINISHED,
        ERROR_TRYING_TO_COMMIT,
        SEQUENCING_ERROR_TRYING_TO_COMMIT,
        ERROR_TRYING_TO_ROLLBACK,
        ERROR_STOPPING_ENGINE,
        ERROR_STARTING_ENGINE,

        /**
         * An error message indicating the type of the object is not incorrect.
         */
        INCORRECT_TYPE,
        NO_ARTIFACT_DESCRIPTION,
        REMOVE_WORKSPACE_OBJECT_ERROR,

        /**
         * Security permission error messages
         */
        READ_NOT_ALLOWED,
        SET_PROPERTY_NOT_ALLOWED,
        ADD_REMOVE_CHILD_NOT_ALLOWED,
        REMOVE_NOT_ALLOWED,

        UNABLE_TO_CONSTRUCT_PROPERTY,
        UNABLE_TO_CONVERT_VALUE,
        UNABLE_TO_FIND_PROPERTY,
        UNABLE_TO_OBTAIN_NAME,
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
            return getEnumName( this ) + DOT + name();
        }
    }

    /**
     * Localized messages relating to validation.
     */
    public enum Validation {

        /**
         * Message when a rule is disabled rule is evaluated. One parameter: rule name.
         */
        ATTEMPT_TO_EVALUATE_DISABLED_RULE,

        /**
         * Message when the number of children of a specific type is above the maximum allowed. Five parameters: (1) node name,
         * (2) node path, (3) child count, (4) child type, (5) max allowed.
         */
        CHILD_COUNT_ABOVE_MAX_VALUE,

        /**
         * Message when the number of children of a specific type is below the minimum allowed. Five parameters: (1) node name,
         * (2) node path, (3) child count, (4) child type, (5) min allowed.
         */
        CHILD_COUNT_BELOW_MIN_VALUE,

        /**
         * Message when a child of a required type is not found. Three parameters: (1) child type, (2) node name, (3) node path.
         */
        CHILD_OF_REQUIRED_TYPE_NOT_FOUND,

        /**
         * Message when a number range rule does not declare a min or a max value. One parameter: rule name.
         */
        NUMBER_RULE_HAS_NO_VALUES,

        /**
         * Message when a number range rule declares a non-numeric value for either the min or max value. One parameter: rule
         * name.
         */
        NUMBER_RULE_NON_NUMERIC_VALUES,

        /**
         * Message when a node name is not value. Two parameters: (1) node name, (2) node path.
         */
        PATTERN_RULE_INVALID_NODE_NAME,

        /**
         * Message when a property value is not value. Three parameters: (1) node name, (2) node path, (3) property name.
         */
        PATTERN_RULE_INVALID_PROPERTY_VALUE,

        /**
         * Message when a property exists but a child of a specific type is found and is not allowed. Four parameters: (1) node
         * name, (2) node path, (3) property name, (4) child type.
         */
        PROPERTY_RULE_ABSENT_CHILD_FOUND,

        /**
         * Message when a property exists and another property exists that is not allowed. Four parameters: (1) node name, (2)
         * node path, (3) property name, (4) disallowed property name.
         */
        PROPERTY_RULE_ABSENT_PROPERTY_FOUND,

        /**
         * Message when a property exists and a required child of a specific type is not found. Four parameters: (1) node name,
         * (2) node path, (3) property name, (4) missing child type.
         */
        PROPERTY_RULE_REQUIRED_CHILD_NOT_FOUND,

        /**
         * Message when a property exists and another required property does not exist. Four parameters: (1) node name, (2) node
         * path, (3) property name, (4) missing property name.
         */
        PROPERTY_RULE_REQUIRED_PROPERTY_NOT_FOUND,

        /**
         * Message when a property value is above the maximum allowed. Five parameters: (1) node name, (2) node path, (3) property
         * name, (4) property value, (5) max value allowed.
         */
        PROPERTY_RULE_VALUE_ABOVE_MAX_VALUE,

        /**
         * Message when property value is below the minimum allowed. Five parameters: (1) node name, (2) node path, (3) property
         * name, (4) property value, (5) max allowed.
         */
        PROPERTY_RULE_VALUE_BELOW_MIN_VALUE,

        /**
         * Message when a child of a specific type exists but another child of another type exists but is not allowed. Four
         * parameters: (1) node name, (2) node path, (3) child type, (4) disallowed child type.
         */
        RELATIONSHIP_RULE_ABSENT_CHILD_FOUND,

        /**
         * Message when a child of a specific type exists and a specific property exists that is not allowed. Four parameters: (1)
         * node name, (2) node path, (3) child type, (4) disallowed property name.
         */
        RELATIONSHIP_RULE_ABSENT_PROPERTY_FOUND,

        /**
         * Message when a child of a specific type exists but another child of another type does not exist but should. Four
         * parameters: (1) node name, (2) node path, (3) child type, (4) missing child type.
         */
        RELATIONSHIP_RULE_REQUIRED_CHILD_NOT_FOUND,

        /**
         * Message when a child of a specific type exists and a specific required property is missing. Four parameters: (1) node
         * name, (2) node path, (3) child type, (4) missing property name.
         */
        RELATIONSHIP_RULE_REQUIRED_PROPERTY_NOT_FOUND,

        /**
         * Message when children of the same type and same nam exist. Four parameters: (1) node name, (2) node path, (3) node
         * name, (4) node type.
         */
        RELATIONSHIP_RULE_SNS_FOUND,

        /**
         * Message when a node is missing required property. Three parameters: (1) node name, (2) node path, (3) missing property
         * name.
         */
        REQUIRED_PROPERTY_NOT_FOUND,

        /**
         * Message when a rule message is missing. Two parameters: (1) node name, (2) message key.
         */
        RULE_MESSAGE_NOT_FOUND;

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

    private static String getEnumName( Enum< ? > enumValue ) {
        String className = enumValue.getClass().getName();
        String[] components = className.split( "\\$" ); //$NON-NLS-1$
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
    private static String getString( Enum< ? > key ) {
        try {
            return RESOURCE_BUNDLE.getString( key.toString() );
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
    public static String getString(Enum< ? > key, Object... parameters) {
        String text = getString( key );

        // Check the trivial cases ...
        if (text == null) {
            return OPEN_ANGLE_BRACKET + key.toString() + CLOSE_ANGLE_BRACKET;
        }
        if (parameters == null || parameters.length == 0) {
            return text;
        }

        return MessageFormat.format( text, parameters );
    }
}
