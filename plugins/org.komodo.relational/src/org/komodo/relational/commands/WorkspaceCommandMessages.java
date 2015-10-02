/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands;

import java.util.ResourceBundle;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for the WorkspaceCommandMessages
 */
@SuppressWarnings( "javadoc" )
public class WorkspaceCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( WorkspaceCommandMessages.class.getPackage().getName() + DOT + WorkspaceCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    public enum General {
        ERROR_WRITING_FILE,
        INVALID_BOOLEAN_PROPERTY_VALUE,
        INVALID_INTEGER_PROPERTY_VALUE,
        INVALID_OBJECT_TYPE,
        INVALID_PROPERTY_NAME,
        MISSING_OUTPUT_FILE_NAME,
        MISSING_PROPERTY_NAME_VALUE,
        MISSING_VDB_EXTERNAL_PATH,
        MISSING_VDB_NAME,
        NO_PROPERTIES,
        OUTPUT_FILE_ERROR,
        PROPERTIES_HEADER,
        PRINT_RELATIONAL_OBJECT,
        PROPERTY_NOT_SET,
        SET_PROPERTY_ERROR,
        SET_PROPERTY_SUCCESS,
        UNSET_MISSING_PROPERTY_NAME,
        UNSET_PROPERTY_ERROR,
        UNSET_PROPERTY_SUCCESS;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum CreateVdbCommand {
        CREATE_VDB_ERROR,
        MISSING_VDB_EXTERNAL_PATH,
        MISSING_VDB_NAME,
        VDB_CREATED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum CreateTeiidCommand {
        CREATE_TEIID_ERROR,
        MISSING_TEIID_NAME,
        TEIID_CREATED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum CreateSchemaCommand {
        CREATE_SCHEMA_ERROR,
        MISSING_SCHEMA_NAME,
        SCHEMA_CREATED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum FindCommand {
        MISSING_TYPE_NAME,
        HELP_TYPES_HEADING,
        FAILURE,
        INVALID_TYPE,
        NO_OBJECTS_FOUND,
        TYPE_HEADER;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum AddConstraintColumnCommand {
        ADD_COLUMN_ERROR,
        COLUMN_REF_ADDED,
        COLUMN_PATH_NOT_FOUND,
        ERROR,
        INVALID_COLUMN_PATH,
        INVALID_COLUMN,
        MISSING_COLUMN_PATH;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    public enum DeleteConstraintColumnCommand {
        COLUMN_REF_REMOVED,
        COLUMN_PATH_NOT_FOUND,
        DELETE_COLUMN_ERROR,
        INVALID_COLUMN_PATH,
        MISSING_COLUMN_PATH;

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

//    private WorkspaceCommandMessages() {
//    }
//
//    /**
//     * Get message string
//     *
//     * @param key
//     *
//     * @return i18n string
//     */
//    private static String getString(Enum<?> key) {
//        try {
//            return RESOURCE_BUNDLE.getString(key.toString());
//        } catch (final Exception err) {
//            String msg;
//
//            if (err instanceof NullPointerException) {
//                msg = "<No message available>"; //$NON-NLS-1$
//            } else if (err instanceof MissingResourceException) {
//                msg = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
//            } else {
//                msg = err.getLocalizedMessage();
//            }
//
//            return msg;
//        }
//    }
//
//    /**
//     * Get message string with parameters
//     *
//     * @param key the enum key
//     * @param parameters parameters
//     *
//     * @return i18n string
//     */
//    public static String getString(Enum<?> key, Object... parameters) {
//        String text = getString(key);
//
//        // Check the trivial cases ...
//        if (text == null) {
//            return OPEN_ANGLE_BRACKET + key.toString() + CLOSE_ANGLE_BRACKET;
//        }
//        if (parameters == null || parameters.length == 0) {
//            return text;
//        }
//
//        return MessageFormat.format(text, parameters);
//    }
//
//    /**
//     * Look up a message in the i18n resource message bundle by key, then format the
//     * message with the given params and return the result.
//     * @param key the message key
//     * @param parameters the parameters
//     * @return the message
//     */
//    public static String getString(String key, Object ... parameters) {
//        String text = null;
//        try {
//            text = RESOURCE_BUNDLE.getString(key);
//        } catch (final Exception err) {
//            if (err instanceof NullPointerException) {
//                text = "<No message available>"; //$NON-NLS-1$
//            } else if (err instanceof MissingResourceException) {
//                text = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
//            } else {
//                text = err.getLocalizedMessage();
//            }
//        }
//
//        // Check the trivial cases ...
//        if (text == null) {
//            return OPEN_ANGLE_BRACKET + key + CLOSE_ANGLE_BRACKET;
//        }
//        if (parameters == null || parameters.length == 0) {
//            return text;
//        }
//
//        return MessageFormat.format(text, parameters);
//    }
}
