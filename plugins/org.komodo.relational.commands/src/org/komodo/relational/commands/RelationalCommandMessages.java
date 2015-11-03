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
public class RelationalCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( RelationalCommandMessages.class.getPackage().getName() + DOT + RelationalCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    public enum General {
        MISSING_PROPERTY_NAME_VALUE,
        SET_PROPERTY_SUCCESS,
        UNSET_MISSING_PROPERTY_NAME,
        UNSET_PROPERTY_SUCCESS;

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

    private static String getEnumName(Enum<?> enumValue) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

}
