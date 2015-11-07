/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.foreignkey;

import java.util.ResourceBundle;
import org.komodo.relational.model.ForeignKey;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for {@link ForeignKey}-related shell commands.
 */
@SuppressWarnings( "javadoc" )
public class ForeignKeyCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( ForeignKeyCommandMessages.class.getPackage().getName() + DOT + ForeignKeyCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    public enum AddReferenceColumnCommand {

        COLUMN_REF_ADDED,
        INVALID_COLUMN,
        MISSING_COLUMN_PATH;

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

    public enum DeleteReferenceColumnCommand {

        COLUMN_REMOVED,
        MISSING_COLUMN_PATH;

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

    private static String getEnumName(Enum<?> enumValue) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

}
