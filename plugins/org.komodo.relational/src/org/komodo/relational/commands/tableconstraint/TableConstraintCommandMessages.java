/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.tableconstraint;

import java.util.ResourceBundle;
import org.komodo.relational.model.TableConstraint;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for {@link TableConstraint}-related shell commands.
 */
@SuppressWarnings( "javadoc" )
public final class TableConstraintCommandMessages implements StringConstants {

    public enum AddConstraintColumnCommand {

        COLUMN_REF_ADDED,
        COLUMN_PATH_NOT_FOUND,
        ERROR,
        INVALID_COLUMN_PATH,
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

    public enum DeleteConstraintColumnCommand {

        COLUMN_REF_REMOVED,
        COLUMN_PATH_NOT_FOUND,
        INVALID_COLUMN_PATH,
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

    private static final String BUNDLE_NAME = ( TableConstraintCommandMessages.class.getPackage().getName()
                                                + DOT
                                                + TableConstraintCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    private static String getEnumName( final Enum< ? > enumValue ) {
        final String className = enumValue.getClass().getName();
        final String[] components = className.split( "\\$" ); //$NON-NLS-1$
        return components[ components.length - 1 ];
    }

}
