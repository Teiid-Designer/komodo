/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.datatyperesultset;

import java.util.ResourceBundle;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for {@link DataTypeResultSet}-related shell commands.
 */
@SuppressWarnings( "javadoc" )
public final class DataTypeResultSetCommandMessages implements StringConstants {

    public enum SetDataTypeResultSetPropertyCommand {

        INVALID_DATA_TYPE_ARRAY_INDICATOR,
        INVALID_DATATYPE_NAME;

        @Override
        public String toString() {
            return getEnumName( this ) + DOT + name();
        }

    }

    private static final String BUNDLE_NAME = ( DataTypeResultSetCommandMessages.class.getPackage().getName()
                                                + DOT
                                                + DataTypeResultSetCommandMessages.class.getSimpleName().toLowerCase() );

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
