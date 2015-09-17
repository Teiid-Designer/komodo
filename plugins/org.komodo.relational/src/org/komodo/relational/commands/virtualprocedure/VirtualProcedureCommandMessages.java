/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.virtualprocedure;

import java.util.ResourceBundle;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for {@link VirtualProcedure}-related shell commands.
 */
public class VirtualProcedureCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( VirtualProcedureCommandMessages.class.getPackage().getName() + DOT + VirtualProcedureCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );
    
    @SuppressWarnings( "javadoc" )
    public enum General {
        MISSING_PARAMETER_NAME,
        PARAMETER_NAME;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }
    
    @SuppressWarnings( "javadoc" )
    public enum AddParameterCommand {
        PARAMETER_ADDED;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    @SuppressWarnings( "javadoc" )
    public enum DeleteParameterCommand {
        PARAMETER_DELETED;

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
