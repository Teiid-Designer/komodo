/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.uniqueconstraint;

import java.util.ResourceBundle;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for {@link UniqueConstraint}-related shell commands.
 */
public class UniqueConstraintCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( UniqueConstraintCommandMessages.class.getPackage().getName() + DOT + UniqueConstraintCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );
    
    @SuppressWarnings( "javadoc" )
    public enum General {
        MISSING_COLUMN_NAME;

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
