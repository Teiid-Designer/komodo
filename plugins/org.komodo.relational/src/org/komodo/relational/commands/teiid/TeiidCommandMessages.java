/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.commands.teiid;

import java.util.ResourceBundle;
import org.komodo.relational.teiid.Teiid;
import org.komodo.spi.constants.StringConstants;

/**
 * Localized messages for {@link Teiid}-related shell commands.
 */
public class TeiidCommandMessages implements StringConstants {

    private static final String BUNDLE_NAME = ( TeiidCommandMessages.class.getPackage().getName() + DOT + TeiidCommandMessages.class.getSimpleName().toLowerCase() );

    /**
     * The resource bundle for localized messages.
     */
    public static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

    private static String getEnumName(Enum<?> enumValue) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

}