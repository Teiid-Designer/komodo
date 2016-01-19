/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.parameter;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.parameter}.
 */
@SuppressWarnings( "javadoc" )
public final class ParameterCommandsI18n extends I18n {

    public static String setParameterPropertyExamples;
    public static String setParameterPropertyHelp;
    public static String setParameterPropertyUsage;

    public static String unsetParameterPropertyExamples;
    public static String unsetParameterPropertyHelp;
    public static String unsetParameterPropertyUsage;

    public static String invalidDirectionPropertyValue;
    public static String invalidNullablePropertyValue;

    static {
        final ParameterCommandsI18n i18n = new ParameterCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private ParameterCommandsI18n() {
        // nothing to do
    }

}
