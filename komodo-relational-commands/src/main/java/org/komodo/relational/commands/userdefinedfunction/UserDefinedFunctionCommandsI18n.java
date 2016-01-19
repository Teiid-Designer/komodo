/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.userdefinedfunction;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.userdefinedfunction}.
 */
@SuppressWarnings( "javadoc" )
public final class UserDefinedFunctionCommandsI18n extends I18n {

    public static String addParameterExamples;
    public static String addParameterHelp;
    public static String addParameterUsage;

    public static String deleteParameterExamples;
    public static String deleteParameterHelp;
    public static String deleteParameterUsage;

    public static String setUserDefinedFunctionPropertyExamples;
    public static String setUserDefinedFunctionPropertyHelp;
    public static String setUserDefinedFunctionPropertyUsage;

    public static String unsetUserDefinedFunctionPropertyExamples;
    public static String unsetUserDefinedFunctionPropertyHelp;
    public static String unsetUserDefinedFunctionPropertyUsage;

    public static String invalidDeterministicPropertyValue;
    public static String invalidSchemaElementTypePropertyValue;
    public static String missingParameterName;
    public static String parameterAdded;
    public static String parameterDeleted;

    static {
        final UserDefinedFunctionCommandsI18n i18n = new UserDefinedFunctionCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private UserDefinedFunctionCommandsI18n() {
        // nothing to do
    }

}
