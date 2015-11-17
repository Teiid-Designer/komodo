/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.pushdownfunction;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.pushdownfunction}.
 */
@SuppressWarnings( "javadoc" )
public final class PushdownFunctionCommandsI18n extends I18n {

    public static String addParameterExamples;
    public static String addParameterHelp;
    public static String addParameterUsage;

    public static String deleteParameterExamples;
    public static String deleteParameterHelp;
    public static String deleteParameterUsage;

    public static String removeResultSetExamples;
    public static String removeResultSetHelp;
    public static String removeResultSetUsage;

    public static String setPushdownFunctionPropertyExamples;
    public static String setPushdownFunctionPropertyHelp;
    public static String setPushdownFunctionPropertyUsage;

    public static String setResultSetExamples;
    public static String setResultSetHelp;
    public static String setResultSetUsage;

    public static String unsetPushdownFunctionPropertyExamples;
    public static String unsetPushdownFunctionPropertyHelp;
    public static String unsetPushdownFunctionPropertyUsage;

    public static String invalidDeterministicPropertyValue;
    public static String invalidResultSetType;
    public static String invalidSchemaElementTypePropertyValue;
    public static String missingParameterName;
    public static String missingResultSetType;
    public static String parameterAdded;
    public static String parameterDeleted;
    public static String resultSetRemoved;
    public static String resultSetTypeSet;

    static {
        final PushdownFunctionCommandsI18n i18n = new PushdownFunctionCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private PushdownFunctionCommandsI18n() {
        // nothing to do
    }

}
