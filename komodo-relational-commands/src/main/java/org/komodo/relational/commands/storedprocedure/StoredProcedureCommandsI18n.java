/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.storedprocedure;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.storedprocedure}.
 */
@SuppressWarnings( "javadoc" )
public final class StoredProcedureCommandsI18n extends I18n {

    public static String addParameterExamples;
    public static String addParameterHelp;
    public static String addParameterUsage;

    public static String deleteParameterExamples;
    public static String deleteParameterHelp;
    public static String deleteParameterUsage;

    public static String removeResultSetExamples;
    public static String removeResultSetHelp;
    public static String removeResultSetUsage;

    public static String setResultSetExamples;
    public static String setResultSetHelp;
    public static String setResultSetUsage;

    public static String setStoredProcedurePropertyExamples;
    public static String setStoredProcedurePropertyHelp;
    public static String setStoredProcedurePropertyUsage;

    public static String unsetStoredProcedurePropertyExamples;
    public static String unsetStoredProcedurePropertyHelp;
    public static String unsetStoredProcedurePropertyUsage;

    public static String invalidResultSetType;
    public static String invalidSchemaElementTypePropertyValue;
    public static String missingParameterName;
    public static String missingResultSetType;
    public static String parameterAdded;
    public static String parameterDeleted;
    public static String resultSetRemoved;
    public static String resultSetRenameError;
    public static String resultSetTypeSet;

    static {
        final StoredProcedureCommandsI18n i18n = new StoredProcedureCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private StoredProcedureCommandsI18n() {
        // nothing to do
    }

}
