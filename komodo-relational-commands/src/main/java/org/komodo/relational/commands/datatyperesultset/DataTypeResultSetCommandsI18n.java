/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.datatyperesultset;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.datatyperesultset}.
 */
@SuppressWarnings( "javadoc" )
public final class DataTypeResultSetCommandsI18n extends I18n {

    public static String setDataTypeResultSetPropertyExamples;
    public static String setDataTypeResultSetPropertyHelp;
    public static String setDataTypeResultSetPropertyUsage;

    public static String unsetDataTypeResultSetPropertyExamples;
    public static String unsetDataTypeResultSetPropertyHelp;
    public static String unsetDataTypeResultSetPropertyUsage;

    public static String invalidDataTypeArrayIndicator;
    public static String invalidDataTypeName;

    static {
        final DataTypeResultSetCommandsI18n i18n = new DataTypeResultSetCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private DataTypeResultSetCommandsI18n() {
        // nothing to do
    }

}
