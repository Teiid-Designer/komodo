/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.tabularresultset;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.tabularresultset}.
 */
@SuppressWarnings( "javadoc" )
public final class TabularResultSetCommandsI18n extends I18n {

    public static String addColumnExamples;
    public static String addColumnHelp;
    public static String addColumnUsage;

    public static String deleteColumnExamples;
    public static String deleteColumnHelp;
    public static String deleteColumnUsage;

    public static String columnAdded;
    public static String columnDeleted;
    public static String missingColumnName;

    static {
        final TabularResultSetCommandsI18n i18n = new TabularResultSetCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private TabularResultSetCommandsI18n() {
        // nothing to do
    }

}
