/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.resultsetcolumn;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.resultsetcolumn}.
 */
@SuppressWarnings( "javadoc" )
public final class ResultSetColumnCommandsI18n extends I18n {

    public static String setResultSetColumnPropertyExamples;
    public static String setResultSetColumnPropertyHelp;
    public static String setResultSetColumnPropertyUsage;

    public static String unsetResultSetColumnPropertyExamples;
    public static String unsetResultSetColumnPropertyHelp;
    public static String unsetResultSetColumnPropertyUsage;

    static {
        final ResultSetColumnCommandsI18n i18n = new ResultSetColumnCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private ResultSetColumnCommandsI18n() {
        // nothing to do
    }

}
