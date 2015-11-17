/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.column;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.column}.
 */
@SuppressWarnings( "javadoc" )
public final class ColumnCommandsI18n extends I18n {

    public static String setColumnPropertyExamples;
    public static String setColumnPropertyHelp;
    public static String setColumnPropertyUsage;

    public static String unsetColumnPropertyExamples;
    public static String unsetColumnPropertyHelp;
    public static String unsetColumnPropertyUsage;

    public static String invalidSearchablePropertyValue;

    static {
        final ColumnCommandsI18n i18n = new ColumnCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private ColumnCommandsI18n() {
        // nothing to do
    }

}
