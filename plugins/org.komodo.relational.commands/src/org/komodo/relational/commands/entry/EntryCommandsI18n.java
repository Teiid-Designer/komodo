/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.entry;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.entry}.
 */
@SuppressWarnings( "javadoc" )
public final class EntryCommandsI18n extends I18n {

    public static String setEntryPropertyExamples;
    public static String setEntryPropertyHelp;
    public static String setEntryPropertyUsage;

    public static String unsetEntryPropertyExamples;
    public static String unsetEntryPropertyHelp;
    public static String unsetEntryPropertyUsage;

    static {
        final EntryCommandsI18n i18n = new EntryCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    public EntryCommandsI18n() {
        // nothing to do
    }

}
