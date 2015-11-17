/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.index;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.index}.
 */
@SuppressWarnings( "javadoc" )
public final class IndexCommandsI18n extends I18n {

    public static String setIndexPropertyExamples;
    public static String setIndexPropertyHelp;
    public static String setIndexPropertyUsage;

    public static String unsetIndexPropertyExamples;
    public static String unsetIndexPropertyHelp;
    public static String unsetIndexPropertyUsage;

    static {
        final IndexCommandsI18n i18n = new IndexCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private IndexCommandsI18n() {
        // nothing to do
    }

}
