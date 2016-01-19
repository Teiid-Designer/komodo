/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.teiid;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.teiid}.
 */
@SuppressWarnings( "javadoc" )
public final class TeiidCommandsI18n extends I18n {

    public static String setTeiidPropertyExamples;
    public static String setTeiidPropertyHelp;
    public static String setTeiidPropertyUsage;

    public static String unsetTeiidPropertyExamples;
    public static String unsetTeiidPropertyHelp;
    public static String unsetTeiidPropertyUsage;

    static {
        final TeiidCommandsI18n i18n = new TeiidCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private TeiidCommandsI18n() {
        // nothing to do
    }

}
