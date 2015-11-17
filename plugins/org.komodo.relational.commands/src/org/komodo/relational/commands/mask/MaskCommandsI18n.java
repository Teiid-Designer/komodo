/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.mask;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.mask}.
 */
@SuppressWarnings( "javadoc" )
public final class MaskCommandsI18n extends I18n {

    public static String setMaskPropertyExamples;
    public static String setMaskPropertyHelp;
    public static String setMaskPropertyUsage;

    public static String unsetMaskPropertyExamples;
    public static String unsetMaskPropertyHelp;
    public static String unsetMaskPropertyUsage;

    static {
        final MaskCommandsI18n i18n = new MaskCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private MaskCommandsI18n() {
        // nothing to do
    }

}
