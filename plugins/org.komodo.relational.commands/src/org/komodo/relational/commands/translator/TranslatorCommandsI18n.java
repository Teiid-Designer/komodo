/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.translator;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.translator}.
 */
@SuppressWarnings( "javadoc" )
public final class TranslatorCommandsI18n extends I18n {

    public static String setTranslatorPropertyExamples;
    public static String setTranslatorPropertyHelp;
    public static String setTranslatorPropertyUsage;

    public static String unsetTranslatorPropertyExamples;
    public static String unsetTranslatorPropertyHelp;
    public static String unsetTranslatorPropertyUsage;

    static {
        final TranslatorCommandsI18n i18n = new TranslatorCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private TranslatorCommandsI18n() {
        // nothing to do
    }

}
