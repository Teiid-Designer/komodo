/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.vdbimport;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.vdbimport} package.
 */
@SuppressWarnings( "javadoc" )
public final class VdbImportCommandsI18n extends I18n {

    public static String setVdbImportPropertyExamples;
    public static String setVdbImportPropertyHelp;
    public static String setVdbImportPropertyUsage;

    public static String unsetVdbImportPropertyExamples;
    public static String unsetVdbImportPropertyHelp;
    public static String unsetVdbImportPropertyUsage;

    static {
        final VdbImportCommandsI18n i18n = new VdbImportCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    public VdbImportCommandsI18n() {
        // nothing to do
    }

}
