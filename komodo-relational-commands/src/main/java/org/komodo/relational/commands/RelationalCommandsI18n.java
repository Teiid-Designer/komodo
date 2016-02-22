/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands}.
 */
@SuppressWarnings( "javadoc" )
public final class RelationalCommandsI18n extends I18n {

    public static String findExamples;
    public static String findHelp;
    public static String findUsage;

    public static String setCustomPropertyHelp;
    public static String setCustomPropertyExamples;
    public static String setCustomPropertyUsage;

    public static String setCustomOptionHelp;
    public static String setCustomOptionExamples;
    public static String setCustomOptionUsage;

    public static String unsetCustomOptionHelp;
    public static String unsetCustomOptionExamples;
    public static String unsetCustomOptionUsage;

    public static String unsetCustomPropertyExamples;
    public static String unsetCustomPropertyHelp;
    public static String unsetCustomPropertyUsage;

    public static String invalidType;
    public static String missingPropertyNameValue;
    public static String missingOptionNameValue;
    public static String missingTypeName;
    public static String noObjectsFound;
    public static String noObjectsFoundForPattern;
    public static String relationalCommandCategory;
    public static String setPropertySuccess;
    public static String setCustomOptionSuccess;
    public static String typeHeader;
    public static String typeHeaderForPattern;
    public static String unsetMissingPropertyName;
    public static String unsetMissingOptionName;
    public static String unsetPropertySuccess;
    public static String unsetCustomOptionSuccess;
    public static String useSetPropertyCommand;

    static {
        final RelationalCommandsI18n i18n = new RelationalCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private RelationalCommandsI18n() {
        // nothing to do
    }

}
