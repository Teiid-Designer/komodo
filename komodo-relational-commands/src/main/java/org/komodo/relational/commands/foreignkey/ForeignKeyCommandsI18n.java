/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.foreignkey;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.foreignkey}.
 */
@SuppressWarnings( "javadoc" )
public final class ForeignKeyCommandsI18n extends I18n {

    public static String addReferenceColumnExamples;
    public static String addReferenceColumnHelp;
    public static String addReferenceColumnUsage;

    public static String deleteReferenceColumnExamples;
    public static String deleteReferenceColumnHelp;
    public static String deleteReferenceColumnUsage;

    public static String columnRefAdded;
    public static String columnRemoved;
    public static String invalidColumn;
    public static String invalidColumnPath;
    public static String missingColumnPathForAdd;
    public static String missingColumnPathForDelete;

    static {
        final ForeignKeyCommandsI18n i18n = new ForeignKeyCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private ForeignKeyCommandsI18n() {
        // nothing to do
    }

}
