/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.tableconstraint;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.tableconstraint}.
 */
@SuppressWarnings( "javadoc" )
public final class TableConstraintCommandsI18n extends I18n {

    public static String addConstraintColumnExamples;
    public static String addConstraintColumnHelp;
    public static String addConstraintColumnUsage;

    public static String deleteConstraintColumnExamples;
    public static String deleteConstraintColumnHelp;
    public static String deleteConstraintColumnUsage;

    public static String columnRefAdded;
    public static String columnRemoved;
    public static String invalidColumn;
    public static String invalidColumnPath;
    public static String missingColumnPathForAdd;
    public static String missingColumnPathForDelete;

    static {
        final TableConstraintCommandsI18n i18n = new TableConstraintCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private TableConstraintCommandsI18n() {
        // nothing to do
    }

}
