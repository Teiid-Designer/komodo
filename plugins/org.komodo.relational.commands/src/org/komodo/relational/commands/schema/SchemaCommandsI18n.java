/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.schema;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.schema}.
 */
@SuppressWarnings( "javadoc" )
public final class SchemaCommandsI18n extends I18n {

    public static String exportExamples;
    public static String exportHelp;
    public static String exportUsage;

    public static String setSchemaPropertyExamples;
    public static String setSchemaPropertyHelp;
    public static String setSchemaPropertyUsage;

    public static String unsetSchemaPropertyExamples;
    public static String unsetSchemaPropertyHelp;
    public static String unsetSchemaPropertyUsage;

    public static String ddlExported;

    static {
        final SchemaCommandsI18n i18n = new SchemaCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private SchemaCommandsI18n() {
        // nothing to do
    }

}
