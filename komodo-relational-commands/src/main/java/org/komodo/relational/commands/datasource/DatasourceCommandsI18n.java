/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.datasource;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.datasource}.
 */
@SuppressWarnings( "javadoc" )
public final class DatasourceCommandsI18n extends I18n {

    public static String setDatasourcePropertyExamples;
    public static String setDatasourcePropertyHelp;
    public static String setDatasourcePropertyUsage;

    public static String unsetDatasourcePropertyExamples;
    public static String unsetDatasourcePropertyHelp;
    public static String unsetDatasourcePropertyUsage;

    static {
        final DatasourceCommandsI18n i18n = new DatasourceCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private DatasourceCommandsI18n() {
        // nothing to do
    }

}
