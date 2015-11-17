/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.virtualprocedure;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.virtualprocedure}.
 */
@SuppressWarnings( "javadoc" )
public final class VirtualProcedureCommandsI18n extends I18n {

    public static String addParameterExamples;
    public static String addParameterHelp;
    public static String addParameterUsage;

    public static String deleteParameterExamples;
    public static String deleteParameterHelp;
    public static String deleteParameterUsage;

    public static String setVirtualProcedurePropertyExamples;
    public static String setVirtualProcedurePropertyHelp;
    public static String setVirtualProcedurePropertyUsage;

    public static String unsetVirtualProcedurePropertyExamples;
    public static String unsetVirtualProcedurePropertyHelp;
    public static String unsetVirtualProcedurePropertyUsage;

    public static String missingParameterName;
    public static String invalidSchemaElementTypePropertyValue;
    public static String parameterAdded;
    public static String parameterDeleted;

    static {
        final VirtualProcedureCommandsI18n i18n = new VirtualProcedureCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private VirtualProcedureCommandsI18n() {
        // nothing to do
    }

}
