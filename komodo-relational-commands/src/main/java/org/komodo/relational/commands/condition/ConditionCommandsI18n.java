/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.condition;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.condition}.
 */
@SuppressWarnings( "javadoc" )
public final class ConditionCommandsI18n extends I18n {

    public static String setConditionPropertyExamples;
    public static String setConditionPropertyHelp;
    public static String setConditionPropertyUsage;

    public static String unsetConditionPropertyExamples;
    public static String unsetConditionPropertyHelp;
    public static String unsetConditionPropertyUsage;

    static {
        final ConditionCommandsI18n i18n = new ConditionCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    public ConditionCommandsI18n() {
        // nothing to do
    }

}
