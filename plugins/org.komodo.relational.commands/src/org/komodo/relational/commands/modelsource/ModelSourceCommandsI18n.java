/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.modelsource;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.relational.commands.modelsource}.
 */
@SuppressWarnings( "javadoc" )
public final class ModelSourceCommandsI18n extends I18n {

    public static String setModelSourcePropertyExamples;
    public static String setModelSourcePropertyHelp;
    public static String setModelSourcePropertyUsage;

    public static String unsetModelSourcePropertyExamples;
    public static String unsetModelSourcePropertyHelp;
    public static String unsetModelSourcePropertyUsage;

    static {
        final ModelSourceCommandsI18n i18n = new ModelSourceCommandsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private ModelSourceCommandsI18n() {
        // nothing to do
    }

}
