/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.api;

import org.komodo.utils.i18n.I18n;

/**
 * Localized messages for the {@link org.komodo.shell.api} package.
 */
@SuppressWarnings( "javadoc" )
public final class ShellApiI18n extends I18n {

    public static String generalCommandCategory;
    public static String invalidFinalArg;
    public static String negativeIndentSupplied;

    static {
        final ShellApiI18n i18n = new ShellApiI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    public ShellApiI18n() {
        // nothing to do
    }

}
