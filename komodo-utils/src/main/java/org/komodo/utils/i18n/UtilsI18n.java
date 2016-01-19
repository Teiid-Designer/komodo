/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.utils.i18n;

/**
 * Localized messages for the {@link org.komodo.utils.i18n} package.
 */
@SuppressWarnings( "javadoc" )
public class UtilsI18n extends I18n {

    public static String missingI18Field;
    public static String missingPropertiesKey;
    public static String problemAccessingI18Field;
    public static String problemLoadingI18nClass;
    public static String problemLoadingI18nProperties;

    static {
        final UtilsI18n i18n = new UtilsI18n();
        i18n.initialize();
    }

    /**
     * Don't allow construction outside of this class.
     */
    private UtilsI18n() {
        // nothing to do
    }

}
