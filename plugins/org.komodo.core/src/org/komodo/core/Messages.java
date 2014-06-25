/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.core;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

/**
 *
 */
public class Messages implements StringConstants {

    private static final String BUNDLE_NAME = Messages.class.getPackage().getName()
    																					+ DOT
    																					+ Messages.class.getSimpleName().toLowerCase();

    private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle(BUNDLE_NAME);

    @SuppressWarnings( "javadoc" )
    public enum CORE {
    	StringUtil_Displayable,
    	Assertion_isEqual,
    	Assertion_isNotEqual,
    	CoreArgCheck_isNonNegativeInt,
		CoreArgCheck_isNonPositiveInt,
		CoreArgCheck_isNegativeInt,
		CoreArgCheck_isPositiveInt,
		CoreArgCheck_isStringNonZeroLength,
		CoreArgCheck_isNonNull,
		CoreArgCheck_isNull,
		CoreArgCheck_isInstanceOf,
		CoreArgCheck_isCollectionNotEmpty,
		CoreArgCheck_isPropertiesNotEmpty,
		CoreArgCheck_isMapNotEmpty,
		CoreArgCheck_isArrayNotEmpty,
		CoreArgCheck_isNotSame,
		CoreArgCheck_contains,
		CoreArgCheck_containsKey,
		StringNameValidator_MinLengthFailure,
		StringNameValidator_The_name_length__is_longer_than_allowed,
		StringNameValidator_The_first_character_of_the_name__must_be_an_alphabetic_character,
		StringNameValidator_The_character_at_position_is_not_allowed,
		StringNameValidator_or_other_valid_characters,
		StringNameValidator_The_name_may_not_be_null,
		StringNameValidator_The_name_is_the_same_as_other_objects_under_the_same_parent,
		StringNameValidator_sameNameCaseSensitive,
		StringNameValidator_The_minimum_length_may_not_exceed_the_maximum_length,
		StringNameValidator_Unable_to_make_the_name_unique_within_the_limits_of_the_maximum_length,
		StringNameValidator_The_character_is_not_a_valid_character,
        StringNameValidator_unquotedNameWithDelimiter;

        @Override
        public String toString() {
            return getEnumName(this) + DOT + name();
        }
    }

    private static String getEnumName(Enum<?> enumValue) {
        String className = enumValue.getClass().getName();
        String[] components = className.split("\\$"); //$NON-NLS-1$
        return components[components.length - 1];
    }

    private Messages() {
    }

    /**
     * Get message string
     *
     * @param key
     *
     * @return i18n string
     */
    private static String getString(Enum<?> key) {
        try {
            return RESOURCE_BUNDLE.getString(key.toString());
        } catch (final Exception err) {
            String msg;

            if (err instanceof NullPointerException) {
                msg = "<No message available>"; //$NON-NLS-1$
            } else if (err instanceof MissingResourceException) {
                msg = OPEN_ANGLE_BRACKET + "Missing message for key \"" + key + "\" in: " + BUNDLE_NAME + CLOSE_ANGLE_BRACKET; //$NON-NLS-1$ //$NON-NLS-2$
            } else {
                msg = err.getLocalizedMessage();
            }

            return msg;
        }
    }

    /**
     * Get message string with parameters
     *
     * @param key
     * @param parameters
     *
     * @return i18n string
     */
    public static String getString(Enum<?> key, Object... parameters) {
        String text = getString(key);

        // Check the trivial cases ...
        if (text == null) {
            return OPEN_ANGLE_BRACKET + key.toString() + CLOSE_ANGLE_BRACKET;
        }
        if (parameters == null || parameters.length == 0) {
            return text;
        }

        return MessageFormat.format(text, parameters);
    }
}
