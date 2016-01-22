/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.spi.repository.validation;

import java.util.Comparator;
import java.util.Locale;


/**
 * Rule constants.
 *
 * @since 8.0
 */
public interface RuleConstants {

    /**
     * The model extension definition schema attribute names.
     */
    interface Attributes {
        String ID = "id"; //$NON-NLS-1$
        String KEY = "id"; //$NON-NLS-1$
        String LOCALE = "locale"; //$NON-NLS-1$
        String JCR_NAME = "jcrName"; //$NON-NLS-1$
        String INCLUSIVE = "inclusive"; //$NON-NLS-1$
        String SEVERITY = "severity"; //$NON-NLS-1$
        String MATCH_TYPE = "matchType"; //$NON-NLS-1$
        String VALUE = "value"; //$NON-NLS-1$
    }

    /**
     * Namespace-related names found in the model extension definition schema.
     */
    interface Namespaces {
        String NS_XSI = "xsi"; //$NON-NLS-1$
        String NS_MED = "p"; //$NON-NLS-1$
        String NS_XSI_VALUE = "http://www.w3.org/2001/XMLSchema-instance"; //$NON-NLS-1$
        String NS_MED_VALUE = "http://www.jboss.org/teiiddesigner/ext/2012"; //$NON-NLS-1$
        String NS_KEY = "http://www.w3.org/2000/xmlns/"; //$NON-NLS-1$
        String NS_SCHEMALOC = "xsi:schemaLocation"; //$NON-NLS-1$
    }

    /**
     * The model extension definition schema element names.
     */
    interface Elements {
        String VALIDATION_RULE_SET = "validationRuleSet"; //$NON-NLS-1$
        String NODE_VALIDATION = "nodeValidation"; //$NON-NLS-1$
        String NAME_VALIDATION = "nameValidation"; //$NON-NLS-1$
        String PROPERTY_VALIDATION = "propertyValidation"; //$NON-NLS-1$
        String REQUIRED_VALIDATION = "requiredValidation"; //$NON-NLS-1$
        String SAME_NAME_SIBLING_VALIDATION = "sameNameSiblingValidation"; //$NON-NLS-1$
        String CHILD_COUNT_VALIDATION = "childCountValidation"; //$NON-NLS-1$
        String VALUE_VALIDATION = "valueValidation"; //$NON-NLS-1$
        String RELATIONSHIP_VALIDATION = "relationshipValidation"; //$NON-NLS-1$
        String VALUE_RANGE_VALIDATION = "valueRangeValidation"; //$NON-NLS-1$
        String CHILD_VALIDATION = "childValidation"; //$NON-NLS-1$
        String PROP_RESTRICTION = "propRestriction"; //$NON-NLS-1$
        String DESCRIPTION = "description"; //$NON-NLS-1$
        String MESSAGE = "message"; //$NON-NLS-1$
        String PATTERN = "pattern"; //$NON-NLS-1$
        String PROP_EXISTS = "propExists"; //$NON-NLS-1$
        String PROP_ABSENT = "propAbsent"; //$NON-NLS-1$
        String CHILD_EXISTS = "childExists"; //$NON-NLS-1$
        String CHILD_ABSENT = "childAbsent"; //$NON-NLS-1$
        String MIN_VALUE = "minValue"; //$NON-NLS-1$
        String MAX_VALUE = "maxValue"; //$NON-NLS-1$
    }

    /**
     * Compares {@link Locale}s based on display language.
     */
    Comparator LOCALE_COMPARATOR = new Comparator<Locale>() {

        /**
         * @param thisLocale the first locale to be compared
         * @param thatLocale the second locale to be compared
         * @return a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the
         *         second
         */
        @Override
        public int compare( Locale thisLocale,
                            Locale thatLocale ) {
            return thisLocale.getDisplayLanguage().compareTo(thatLocale.getDisplayLanguage());
        }
    };

}
