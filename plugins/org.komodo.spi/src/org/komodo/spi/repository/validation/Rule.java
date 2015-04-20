/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.spi.repository.validation;

import org.komodo.spi.KException;
import org.komodo.spi.outcome.Outcome.Level;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A rule used in validation of {@link KomodoObject}'s.
 */
public interface Rule {

    /**
     * The allowed keys for localized messages. The key names are used in the rule definition XML files when default messages
     * should be overwritten.
     */
    enum MessageKey {

        /**
         * The localized message key for the error message used when the count of children of a specific type is higher than the
         * required number. Value is {@value} .
         */
        CHILD_COUNT_ABOVE_MAX_VALUE,

        /**
         * The localized message key for the error message used when the count of children of a specific type is lower than the
         * required number. Value is {@value} .
         */
        CHILD_COUNT_BELOW_MIN_VALUE,

        /**
         * The localized message key for the error message used when a child of a required type is not found. Value is {@value} .
         */
        CHILD_OF_REQUIRED_TYPE_NOT_FOUND,

        /**
         * The localized message key for the description. Value is {@value} .
         */
        DESCRIPTION,

        /**
         * The localized message key for the error message used when a number rule has neither a min or max value the required
         * number. Value is {@value} .
         */
        NUMBER_RULE_HAS_NO_VALUES,

        /**
         * The localized message key for the error message used when a number rule has a min or max value that is not a number.
         * Value is {@value} .
         */
        NUMBER_RULE_NON_NUMERIC_VALUES,

        /**
         * The localized message key for the error message used when a node name is invalid. Value is {@value} .
         */
        PATTERN_RULE_INVALID_NODE_NAME,

        /**
         * The localized message key for the error message used when a property value is invalid. Value is {@value} .
         */
        PATTERN_RULE_INVALID_PROPERTY_VALUE,

        /**
         * The localized message key for the error message used when a property exists and a child exists with an absent
         * relationship node type.Value is * {@value} .
         */
        PROPERTY_RULE_ABSENT_CHILD_FOUND,

        /**
         * The localized message key for the error message used when a property exists and an absent relationship property exists.
         * Value is * {@value} .
         */
        PROPERTY_RULE_ABSENT_PROPERTY_FOUND,

        /**
         * The localized message key for the error message used when a property exists and a child with an absent child type
         * relationship exists. Value is * {@value} .
         */
        PROPERTY_RULE_REQUIRED_CHILD_NOT_FOUND,

        /**
         * The localized message key for the error message used when a property exists and a required relationship property does
         * not exist. Value is * {@value} .
         */
        PROPERTY_RULE_REQUIRED_PROPERTY_NOT_FOUND,

        /**
         * The localized message key for the error message used when a property value is above the maximum value. Value is * * * *
         * * {@value} .
         */
        PROPERTY_RULE_VALUE_ABOVE_MAX_VALUE,

        /**
         * The localized message key for the error message used when a property value is below the minimum value. Value is * * * *
         * * {@value} .
         */
        PROPERTY_RULE_VALUE_BELOW_MIN_VALUE,

        /**
         * The localized message key for the error message used when a relationship rule has a child with a type that it should
         * not have. Value is {@value} .
         */
        RELATIONSHIP_RULE_ABSENT_CHILD_FOUND,

        /**
         * The localized message key for the error message used when a relationship rule has a property that it should not have.
         * Value is {@value} .
         */
        RELATIONSHIP_RULE_ABSENT_PROPERTY_FOUND,

        /**
         * The localized message key for the error message used when a relationship rule does not have a child having a required
         * type. Value is {@value} .
         */
        RELATIONSHIP_RULE_REQUIRED_CHILD_NOT_FOUND,

        /**
         * The localized message key for the error message used when a relationship rule does not have a required property. Value
         * is {@value} .
         */
        RELATIONSHIP_RULE_REQUIRED_PROPERTY_NOT_FOUND,

        /**
         * The localized message key for the error message used when a relationship rule has found 2 children with the same name
         * and same type. Value is {@value} .
         */
        RELATIONSHIP_RULE_SNS_FOUND,

        /**
         * The localized message key for the error message used when a required property is not found. Value is {@value} .
         */
        REQUIRED_PROPERTY_NOT_FOUND;

    }

    /**
     * The type of rule.
     */
    enum RuleType {

        /**
         * Validates a number range. For example, a property value of the number of children of a specific type.
         */
        NUMBER,

        /**
         * Validates a string. For example, a node name or a property value.
         */
        PATTERN,

        /**
         * Validates the existence of properties and child nodes of specific types.
         */
        RELATIONSHIP,

        /**
         * Validates the existence of a property or a child of a specific node type.
         */
        REQUIRED,

        /**
         * Validates that sibling nodes do not have the same name.
         */
        SAME_NAME_SIBLING

    }

    /**
     * Indicates if the rule is validating a node, the children of a node, or a node property.
     */
    enum ValidationType {

        /**
         * Validation relating to children is being done.
         */
        CHILD,

        /**
         * Does not validate a specific property or child node type.
         */
        NODE,

        /**
         * A property is being validated.
         */
        PROPERTY

    }

    /**
     * @param transaction
     *        the transaction (can be <code>null</code> if query should be automatically committed)
     * @param kobject
     *        the object being evaluated (cannot be <code>null</code>)
     * @return the result (never <code>null</code>)
     * @throws KException
     *         if the object is not of the right type to be evaluated or if an error occurs
     */
    Result evaluate( final UnitOfWork transaction,
                     final KomodoObject kobject ) throws KException;

    /**
     * @return the localized rule description (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription() throws KException;

    /**
     * @return the fully qualified JCR name of the property or child node type being validated (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getJcrName() throws KException;

    /**
     * @param key
     *        the message key (cannot be empty)
     * @param args
     *        the message arguments (can be null or empty but not contain empty elements)
     * @return the localized rule message (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getMessage( final String key,
                       final String... args ) throws KException;

    /**
     * @return the unique identifier (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getName() throws KException;

    /**
     * @return the fully qualified JCR node type validated by this rule (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getNodeType() throws KException;

    /**
     * @return the rule type (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    RuleType getRuleType() throws KException;

    /**
     * @return the rule severity (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Level getSeverity() throws KException;

    /**
     * @return the validation type (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ValidationType getValidationType() throws KException;

    /**
     * @return <code>true</code> if rule is enabled and should be run
     * @throws KException
     *         if an error occurs
     */
    boolean isEnabled() throws KException;

    /**
     * @param newEnabled
     *        the new enabled indicator
     * @throws KException
     *         if an error occurs
     */
    void setEnabled( final boolean newEnabled ) throws KException;

    /**
     * @param newLevel
     *        the new severity (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setSeverity( final Level newLevel ) throws KException;

}
