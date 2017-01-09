/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.spi.repository.validation;

import java.util.Map;
import org.komodo.spi.KException;
import org.komodo.spi.outcome.Outcome.Level;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;

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
     * Indicates property restriction type
     */
    enum PropertyRestriction {

        /**
         * Property restriction applies to a CHILD validation.
         */
        CHILD,

        /**
         * Property restriction applies to a NODE validation.
         */
        NODE
    }

    /**
     * An empty array of rules.
     */
    Rule[] NO_RULES = new Rule[0];

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param kobject
     *        the object being evaluated (cannot be <code>null</code>)
     * @return the result (never <code>null</code>)
     * @throws KException
     *         if the object is not of the right type to be evaluated or if an error occurs
     */
    Result evaluate( final UnitOfWork transaction,
                     final KomodoObject kobject ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the localized rule description (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getDescription( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the fully qualified JCR name of the property or child node type being validated (can be empty)
     * @throws KException
     *         if an error occurs
     */
    String getJcrName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param key
     *        the message key (cannot be empty)
     * @return the localized rule message (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getMessage( final UnitOfWork transaction,
                       final String key ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the unique identifier (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getName( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the fully qualified JCR node type validated by this rule (never empty)
     * @throws KException
     *         if an error occurs
     */
    String getNodeType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param restrictionType the restriction type (NODE or CHILD).
     * @return the map of property name-value that the node type must possess. (never empty)
     * @throws KException
     *         if an error occurs
     */
    Map<String,String> getPropRestrictions( final UnitOfWork transaction, PropertyRestriction restrictionType ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the rule type (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    RuleType getRuleType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the rule severity (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Level getSeverity( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return the validation type (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    ValidationType getValidationType( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if the property or child that this rule pertains to is required.
     * @throws KException
     *         if an error occurs
     */
    boolean isRequired( final UnitOfWork transaction ) throws KException;

    /**
     * Determine if the rule applies to the supplied KomodoObject.  
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param kObject
     *         the KomodoObject to test
     * @return <code>true</code> if the rule applies to the supplied object.
     * @throws KException
     *         if an error occurs
     */
    boolean isApplicable( final UnitOfWork transaction, final KomodoObject kObject ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newRequired
     *        the new required indicator
     * @throws KException
     *         if an error occurs
     */
    void setRequired( final UnitOfWork transaction,
                      final boolean newRequired ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @return <code>true</code> if rule is enabled and should be run
     * @throws KException
     *         if an error occurs
     */
    boolean isEnabled( final UnitOfWork transaction ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newEnabled
     *        the new enabled indicator
     * @throws KException
     *         if an error occurs
     */
    void setEnabled( final UnitOfWork transaction,
                     final boolean newEnabled ) throws KException;

    /**
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param newLevel
     *        the new severity (cannot be <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    void setSeverity( final UnitOfWork transaction,
                      final Level newLevel ) throws KException;

}
