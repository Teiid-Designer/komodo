/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.spi.repository;

import java.io.File;
import java.util.List;
import org.komodo.spi.KException;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.spi.utils.LocalizedMessage;

/**
 * Stores Komodo environment settings and preferences.
 */
public interface ValidationManager {

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param propertyName
     *        the name of the property that is required (cannot be empty)
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Rule addPropertyRequiredRule( final UnitOfWork transaction,
                                  final String name,
                                  final String nodeType,
                                  final String propertyName,
                                  final List< LocalizedMessage > descriptions,
                                  final List< LocalizedMessage > messages ) throws KException;

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param childType
     *        the name of the child type that must have at least one child (cannot be empty)
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Rule addChildTypeRequiredRule( final UnitOfWork transaction,
                                   final String name,
                                   final String nodeType,
                                   final String childType,
                                   final List< LocalizedMessage > descriptions,
                                   final List< LocalizedMessage > messages ) throws KException;

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param propertyName
     *        the name of the property whose value is being validated (cannot be empty)
     * @param pattern
     *        the regular expression that the property value must match (cannot be empty)
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Rule addPropertyPatternRule( final UnitOfWork transaction,
                                 final String name,
                                 final String nodeType,
                                 final String propertyName,
                                 final String pattern,
                                 final List< LocalizedMessage > descriptions,
                                 final List< LocalizedMessage > messages ) throws KException;

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param pattern
     *        the regular expression that the child node name must match (cannot be empty)
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Rule addNodeNameRule( final UnitOfWork transaction,
                          final String name,
                          final String nodeType,
                          final String pattern,
                          final List< LocalizedMessage > descriptions,
                          final List< LocalizedMessage > messages ) throws KException;

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param propertyName
     *        the name of the property whose value range is being validated (cannot be empty)
     * @param minValue
     *        the minimum allowed value (cannot be <code>null</code>)
     * @param minInclusive
     *        <code>true</code> if the property value can equal the minimum value
     * @param maxValue
     *        the maximum allowed value (cannot be <code>null</code>)
     * @param maxInclusive
     *        <code>true</code> if the property value can equal the maximum value
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Rule addPropertyValueNumberValidationRule( final UnitOfWork transaction,
                                               final String name,
                                               final String nodeType,
                                               final String propertyName,
                                               final Number minValue,
                                               final boolean minInclusive,
                                               final Number maxValue,
                                               final boolean maxInclusive,
                                               final List< LocalizedMessage > descriptions,
                                               final List< LocalizedMessage > messages ) throws KException;

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param childType
     *        the name of the child type whose child count range is being validated (cannot be empty)
     * @param minValue
     *        the minimum allowed number of children with the specified type (cannot be <code>null</code>)
     * @param minInclusive
     *        <code>true</code> if the number of children can equal the minimum value
     * @param maxValue
     *        the maximum allowed number of children with the specified type (cannot be <code>null</code>)
     * @param maxInclusive
     *        <code>true</code> if the number of children can equal the maximum value
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Rule addChildCountValidationRule( final UnitOfWork transaction,
                                      final String name,
                                      final String nodeType,
                                      final String childType,
                                      final Number minValue,
                                      final boolean minInclusive,
                                      final Number maxValue,
                                      final boolean maxInclusive,
                                      final List< LocalizedMessage > descriptions,
                                      final List< LocalizedMessage > messages ) throws KException;

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param matchType
     *        <code>true</code> if only children of the same type can't have the same name
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Rule addSameNameSiblingValidationRule( final UnitOfWork transaction,
                                           final String name,
                                           final String nodeType,
                                           final boolean matchType,
                                           final List< LocalizedMessage > descriptions,
                                           final List< LocalizedMessage > messages ) throws KException;

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param propertyName
     *        the property whose relationships are being validated (cannot be empty)
     * @param propsThatMustExist
     *        a list of properties that must exist (can be <code>null</code> or empty)
     * @param propsThatMustNotExist
     *        a list of properties that must NOT exist (can be <code>null</code> or empty)
     * @param childTypesThatMustExist
     *        a list of node types that at least one child must have (can be <code>null</code> or empty)
     * @param childTypesThatMustNotExist
     *        a list of node types that no child must have (can be <code>null</code> or empty)
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Rule addPropertyRelationshipValidationRule( final UnitOfWork transaction,
                                                final String name,
                                                final String nodeType,
                                                final String propertyName,
                                                final List< String > propsThatMustExist,
                                                final List< String > propsThatMustNotExist,
                                                final List< String > childTypesThatMustExist,
                                                final List< String > childTypesThatMustNotExist,
                                                final List< LocalizedMessage > descriptions,
                                                final List< LocalizedMessage > messages ) throws KException;

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param childType
     *        the node type whose relationships are being validated (cannot be empty)
     * @param propsThatMustExist
     *        a list of properties that must exist if a child with the specified type exists (can be <code>null</code> or empty)
     * @param propsThatMustNotExist
     *        a list of properties that must NOT exist if a child with the specified type exists (can be <code>null</code> or
     *        empty)
     * @param childTypesThatMustExist
     *        a list of node types that at least one child must have if a child with the specified type exists (can be
     *        <code>null</code> or empty)
     * @param childTypesThatMustNotExist
     *        a list of node types that no child must have if a child with the specified type exists (can be <code>null</code> or
     *        empty)
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    Rule addChildRelationshipValidationRule( final UnitOfWork transaction,
                                             final String name,
                                             final String nodeType,
                                             final String childType,
                                             final List< String > propsThatMustExist,
                                             final List< String > propsThatMustNotExist,
                                             final List< String > childTypesThatMustExist,
                                             final List< String > childTypesThatMustNotExist,
                                             final List< LocalizedMessage > descriptions,
                                             final List< LocalizedMessage > messages ) throws KException;

    /**
     * @param rulesXmlFile
     *        the file whose rule definitions are being validated (cannot be <code>null</code>)
     * @return a list of errors (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    List< String > validateRules( final File rulesXmlFile ) throws KException;

}
