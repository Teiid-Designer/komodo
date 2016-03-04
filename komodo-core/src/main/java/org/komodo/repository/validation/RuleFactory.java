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
package org.komodo.repository.validation;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import javax.jcr.Node;
import javax.jcr.Session;
import org.komodo.core.KomodoLexicon;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.RepositoryImpl.UnitOfWorkImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.spi.repository.validation.Rule.MessageKey;
import org.komodo.spi.utils.LocalizedMessage;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.modeshape.jcr.api.JcrTools;

/**
 * A factory for {@link Rule objects}.
 */
public final class RuleFactory {

    private static final KLog LOGGER = KLog.getLogger();

    /**
     * The root path of the Komodo environment validation area.
     */
    public static String VALIDATION_ROOT = ( RepositoryImpl.ENV_ROOT + StringConstants.FORWARD_SLASH + KomodoLexicon.Environment.VALIDATION );

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository (cannot be <code>null</code>)
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param nodePropRestrictionMap
     *        the additional property restrictions for this rule (cannot be <code>null</code>)
     * @param childType
     *        the name of the child type whose child count range is being validated (cannot be empty)
     * @param childPropRestrictionMap
     *        the additional child property restrictions for this rule (cannot be <code>null</code>)
     * @param childRequired
     *        <code>true</code> if a child of this type must exist.
     * @param minValue
     *        the minimum allowed number of children with the specified type (cannot be <code>null</code>)
     * @param minInclusive
     *        <code>true</code> if the number of children can equal the minimum value
     * @param maxValue
     *        the maximum allowed number of children with the specified type (cannot be <code>null</code>)
     * @param maxInclusive
     *        <code>true</code> if the number of children can equal the maximum value
     * @param severity
     *        the severity of the rule.
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Rule createChildCountValidationRule( final UnitOfWork transaction,
                                                       final Repository repository,
                                                       final String name,
                                                       final String nodeType,
                                                       final Map<String,String> nodePropRestrictionMap,
                                                       final String childType,
                                                       final Map<String,String> childPropRestrictionMap,
                                                       final boolean childRequired,
                                                       final Number minValue,
                                                       final boolean minInclusive,
                                                       final Number maxValue,
                                                       final boolean maxInclusive,
                                                       final Outcome.Level severity,
                                                       final List< LocalizedMessage > descriptions,
                                                       final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( childType, "childType" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( minValue != null ) || ( maxValue != null ), "minValue or maxValue must not be null" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "createChildCountValidationRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              repository,
                                              name,
                                              KomodoLexicon.Rule.NUMBER_RULE,
                                              Rule.ValidationType.CHILD,
                                              Rule.RuleType.NUMBER,
                                              nodeType,
                                              nodePropRestrictionMap,
                                              childPropRestrictionMap,
                                              severity,
                                              descriptions,
                                              messages );
            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, childType );
            rule.setProperty( transaction, KomodoLexicon.Rule.REQUIRED, childRequired );

            if (minValue != null) {
                rule.setProperty( transaction, KomodoLexicon.Rule.MIN_VALUE, minValue.toString() );
                rule.setProperty( transaction, KomodoLexicon.Rule.MIN_VALUE_INCLUSIVE, minInclusive );
            }

            if (maxValue != null) {
                rule.setProperty( transaction, KomodoLexicon.Rule.MAX_VALUE, maxValue.toString() );
                rule.setProperty( transaction, KomodoLexicon.Rule.MAX_VALUE_INCLUSIVE, maxInclusive );
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( e );
        }
    }

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository (cannot be <code>null</code>)
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param nodePropRestrictionMap
     *        the additional property restrictions for this rule (cannot be <code>null</code>)
     * @param childType
     *        the node type whose relationships are being validated (cannot be empty)
     * @param childPropRestrictionMap
     *        the additional child property restrictions for this rule (cannot be <code>null</code>)
     * @param childRequired
     *        <code>true</code> if a child of this type must exist.
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
     * @param severity
     *        the severity of the rule.
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Rule createChildRelationshipValidationRule( final UnitOfWork transaction,
                                                              final Repository repository,
                                                              final String name,
                                                              final String nodeType,
                                                              final Map<String,String> nodePropRestrictionMap,
                                                              final String childType,
                                                              final Map<String,String> childPropRestrictionMap,
                                                              final boolean childRequired,
                                                              final List< String > propsThatMustExist,
                                                              final List< String > propsThatMustNotExist,
                                                              final List< String > childTypesThatMustExist,
                                                              final List< String > childTypesThatMustNotExist,
                                                              final Outcome.Level severity,
                                                              final List< LocalizedMessage > descriptions,
                                                              final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( childType, "childType" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( ( propsThatMustExist != null ) && !propsThatMustExist.isEmpty() )
                         || ( ( propsThatMustNotExist != null ) && !propsThatMustNotExist.isEmpty() )
                         || ( ( childTypesThatMustExist != null ) && !childTypesThatMustExist.isEmpty() )
                         || ( ( childTypesThatMustNotExist != null ) && !childTypesThatMustNotExist.isEmpty() ),
        "at least one relationship collection must not be empty" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "createChildRelationshipValidationRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              repository,
                                              name,
                                              KomodoLexicon.Rule.RELATIONSHIP_RULE,
                                              Rule.ValidationType.CHILD,
                                              Rule.RuleType.RELATIONSHIP,
                                              nodeType,
                                              nodePropRestrictionMap,
                                              childPropRestrictionMap,
                                              severity,
                                              descriptions,
                                              messages );

            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, childType );
            rule.setProperty( transaction, KomodoLexicon.Rule.REQUIRED, childRequired );

            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.PROP_EXISTS, propsThatMustExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.PROP_ABSENT, propsThatMustNotExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.CHILD_EXISTS, childTypesThatMustExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.CHILD_ABSENT, childTypesThatMustNotExist );

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( e );
        }
    }

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository (cannot be <code>null</code>)
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param nodePropRestrictionMap
     *        the additional property restrictions for this rule (cannot be <code>null</code>)
     * @param pattern
     *        the regular expression that the child node name must match (cannot be empty)
     * @param severity
     *        the severity of the rule.
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Rule createNodeNameRule( final UnitOfWork transaction,
                                           final Repository repository,
                                           final String name,
                                           final String nodeType,
                                           final Map<String,String> nodePropRestrictionMap,
                                           final String pattern,
                                           final Outcome.Level severity,
                                           final List< LocalizedMessage > descriptions,
                                           final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( pattern, "pattern" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "createNodeNameRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              repository,
                                              name,
                                              KomodoLexicon.Rule.PATTERN_RULE,
                                              Rule.ValidationType.NODE,
                                              Rule.RuleType.PATTERN,
                                              nodeType,
                                              nodePropRestrictionMap,
                                              Collections.emptyMap(),
                                              severity,
                                              descriptions,
                                              messages );
            rule.setProperty( transaction, KomodoLexicon.Rule.PATTERN, pattern );
            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( e );
        }
    }

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository (cannot be <code>null</code>)
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param nodePropRestrictionMap
     *        the additional property restrictions for this rule (cannot be <code>null</code>)
     * @param propertyName
     *        the name of the property whose value is being validated (cannot be empty)
     * @param propertyRequired
     *        <code>true</code> if the property is required.
     * @param pattern
     *        the regular expression that the property value must match (cannot be empty)
     * @param severity
     *        the severity of the rule.
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Rule createPropertyPatternRule( final UnitOfWork transaction,
                                                  final Repository repository,
                                                  final String name,
                                                  final String nodeType,
                                                  final Map<String,String> nodePropRestrictionMap,
                                                  final String propertyName,
                                                  final boolean propertyRequired,
                                                  final String pattern,
                                                  final Outcome.Level severity,
                                                  final List< LocalizedMessage > descriptions,
                                                  final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( pattern, "pattern" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "createPropertyPatternRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              repository,
                                              name,
                                              KomodoLexicon.Rule.PATTERN_RULE,
                                              Rule.ValidationType.PROPERTY,
                                              Rule.RuleType.PATTERN,
                                              nodeType,
                                              nodePropRestrictionMap,
                                              Collections.emptyMap(),
                                              severity,
                                              descriptions,
                                              messages );
            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, propertyName );
            rule.setProperty( transaction, KomodoLexicon.Rule.REQUIRED, propertyRequired );
            rule.setProperty( transaction, KomodoLexicon.Rule.PATTERN, pattern );
            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( e );
        }
    }

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository (cannot be <code>null</code>)
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param nodePropRestrictionMap
     *        the additional property restrictions for this rule (cannot be <code>null</code>)
     * @param propertyName
     *        the property whose relationships are being validated (cannot be empty)
     * @param propertyRequired
     *        <code>true</code> if the property must exist.
     * @param propsThatMustExist
     *        a list of properties that must exist (can be <code>null</code> or empty)
     * @param propsThatMustNotExist
     *        a list of properties that must NOT exist (can be <code>null</code> or empty)
     * @param childTypesThatMustExist
     *        a list of node types that at least one child must have (can be <code>null</code> or empty)
     * @param childTypesThatMustNotExist
     *        a list of node types that no child must have (can be <code>null</code> or empty)
     * @param severity
     *        the severity of the rule.
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Rule createPropertyRelationshipValidationRule( final UnitOfWork transaction,
                                                                 final Repository repository,
                                                                 final String name,
                                                                 final String nodeType,
                                                                 final Map<String,String> nodePropRestrictionMap,
                                                                 final String propertyName,
                                                                 final boolean propertyRequired,
                                                                 final List< String > propsThatMustExist,
                                                                 final List< String > propsThatMustNotExist,
                                                                 final List< String > childTypesThatMustExist,
                                                                 final List< String > childTypesThatMustNotExist,
                                                                 final Outcome.Level severity,
                                                                 final List< LocalizedMessage > descriptions,
                                                                 final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( ( propsThatMustExist != null ) && !propsThatMustExist.isEmpty() )
                         || ( ( propsThatMustNotExist != null ) && !propsThatMustNotExist.isEmpty() )
                         || ( ( childTypesThatMustExist != null ) && !childTypesThatMustExist.isEmpty() )
                         || ( ( childTypesThatMustNotExist != null ) && !childTypesThatMustNotExist.isEmpty() ),
        "at least one relationship collection must not be empty" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "createPropertyRelationshipValidationRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              repository,
                                              name,
                                              KomodoLexicon.Rule.RELATIONSHIP_RULE,
                                              Rule.ValidationType.PROPERTY,
                                              Rule.RuleType.RELATIONSHIP,
                                              nodeType,
                                              nodePropRestrictionMap,
                                              Collections.emptyMap(),
                                              severity,
                                              descriptions,
                                              messages );
            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, propertyName );
            rule.setProperty( transaction, KomodoLexicon.Rule.REQUIRED, propertyRequired );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.PROP_EXISTS, propsThatMustExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.PROP_ABSENT, propsThatMustNotExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.CHILD_EXISTS, childTypesThatMustExist );
            processMultiValuedProperty( transaction, rule, KomodoLexicon.Rule.CHILD_ABSENT, childTypesThatMustNotExist );
            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( e );
        }
    }

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository (cannot be <code>null</code>)
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param nodePropRestrictionMap
     *        the additional property restrictions for this rule (cannot be <code>null</code>)
     * @param propertyName
     *        the name of the property whose value range is being validated (cannot be empty)
     * @param propertyRequired
     *        <code>true</code> if the property must exist.
     * @param minValue
     *        the minimum allowed value (cannot be <code>null</code>)
     * @param minInclusive
     *        <code>true</code> if the property value can equal the minimum value
     * @param maxValue
     *        the maximum allowed value (cannot be <code>null</code>)
     * @param maxInclusive
     *        <code>true</code> if the property value can equal the maximum value
     * @param severity
     *        the severity of the rule.
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Rule createPropertyValueNumberValidationRule( final UnitOfWork transaction,
                                                                final Repository repository,
                                                                final String name,
                                                                final String nodeType,
                                                                final Map<String,String> nodePropRestrictionMap,
                                                                final String propertyName,
                                                                final boolean propertyRequired,
                                                                final Number minValue,
                                                                final boolean minInclusive,
                                                                final Number maxValue,
                                                                final boolean maxInclusive,
                                                                final Outcome.Level severity,
                                                                final List< LocalizedMessage > descriptions,
                                                                final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( propertyName, "propertyName" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( minValue != null ) || ( maxValue != null ), "minValue or maxValue must not be null" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "createPropertyValueNumberValidationRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              repository,
                                              name,
                                              KomodoLexicon.Rule.NUMBER_RULE,
                                              Rule.ValidationType.PROPERTY,
                                              Rule.RuleType.NUMBER,
                                              nodeType,
                                              nodePropRestrictionMap,
                                              Collections.emptyMap(),
                                              severity,
                                              descriptions,
                                              messages );
            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, propertyName );
            rule.setProperty( transaction, KomodoLexicon.Rule.REQUIRED, propertyRequired );

            if (minValue != null) {
                rule.setProperty( transaction, KomodoLexicon.Rule.MIN_VALUE, minValue.toString() );
                rule.setProperty( transaction, KomodoLexicon.Rule.MIN_VALUE_INCLUSIVE, minInclusive );
            }

            if (maxValue != null) {
                rule.setProperty( transaction, KomodoLexicon.Rule.MAX_VALUE, maxValue.toString() );
                rule.setProperty( transaction, KomodoLexicon.Rule.MAX_VALUE_INCLUSIVE, maxInclusive );
            }

            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( e );
        }
    }

    /**
     * The error message and description list elements must be 2 element arrays with the first element being the locale and the
     * second element being the translated text.
     *
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository (cannot be <code>null</code>)
     * @param name
     *        the unique rule name (cannot be empty)
     * @param nodeType
     *        the node type name this rule is validating (cannot be empty)
     * @param nodePropRestrictionMap
     *        the additional property restrictions for this rule (cannot be <code>null</code>)
     * @param childType
     *        the child type whose names are being validated (cannot be empty)
     * @param childPropRestrictionMap
     *        the additional child property restrictions for this rule (cannot be <code>null</code>)
     * @param childRequired
     *        <code>true</code> if a child of this type must exist.
     * @param matchType
     *        <code>true</code> if only children of the same type can't have the same name
     * @param severity
     *        the severity of the rule.
     * @param descriptions
     *        the localized descriptions (cannot be <code>null</code>, include empty elements, or be empty)
     * @param messages
     *        the localized error messages (cannot be <code>null</code>, include empty elements, or be empty)
     * @return the new rule (never <code>null</code>)
     * @throws KException
     *         if an error occurs
     */
    public static Rule createSameNameSiblingValidationRule( final UnitOfWork transaction,
                                                            final Repository repository,
                                                            final String name,
                                                            final String nodeType,
                                                            final Map<String,String> nodePropRestrictionMap,
                                                            final String childType,
                                                            final Map<String,String> childPropRestrictionMap,
                                                            final boolean childRequired,
                                                            final boolean matchType,
                                                            final Outcome.Level severity,
                                                            final List< LocalizedMessage > descriptions,
                                                            final List< LocalizedMessage > messages ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$

        if (LOGGER.isDebugEnabled()) {
            LOGGER.debug( "createSameNameSiblingValidationRule: transaction = {0}, name = {1}", transaction.getName(), name ); //$NON-NLS-1$
        }

        try {
            final RuleImpl rule = createRule( transaction,
                                              repository,
                                              name,
                                              KomodoLexicon.Rule.SNS_RULE,
                                              Rule.ValidationType.CHILD,
                                              Rule.RuleType.SAME_NAME_SIBLING,
                                              nodeType,
                                              nodePropRestrictionMap,
                                              childPropRestrictionMap,
                                              severity,
                                              descriptions,
                                              messages );
            rule.setProperty( transaction, KomodoLexicon.Rule.MATCH_TYPE, matchType );
            rule.setProperty( transaction, KomodoLexicon.Rule.JCR_NAME, childType );
            rule.setProperty( transaction, KomodoLexicon.Rule.REQUIRED, childRequired );
            return rule;
        } catch (final Exception e) {
            throw ObjectImpl.handleError( e );
        }
    }

    private static RuleImpl createRule( final UnitOfWork uow,
                                        final Repository repository,
                                        final String name,
                                        final String nodeType,
                                        final Rule.ValidationType validationType,
                                        final Rule.RuleType ruleType,
                                        final String ruleNodeType,
                                        final Map<String,String> nodePropertyRestrictions,
                                        final Map<String,String> childPropertyRestrictions,
                                        final Outcome.Level severity,
                                        final List< LocalizedMessage > descriptions,
                                        final List< LocalizedMessage > messages ) throws Exception {
        assert ( uow != null );
        assert ( validationType != null );
        assert ( ruleType != null );
        ArgCheck.isNotNull( nodePropertyRestrictions );
        ArgCheck.isNotNull( childPropertyRestrictions );
        ArgCheck.isNotEmpty( name, "name" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( nodeType, "nodeType" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( ruleNodeType, "ruleNodeType" ); //$NON-NLS-1$
        ArgCheck.isNotEmpty( descriptions, "descriptions" ); //$NON-NLS-1$

        final KomodoObject parent = getValidationDefaultAreaNode( uow, repository );
        final KomodoObject rule = parent.addChild( uow, name, nodeType );
        rule.setProperty( uow, KomodoLexicon.Rule.NODE_TYPE, ruleNodeType );
        rule.setProperty( uow, KomodoLexicon.Rule.VALIDATION_TYPE, validationType.name() );
        rule.setProperty( uow, KomodoLexicon.Rule.SEVERITY, severity );

        // Add optional property restrictions
        {
            // Add grouping node if necessary
            if(!nodePropertyRestrictions.isEmpty() || !childPropertyRestrictions.isEmpty()) {
                final KomodoObject propRestrictionsNode = rule.addChild( uow,
                                                                         KomodoLexicon.Rule.PROP_RESTRICTIONS_GROUPING,
                                                                         KomodoLexicon.Rule.PROP_RESTRICTIONS_GROUPING);
                for (final String propName : nodePropertyRestrictions.keySet()) {
                    final KomodoObject node = propRestrictionsNode.addChild( uow,
                                                                             propName,
                                                                             KomodoLexicon.Rule.PROP_RESTRICTION);
                    node.setProperty( uow, KomodoLexicon.Rule.PROP_VALUE, nodePropertyRestrictions.get(propName) );
                    node.setProperty( uow, KomodoLexicon.Rule.RESTRICTION_TYPE, Rule.PropertyRestriction.NODE.name() );
                }
                for (final String propName : childPropertyRestrictions.keySet()) {
                    final KomodoObject node = propRestrictionsNode.addChild( uow,
                                                                             propName,
                                                                             KomodoLexicon.Rule.PROP_RESTRICTION);
                    node.setProperty( uow, KomodoLexicon.Rule.PROP_VALUE, childPropertyRestrictions.get(propName) );
                    node.setProperty( uow, KomodoLexicon.Rule.RESTRICTION_TYPE, Rule.PropertyRestriction.CHILD.name() );
                }
            }
        }

        // add description and optional messages
        final KomodoObject messagesNode = rule.addChild( uow,
                                                         KomodoLexicon.Rule.MESSAGES,
                                                         KomodoLexicon.Rule.LOCALIZED_MESSAGE_GROUPING );

        { // add descriptions
            final KomodoObject description = messagesNode.addChild( uow,
                                                                    MessageKey.DESCRIPTION.name(),
                                                                    KomodoLexicon.Rule.LOCALIZED_MESSAGE );

            for (final LocalizedMessage localizedDescription : descriptions) {
                final KomodoObject node = description.addChild( uow,
                                                                localizedDescription.getLocaleCode(),
                                                                KomodoLexicon.Rule.LOCALIZED_TEXT_TYPE );
                node.setProperty( uow, KomodoLexicon.Rule.LOCALIZED_TEXT, localizedDescription.getMessage() );
            }
        }

        { // add messages
            if (( messages != null ) && !messages.isEmpty()) {
                for (final LocalizedMessage localizedMessage : messages) {
                    final String id = localizedMessage.getId();
                    KomodoObject message = null;

                    if (messagesNode.hasChild( uow, id, KomodoLexicon.Rule.LOCALIZED_MESSAGE )) {
                        message = messagesNode.getChild( uow, id, KomodoLexicon.Rule.LOCALIZED_MESSAGE );
                    } else {
                        message = messagesNode.addChild( uow, id, KomodoLexicon.Rule.LOCALIZED_MESSAGE );
                    }

                    final KomodoObject node = message.addChild( uow,
                                                                localizedMessage.getLocaleCode(),
                                                                KomodoLexicon.Rule.LOCALIZED_TEXT_TYPE );
                    node.setProperty( uow, KomodoLexicon.Rule.LOCALIZED_TEXT, localizedMessage.getMessage() );
                }
            }
        }

        return new RuleImpl( uow, repository, rule.getAbsolutePath() );
    }

    protected static Session getSession( final UnitOfWork transaction ) {
        return ( ( UnitOfWorkImpl )transaction ).getSession();
    }

    /**
     * Get the Validation defaults location from the repository
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @param repository
     *        the repository (cannot be <code>null</code>)
     * @return the validation area
     * @throws KException
     *         if an error occurs
     */
    public static KomodoObject getValidationDefaultAreaNode( final UnitOfWork transaction, final Repository repository ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repository, "repository" ); //$NON-NLS-1$

        final Session session = getSession( transaction );
        final JcrTools jcrTools = new JcrTools();
        try {
            final Node node = jcrTools.findOrCreateNode( session, VALIDATION_ROOT );

            return new ObjectImpl( repository , node.getPath(), node.getIndex() );
        } catch (Exception e) {
            throw new KException(e);
        }
    }

    private static void processMultiValuedProperty( final UnitOfWork uow,
                                                    final KomodoObject rule,
                                                    final String propName,
                                                    final List< String > values ) throws Exception {
        if (( values != null ) && !values.isEmpty()) {
            final String[] result = new String[ values.size() ];
            int i = 0;

            for (final String value : values) {
                ArgCheck.isNotEmpty( value, "value" ); //$NON-NLS-1$
                result[i++] = value;
            }

            rule.setProperty( uow, propName, ( Object[] )result );
        }
    }

    private RuleFactory() {
        // nothing to do
    }

}

