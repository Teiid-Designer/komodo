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

import java.text.NumberFormat;
import java.text.ParseException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import org.komodo.core.KomodoLexicon;
import org.komodo.repository.Messages;
import org.komodo.repository.ObjectImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.outcome.Outcome.Level;
import org.komodo.spi.outcome.OutcomeFactory.OutcomeImpl;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyValueType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * An implementation of a {@link Rule validation rule}.
 */
public class RuleImpl extends ObjectImpl implements Rule {

    class ResultImpl extends OutcomeImpl implements Result {

        private final String path;
        private final String ruleId;
        private final long timeCreated;

        ResultImpl( final String nodePath,
                    final String evaluationRuleId,
                    final Level outcomeLevel,
                    final String outcomeMessage ) {
            this.path = nodePath;
            this.ruleId = evaluationRuleId;
            this.timeCreated = System.currentTimeMillis();
            setMessage( outcomeMessage );
            setLevel( outcomeLevel );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.validation.Result#getPath()
         */
        @Override
        public String getPath() {
            return this.path;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.validation.Result#getRuleId()
         */
        @Override
        public String getRuleId() {
            return this.ruleId;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.repository.validation.Result#getTimestamp()
         */
        @Override
        public long getTimestamp() {
            return this.timeCreated;
        }

    }

    private static String getLocaleCode( final boolean includeCountry,
                                         final boolean includeVariant ) {
        final Locale locale = Locale.getDefault();
        final StringBuilder result = new StringBuilder( locale.getLanguage() );

        if ( includeCountry ) {
            final String country = locale.getCountry();

            if ( !StringUtils.isBlank( country ) ) {
                result.append( StringConstants.UNDERSCORE_CHAR ).append( country );

                if ( includeVariant ) {
                    final String variant = locale.getVariant();

                    if ( !StringUtils.isBlank( variant ) ) {
                        result.append( StringConstants.UNDERSCORE_CHAR ).append( variant );
                    }
                }

            }
        }

        return result.toString();
    }

    /**
     * Constructs a validation rule.
     *
     * @param uow
     *        the transaction (can be <code>null</code> if update should be automatically committed)
     * @param repository
     *        the repository where the relational object exists (cannot be <code>null</code>)
     * @param path
     *        the path (cannot be empty but assumed to be a path to a rule)
     * @throws KException
     *         if an error occurs
     */
    public RuleImpl( final UnitOfWork uow,
                     final Repository repository,
                     final String path ) throws KException {
        super( repository, path, 0 );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#evaluate(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public Result evaluate( final UnitOfWork transaction,
                            final KomodoObject kobject ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$
        ArgCheck.isNotNull( kobject, "kobject" ); //$NON-NLS-1$

        if ( !isEnabled( transaction ) ) {
            return new ProblemRule(getName(transaction),ProblemRule.Type.NOT_ENABLED).evaluate(transaction, kobject);
        }

        try {
            final RuleType ruleType = getRuleType( transaction );

            switch ( getValidationType( transaction ) ) {
                case NODE:
                    return evaluateNodeRule( transaction, kobject, ruleType );
                case PROPERTY:
                    return evaluatePropertyRule( transaction, kobject, ruleType );
                case CHILD:
                    return evaluateChildRule( transaction, kobject, ruleType );
                default:
                    // need to add new validation type
                    throw new RuntimeException( "Unexpected validation type of '" + getValidationType( transaction ) + '\'' ); //$NON-NLS-1$
            }
        } catch ( final Exception e ) {
            if ( e instanceof KException ) {
                throw ( KException )e;
            }

            throw new KException( e );
        }
    }

    private Result evaluateChildRule( final UnitOfWork transaction,
                                      final KomodoObject kobject,
                                      final RuleType ruleType ) throws KException {
        final Property childTypeProp = getProperty( transaction, KomodoLexicon.Rule.JCR_NAME );
        assert ( childTypeProp != null );

        final String childType = childTypeProp.getStringValue( transaction );
        final Map<String,String> propRestrictions = getPropRestrictions(transaction,PropertyRestriction.CHILD);
        
        // Determine if a child that passes the type and property restrictions exists
        KomodoObject[] matchingChildren = ValidationUtils.getChildrenMatchingTypeAndPropRestrictions(transaction, kobject, childType, propRestrictions);
        boolean hasChildWithTypeAndRestrictions = matchingChildren.length > 0;
        
        // Determine if the property is required
        final Property childRequired = getProperty( transaction, KomodoLexicon.Rule.REQUIRED );
        boolean childRqd = false;
        if(childRequired!=null) {
            childRqd = childRequired.getBooleanValue( transaction );
        }
        
        String errorMsg = null;
        String[] args = null;

        // Child required but does not exist.
        if(childRqd && !hasChildWithTypeAndRestrictions) {
            // Use rule description if found, otherwise use a default 'child not found' message.
            errorMsg = getDescription( transaction );
            if( StringUtils.isBlank(errorMsg) ) {
                args = new String[] { childType, kobject.getName( transaction ), kobject.getAbsolutePath() };
                errorMsg = Messages.getString( Messages.Validation.CHILD_OF_REQUIRED_TYPE_NOT_FOUND, ( Object[] )args );
            }
            return new ResultImpl( kobject.getAbsolutePath(), getName( transaction ), getSeverity( transaction ), errorMsg );
        }
        
        switch ( ruleType ) {
            case NUMBER: {
                final Integer childCount = matchingChildren.length;
                boolean minChecked = false;
                boolean maxChecked = false;

                try {
                    { // check min value
                        final Property minValueProp = getProperty( transaction, KomodoLexicon.Rule.MIN_VALUE );

                        if ( minValueProp != null ) {
                            minChecked = true;
                            boolean inclusive = true;
                            final Property minInclusiveProp = getProperty( transaction, KomodoLexicon.Rule.MIN_VALUE_INCLUSIVE );

                            if ( minInclusiveProp != null ) {
                                inclusive = minInclusiveProp.getBooleanValue( transaction );
                            }

                            final String minString = minValueProp.getStringValue( transaction );
                            final Number minValue = NumberFormat.getInstance().parse( minString );
                            final int result = Double.compare( childCount.doubleValue(), minValue.doubleValue() );

                            if ( ( inclusive && ( result < 0 ) ) || ( !inclusive && ( result <= 0 ) ) ) {
                                // Get specific message or description
                                errorMsg = getMessageOrDescription( transaction, MessageKey.CHILD_COUNT_BELOW_MIN_VALUE.name() );
                                // If rule does not have message or description for the locale, use a default.
                                if(StringUtils.isBlank(errorMsg)) {
                                    args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), childCount.toString(), childType, minString };
                                    errorMsg = Messages.getString( Messages.Validation.CHILD_COUNT_BELOW_MIN_VALUE, ( Object[] )args );
                                }
                            }
                        }
                    }

                    if ( StringUtils.isBlank( errorMsg ) ) {
                        { // check max value
                            final Property maxValueProp = getProperty( transaction, KomodoLexicon.Rule.MAX_VALUE );

                            if ( maxValueProp != null ) {
                                maxChecked = true;
                                boolean inclusive = true;
                                final Property maxInclusiveProp = getProperty( transaction, KomodoLexicon.Rule.MAX_VALUE_INCLUSIVE );

                                if ( maxInclusiveProp != null ) {
                                    inclusive = maxInclusiveProp.getBooleanValue( transaction );
                                }

                                final String maxString = maxValueProp.getStringValue( transaction );
                                final Number maxValue = NumberFormat.getInstance().parse( maxString );
                                final int result = Double.compare( childCount.doubleValue(), maxValue.doubleValue() );

                                if ( ( inclusive && ( result > 0 ) ) || ( !inclusive && ( result >= 0 ) ) ) {
                                    // Get specific message or description
                                    errorMsg = getMessageOrDescription( transaction, MessageKey.CHILD_COUNT_ABOVE_MAX_VALUE.name() );
                                    // If rule does not have message or description for the locale, use a default.
                                    if(StringUtils.isBlank(errorMsg)) {
                                        args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), childCount.toString(), childType, maxString };
                                        errorMsg = Messages.getString( Messages.Validation.CHILD_COUNT_ABOVE_MAX_VALUE, ( Object[] )args );
                                    }
                                }
                            }
                        }
                    }

                    if ( StringUtils.isBlank( errorMsg ) && !minChecked && !maxChecked ) {
                        // Get specific message or description
                        errorMsg = getMessageOrDescription( transaction, MessageKey.NUMBER_RULE_HAS_NO_VALUES.name() );
                        // If rule does not have message or description for the locale, use a default.
                        if(StringUtils.isBlank(errorMsg)) {
                            args = new String[] { getName( transaction ) };
                            errorMsg = Messages.getString( Messages.Validation.NUMBER_RULE_HAS_NO_VALUES, ( Object[] )args );
                        }
                    }
                } catch ( final ParseException ex ) {
                    // not a valid number rule and should be caught by XSD validation
                    // Get specific message or description
                    errorMsg = getMessageOrDescription( transaction, MessageKey.NUMBER_RULE_NON_NUMERIC_VALUES.name() );
                    // If rule does not have message or description for the locale, use a default.
                    if(StringUtils.isBlank(errorMsg)) {
                        args = new String[] { getName( transaction ) };
                        errorMsg = Messages.getString( Messages.Validation.NUMBER_RULE_NON_NUMERIC_VALUES, ( Object[] )args );
                    }
                }

                break;
            }
            case RELATIONSHIP: {
                // must have at least one child of this type to invoke this rule
                if ( !hasChildWithTypeAndRestrictions ) {
                    break;
                }
                
                // Check all of the children of the specified type
                for( KomodoObject kidObj : matchingChildren ) {
                    
                    { // props exist
                        final Property propExistsProp = getProperty( transaction, KomodoLexicon.Rule.PROP_EXISTS );

                        if ( propExistsProp != null ) {
                            for ( final String prop : propExistsProp.getStringValues( transaction ) ) {
                                if ( !kidObj.hasProperty( transaction, prop ) ) {
                                    // Get specific message or description
                                    errorMsg = getMessageOrDescription( transaction, MessageKey.RELATIONSHIP_RULE_REQUIRED_PROPERTY_NOT_FOUND.name() );
                                    // If rule does not have message or description for the locale, use a default.
                                    if(StringUtils.isBlank(errorMsg)) {
                                        args = new String[] { kidObj.getName( transaction ), kidObj.getAbsolutePath(), childType, prop };
                                        errorMsg = Messages.getString( Messages.Validation.RELATIONSHIP_RULE_REQUIRED_PROPERTY_NOT_FOUND, ( Object[] )args );
                                    }
                                    break;
                                }
                            }
                        }
                    }

                    if ( !StringUtils.isBlank( errorMsg ) ) {
                        break;
                    }

                    { // props absent
                        final Property propAbsentProp = getProperty( transaction, KomodoLexicon.Rule.PROP_ABSENT );

                        if ( propAbsentProp != null ) {
                            for ( final String prop : propAbsentProp.getStringValues( transaction ) ) {
                                if ( kidObj.hasProperty( transaction, prop ) ) {
                                    // Get specific message or description
                                    errorMsg = getMessageOrDescription( transaction, MessageKey.RELATIONSHIP_RULE_ABSENT_PROPERTY_FOUND.name() );
                                    // If rule does not have message or description for the locale, use a default.
                                    if(StringUtils.isBlank(errorMsg)) {
                                        args = new String[] { kidObj.getName( transaction ), kidObj.getAbsolutePath(), childType, prop };
                                        errorMsg = Messages.getString( Messages.Validation.RELATIONSHIP_RULE_ABSENT_PROPERTY_FOUND, ( Object[] )args );
                                    }
                                    break;
                                }
                            }
                        }
                    }

                    if ( !StringUtils.isBlank( errorMsg ) ) {
                        break;
                    }

                    { // children exist
                        final Property childExistsProp = getProperty( transaction, KomodoLexicon.Rule.CHILD_EXISTS );

                        if ( childExistsProp != null ) {
                            for ( final String kidType : childExistsProp.getStringValues( transaction ) ) {
                                if ( kidObj.getChildrenOfType( transaction, kidType ).length == 0 ) {
                                    // Get specific message or description
                                    errorMsg = getMessageOrDescription( transaction, MessageKey.RELATIONSHIP_RULE_REQUIRED_CHILD_NOT_FOUND.name() );
                                    // If rule does not have message or description for the locale, use a default.
                                    if(StringUtils.isBlank(errorMsg)) {
                                        args = new String[] { kidObj.getName( transaction ), kidObj.getAbsolutePath(), childType, kidType };
                                        errorMsg = Messages.getString( Messages.Validation.RELATIONSHIP_RULE_REQUIRED_CHILD_NOT_FOUND, ( Object[] )args );
                                    }
                                    break;
                                }
                            }
                        }
                    }

                    if ( !StringUtils.isBlank( errorMsg ) ) {
                        break;
                    }

                    { // children absent
                        final Property childAbsentProp = getProperty( transaction, KomodoLexicon.Rule.CHILD_ABSENT );

                        if ( childAbsentProp != null ) {
                            for ( final String kidType : childAbsentProp.getStringValues( transaction ) ) {
                                if ( kidObj.getChildrenOfType( transaction, kidType ).length > 0 ) {
                                    args = new String[] { kidObj.getName( transaction ), kidObj.getAbsolutePath(), childType, kidType };

                                    // Get specific message or description
                                    errorMsg = getMessageOrDescription( transaction, MessageKey.RELATIONSHIP_RULE_ABSENT_CHILD_FOUND.name() );
                                    // If rule does not have message or description for the locale, use a default.
                                    if(StringUtils.isBlank(errorMsg)) {
                                        errorMsg = Messages.getString( Messages.Validation.RELATIONSHIP_RULE_ABSENT_CHILD_FOUND, ( Object[] )args );
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    
                }

                break;
            }
            case SAME_NAME_SIBLING: {
                // Determine of only checking matches according to type
                boolean matchOnType = false;
                final Property matchOnTypeProp = getProperty( transaction, KomodoLexicon.Rule.MATCH_TYPE );

                if ( matchOnTypeProp != null ) {
                    matchOnType = matchOnTypeProp.getBooleanValue( transaction );
                }
                
                KomodoObject[] kids = null;
                if(matchOnType) {
                    kids = matchingChildren;
                } else {
                    kids = kobject.getChildren( transaction );
                }
                
                final Set< String > names = new HashSet<>();

                for ( final KomodoObject kid : kids ) {
                    final String name = kid.getName( transaction );

                    if ( !names.add( name ) ) {
                        args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), name, childType };

                        // Get specific message or description
                        errorMsg = getMessageOrDescription( transaction, MessageKey.RELATIONSHIP_RULE_SNS_FOUND.name() );
                        // If rule does not have message or description for the locale, use a default.
                        if(StringUtils.isBlank(errorMsg)) {
                            errorMsg = Messages.getString( Messages.Validation.RELATIONSHIP_RULE_SNS_FOUND, ( Object[] )args );
                        }
                        break;
                    }
                }

                break;
            }
            case PATTERN:
                assert false; // not a valid rule and should be caught by XSD validation
                break;
            default:
                assert false; // need to add a new case statement
                break;
        }

        if ( StringUtils.isBlank( errorMsg ) ) {
            return new ResultImpl( kobject.getAbsolutePath(), getName( transaction ), Level.OK, StringConstants.EMPTY_STRING );
        }

        assert ( !StringUtils.isBlank( errorMsg ) );
        return new ResultImpl( kobject.getAbsolutePath(), getName( transaction ), getSeverity( transaction ), errorMsg );
    }
    
    private Result evaluateNodeRule( final UnitOfWork transaction,
                                     final KomodoObject kobject,
                                     final RuleType ruleType ) throws KException {
        String errorMsg = null;
        String[] args = null;

        switch ( ruleType ) {
            case PATTERN: {
                final Property patternProp = getProperty( transaction, KomodoLexicon.Rule.PATTERN );
                assert ( patternProp != null );

                final String name = kobject.getName( transaction );

                if ( !name.matches( patternProp.getStringValue( transaction ) ) ) {
                    args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath() };

                    // Get specific message or description
                    errorMsg = getMessageOrDescription( transaction, MessageKey.PATTERN_RULE_INVALID_NODE_NAME.name() );
                    // If rule does not have message or description for the locale, use a default.
                    if(StringUtils.isBlank(errorMsg)) {
                        errorMsg = Messages.getString( Messages.Validation.PATTERN_RULE_INVALID_NODE_NAME, ( Object[] )args );
                    }
                }

                break;
            }
            case NUMBER:
            case RELATIONSHIP:
            case SAME_NAME_SIBLING:
                assert false; // not a valid rule and should be caught be XSD validation
                break;
            default:
                assert false; // need to add a new case statement
                break;
        }

        if ( StringUtils.isBlank( errorMsg ) ) {
            return new ResultImpl( kobject.getAbsolutePath(), getName( transaction ), Level.OK, StringConstants.EMPTY_STRING );
        }

        assert ( !StringUtils.isBlank( errorMsg ) );
        return new ResultImpl( kobject.getAbsolutePath(), getName( transaction ), getSeverity( transaction ), errorMsg );
    }

    private Result evaluatePropertyRule( final UnitOfWork transaction,
                                         final KomodoObject kobject,
                                         final RuleType ruleType ) throws KException {
        final Property jcrNameProp = getProperty( transaction, KomodoLexicon.Rule.JCR_NAME );
        assert ( jcrNameProp != null );

        final String propName = jcrNameProp.getStringValue( transaction );
        final boolean exists = kobject.hasProperty( transaction, propName );
        
        // Determine if the property is required
        final Property propRequired = getProperty( transaction, KomodoLexicon.Rule.REQUIRED );
        boolean propRqd = false;
        if(propRequired!=null) {
            propRqd = propRequired.getBooleanValue( transaction );
        }
        
        String errorMsg = null;
        String[] args = null;
        
        // Property is required but does not exist
        if ( propRqd && !exists ) {
            // Use rule description if found, otherwise use a default 'property not found' message.
            errorMsg = getDescription( transaction );
            if( StringUtils.isBlank(errorMsg) ) {
                args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), propName };
                errorMsg = Messages.getString( Messages.Validation.REQUIRED_PROPERTY_NOT_FOUND, ( Object[] )args );
            }
            return new ResultImpl( kobject.getAbsolutePath(), getName( transaction ), getSeverity( transaction ), errorMsg );
        }

        switch ( ruleType ) {
            case PATTERN: {
                // check value against the pattern
                final Property patternProp = getProperty( transaction, KomodoLexicon.Rule.PATTERN );
                assert ( patternProp != null );

                final String value = kobject.getProperty( transaction, propName ).getStringValue( transaction );

                if ( !value.matches( patternProp.getStringValue( transaction ) ) ) {
                    args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), propName };

                    // Get specific message or description
                    errorMsg = getMessageOrDescription( transaction, MessageKey.PATTERN_RULE_INVALID_PROPERTY_VALUE.name() );
                    // If rule does not have message or description for the locale, use a default.
                    if(StringUtils.isBlank(errorMsg)) {
                        errorMsg = Messages.getString( Messages.Validation.PATTERN_RULE_INVALID_PROPERTY_VALUE, ( Object[] )args );
                    }
                }

                break;
            }
            case NUMBER: {
                boolean minChecked = false;
                boolean maxChecked = false;
                final String valueString = kobject.getProperty( transaction, propName ).getStringValue( transaction );

                if ( StringUtils.isNumber( valueString ) ) {
                    try {
                        final Number value = NumberFormat.getInstance().parse( valueString );

                        { // check min value
                            final Property minValueProp = getProperty( transaction, KomodoLexicon.Rule.MIN_VALUE );

                            if ( minValueProp != null ) {
                                minChecked = true;
                                boolean inclusive = true;
                                final Property minInclusiveProp = getProperty( transaction, KomodoLexicon.Rule.MIN_VALUE_INCLUSIVE );

                                if ( minInclusiveProp != null ) {
                                    inclusive = minInclusiveProp.getBooleanValue( transaction );
                                }

                                final String minString = minValueProp.getStringValue( transaction );
                                final Number minValue = NumberFormat.getInstance().parse( minString );
                                final int result = Double.compare( value.doubleValue(), minValue.doubleValue() );

                                if ( ( inclusive && ( result < 0 ) ) || ( !inclusive && ( result <= 0 ) ) ) {
                                    args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), propName, valueString, minString };

                                    // Get specific message or description
                                    errorMsg = getMessageOrDescription( transaction, MessageKey.PROPERTY_RULE_VALUE_BELOW_MIN_VALUE.name() );
                                    // If rule does not have message or description for the locale, use a default.
                                    if(StringUtils.isBlank(errorMsg)) {
                                        errorMsg = Messages.getString( Messages.Validation.PROPERTY_RULE_VALUE_BELOW_MIN_VALUE, ( Object[] )args );
                                    }
                                }
                            }
                        }

                        if ( StringUtils.isBlank( errorMsg ) ) {
                            { // check max value
                                final Property maxValueProp = getProperty( transaction, KomodoLexicon.Rule.MAX_VALUE );

                                if ( maxValueProp != null ) {
                                    maxChecked = true;
                                    boolean inclusive = true;
                                    final Property maxInclusiveProp = getProperty( transaction,
                                                                                   KomodoLexicon.Rule.MAX_VALUE_INCLUSIVE );

                                    if ( maxInclusiveProp != null ) {
                                        inclusive = maxInclusiveProp.getBooleanValue( transaction );
                                    }

                                    final String maxString = maxValueProp.getStringValue( transaction );
                                    final Number maxValue = NumberFormat.getInstance().parse( maxString );
                                    final int result = Double.compare( value.doubleValue(), maxValue.doubleValue() );

                                    if ( ( inclusive && ( result > 0 ) ) || ( !inclusive && ( result >= 0 ) ) ) {
                                        args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), propName, valueString, maxString };

                                        // Get specific message or description
                                        errorMsg = getMessageOrDescription( transaction, MessageKey.PROPERTY_RULE_VALUE_ABOVE_MAX_VALUE.name() );
                                        // If rule does not have message or description for the locale, use a default.
                                        if(StringUtils.isBlank(errorMsg)) {
                                            errorMsg = Messages.getString( Messages.Validation.PROPERTY_RULE_VALUE_ABOVE_MAX_VALUE, ( Object[] )args );
                                        }
                                    }
                                }
                            }
                        }

                        if ( StringUtils.isBlank( errorMsg ) && !minChecked && !maxChecked ) {
                            args = new String[] { getName( transaction ) };

                            // Get specific message or description
                            errorMsg = getMessageOrDescription( transaction, MessageKey.NUMBER_RULE_HAS_NO_VALUES.name() );
                            // If rule does not have message or description for the locale, use a default.
                            if(StringUtils.isBlank(errorMsg)) {
                                errorMsg = Messages.getString( Messages.Validation.NUMBER_RULE_HAS_NO_VALUES, ( Object[] )args );
                            }
                        }
                    } catch ( final ParseException ex ) {
                        // not a valid number rule and should be caught by XSD validation
                        args = new String[] { getName( transaction ) };

                        // Get specific message or description
                        errorMsg = getMessageOrDescription( transaction, MessageKey.NUMBER_RULE_NON_NUMERIC_VALUES.name() );
                        // If rule does not have message or description for the locale, use a default.
                        if(StringUtils.isBlank(errorMsg)) {
                            errorMsg = Messages.getString( Messages.Validation.NUMBER_RULE_NON_NUMERIC_VALUES, ( Object[] )args );
                        }
                    }
                }

                break;
            }
            case RELATIONSHIP: {
                { // props exist
                    final Property propExistsProp = getProperty( transaction, KomodoLexicon.Rule.PROP_EXISTS );

                    if ( propExistsProp != null ) {
                        for ( final String prop : propExistsProp.getStringValues( transaction ) ) {
                            if ( !kobject.hasProperty( transaction, prop ) ) {
                                args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), propName, prop };

                                // Get specific message or description
                                errorMsg = getMessageOrDescription( transaction, MessageKey.PROPERTY_RULE_REQUIRED_PROPERTY_NOT_FOUND.name() );
                                // If rule does not have message or description for the locale, use a default.
                                if(StringUtils.isBlank(errorMsg)) {
                                    errorMsg = Messages.getString( Messages.Validation.PROPERTY_RULE_REQUIRED_PROPERTY_NOT_FOUND, ( Object[] )args );
                                }
                                break;
                            }
                        }
                    }
                }

                if ( !StringUtils.isBlank( errorMsg ) ) {
                    break;
                }

                { // props absent
                    final Property propAbsentProp = getProperty( transaction, KomodoLexicon.Rule.PROP_ABSENT );

                    if ( propAbsentProp != null ) {
                        for ( final String prop : propAbsentProp.getStringValues( transaction ) ) {
                            if ( kobject.hasProperty( transaction, prop ) ) {
                                args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), propName, prop };

                                // Get specific message or description
                                errorMsg = getMessageOrDescription( transaction, MessageKey.PROPERTY_RULE_ABSENT_PROPERTY_FOUND.name() );
                                // If rule does not have message or description for the locale, use a default.
                                if(StringUtils.isBlank(errorMsg)) {
                                    errorMsg = Messages.getString( Messages.Validation.PROPERTY_RULE_ABSENT_PROPERTY_FOUND, ( Object[] )args );
                                }
                                break;
                            }
                        }
                    }
                }

                if ( !StringUtils.isBlank( errorMsg ) ) {
                    break;
                }

                { // children exist
                    final Property childExistsProp = getProperty( transaction, KomodoLexicon.Rule.CHILD_EXISTS );

                    if ( childExistsProp != null ) {
                        for ( final String childType : childExistsProp.getStringValues( transaction ) ) {
                            if ( kobject.getChildrenOfType( transaction, childType ).length == 0 ) {
                                args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), propName, childType };

                                // Get specific message or description
                                errorMsg = getMessageOrDescription( transaction, MessageKey.RELATIONSHIP_RULE_REQUIRED_CHILD_NOT_FOUND.name() );
                                // If rule does not have message or description for the locale, use a default.
                                if(StringUtils.isBlank(errorMsg)) {
                                    errorMsg = Messages.getString( Messages.Validation.RELATIONSHIP_RULE_REQUIRED_CHILD_NOT_FOUND, ( Object[] )args );
                                }
                                break;
                            }
                        }
                    }
                }

                if ( !StringUtils.isBlank( errorMsg ) ) {
                    break;
                }

                { // children absent
                    final Property childAbsentProp = getProperty( transaction, KomodoLexicon.Rule.CHILD_ABSENT );

                    if ( childAbsentProp != null ) {
                        for ( final String childType : childAbsentProp.getStringValues( transaction ) ) {
                            if ( kobject.getChildrenOfType( transaction, childType ).length > 0 ) {
                                args = new String[] { kobject.getName( transaction ), kobject.getAbsolutePath(), propName, childType };

                                // Get specific message or description
                                errorMsg = getMessageOrDescription( transaction, MessageKey.PROPERTY_RULE_ABSENT_CHILD_FOUND.name() );
                                // If rule does not have message or description for the locale, use a default.
                                if(StringUtils.isBlank(errorMsg)) {
                                    errorMsg = Messages.getString( Messages.Validation.PROPERTY_RULE_ABSENT_CHILD_FOUND, ( Object[] )args );
                                }
                                break;
                            }
                        }
                    }
                }

                break;
            }
            case SAME_NAME_SIBLING:
                assert false; // not a valid rule and should be caught by XSD validation
                break;
            default:
                assert false; // need to add a new case statement
                break;
        }

        if ( StringUtils.isBlank( errorMsg ) ) {
            return new ResultImpl( kobject.getAbsolutePath(), getName( transaction ), Level.OK, StringConstants.EMPTY_STRING );
        }

        return new ResultImpl( kobject.getAbsolutePath(), getName( transaction ), getSeverity( transaction ), errorMsg );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription( final UnitOfWork transaction ) throws KException {
        return getMessage( transaction, MessageKey.DESCRIPTION.name() );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getJcrName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJcrName( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction, PropertyValueType.STRING, "getJcrName", KomodoLexicon.Rule.JCR_NAME ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getMessage(org.komodo.spi.repository.Repository.UnitOfWork,
     *      java.lang.String)
     */
    @Override
    public String getMessage( final UnitOfWork transaction,
                              final String key ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject localizedText = null;

        if ( hasChild( transaction, KomodoLexicon.Rule.MESSAGES ) ) {
            final KomodoObject messages = getChild( transaction,
                                                    KomodoLexicon.Rule.MESSAGES,
                                                    KomodoLexicon.Rule.LOCALIZED_MESSAGE_GROUPING );

            if ( messages.hasChild( transaction, key, KomodoLexicon.Rule.LOCALIZED_MESSAGE ) ) {
                final KomodoObject message = messages.getChild( transaction, key, KomodoLexicon.Rule.LOCALIZED_MESSAGE );

                if ( message.hasChild( transaction, getLocaleCode( true, true ), KomodoLexicon.Rule.LOCALIZED_TEXT_TYPE ) ) {
                    localizedText = message.getChild( transaction,
                                                      getLocaleCode( true, true ),
                                                      KomodoLexicon.Rule.LOCALIZED_TEXT_TYPE );
                } else if ( message.hasChild( transaction, getLocaleCode( true, false ), KomodoLexicon.Rule.LOCALIZED_TEXT_TYPE ) ) {
                    localizedText = message.getChild( transaction,
                                                      getLocaleCode( true, false ),
                                                      KomodoLexicon.Rule.LOCALIZED_TEXT_TYPE );
                } else if ( message.hasChild( transaction, getLocaleCode( false, false ), KomodoLexicon.Rule.LOCALIZED_TEXT_TYPE ) ) {
                    localizedText = message.getChild( transaction,
                                                      getLocaleCode( false, false ),
                                                      KomodoLexicon.Rule.LOCALIZED_TEXT_TYPE );
                }
            }
        }

        if ( localizedText == null ) {
            return null;
        }

        final Property prop = localizedText.getProperty( transaction, KomodoLexicon.Rule.LOCALIZED_TEXT );
        return prop.getStringValue( transaction );
    }
    
    /*
     * Gets the message for the specified key.  If a 'message' is not found for the key, the rule 'definition' is returned. 
     */
    private String getMessageOrDescription( final UnitOfWork transaction,
                                            final String key ) throws KException {
        
        String errorMsg = getMessage( transaction, key );

        // if error message for specified key not found, use description.
        if ( StringUtils.isBlank( errorMsg ) ) {
            errorMsg = getDescription( transaction );
        }
        
        return errorMsg;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getNodeType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNodeType( final UnitOfWork transaction ) throws KException {
        return getObjectProperty( transaction, PropertyValueType.STRING, "getNodeType", KomodoLexicon.Rule.NODE_TYPE ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getPropRestrictions(org.komodo.spi.repository.Repository.UnitOfWork,org.komodo.spi.repository.validation.Rule.PropertyRestriction)
     */
    @Override
    public Map<String, String> getPropRestrictions(UnitOfWork transaction, PropertyRestriction restrictionType) throws KException {
        Map<String,String> resultMap = new HashMap<String,String>();
        
        if ( hasChild( transaction, KomodoLexicon.Rule.PROP_RESTRICTIONS_GROUPING ) ) {
            final KomodoObject propRestrictions = getChild( transaction,
                                                    KomodoLexicon.Rule.PROP_RESTRICTIONS_GROUPING,
                                                    KomodoLexicon.Rule.PROP_RESTRICTIONS_GROUPING );

            KomodoObject[] restrictionNodes = propRestrictions.getChildrenOfType(transaction, KomodoLexicon.Rule.PROP_RESTRICTION);
            for(KomodoObject kObj : restrictionNodes) {
                String propName = kObj.getName(transaction);
                String propValue = kObj.getProperty(transaction, KomodoLexicon.Rule.PROP_VALUE).getStringValue(transaction);
                String type = kObj.getProperty(transaction, KomodoLexicon.Rule.RESTRICTION_TYPE).getStringValue(transaction);
                if(restrictionType.name().equals(type)) {
                    resultMap.put(propName, propValue);
                }
            }
        }

        return resultMap;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getRuleType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public RuleType getRuleType( final UnitOfWork transaction ) throws KException {
        final String value = getObjectProperty( transaction,
                                                PropertyValueType.STRING,
                                                "getRuleType", KomodoLexicon.Rule.RULE_TYPE ); //$NON-NLS-1$

        try {
            final RuleType result = RuleType.valueOf( value );
            return result;
        } catch ( final Exception e ) {
            throw new KException( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getSeverity(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Level getSeverity( final UnitOfWork transaction ) throws KException {
        final String value = getObjectProperty( transaction, PropertyValueType.STRING, "getSeverity", KomodoLexicon.Rule.SEVERITY ); //$NON-NLS-1$

        try {
            final Level result = Level.valueOf( value );
            return result;
        } catch ( final Exception e ) {
            return Level.ERROR;
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getValidationType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ValidationType getValidationType( final UnitOfWork transaction ) throws KException {
        final String value = getObjectProperty( transaction, 
                                                PropertyValueType.STRING, "getValidationType", //$NON-NLS-1$
                                                KomodoLexicon.Rule.VALIDATION_TYPE );

        try {
            final ValidationType result = ValidationType.valueOf( value );
            return result;
        } catch ( final Exception e ) {
            throw new KException( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#isRequired(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isRequired(UnitOfWork transaction) throws KException {
        final Boolean required = getObjectProperty( transaction,
                                                    PropertyValueType.BOOLEAN,
                                                    "isRequired", KomodoLexicon.Rule.REQUIRED ); //$NON-NLS-1$
        return ( required == null ) ? true : required;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#setRequired(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setRequired(UnitOfWork transaction,
                            boolean newRequired) throws KException {
        setObjectProperty( transaction, "setRequired", KomodoLexicon.Rule.REQUIRED, newRequired ); //$NON-NLS-1$
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#isApplicable(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public boolean isApplicable(UnitOfWork transaction,
                                KomodoObject kObject) throws KException {
        // Object type must be compatible with the rule.
        if (!ValidationUtils.objectTypeMatches(transaction, kObject, getNodeType(transaction))) return false;
        
        // Object properties must be compatible with the rule.
        if (!ValidationUtils.objectPropsPassRestrictions(transaction, kObject, getPropRestrictions(transaction,Rule.PropertyRestriction.NODE))) return false;
        
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#isEnabled(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isEnabled( final UnitOfWork transaction ) throws KException {
        final Boolean enabled = getObjectProperty( transaction,
                                                   PropertyValueType.BOOLEAN,
                                                   "isEnabled", KomodoLexicon.Rule.ENABLED ); //$NON-NLS-1$
        return ( enabled == null ) ? true : enabled;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#setEnabled(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setEnabled( final UnitOfWork transaction,
                            final boolean newEnabled ) throws KException {
        setObjectProperty( transaction, "setEnabled", KomodoLexicon.Rule.ENABLED, newEnabled ); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#setSeverity(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.outcome.Outcome.Level)
     */
    @Override
    public void setSeverity( final UnitOfWork transaction,
                             final Level newLevel ) throws KException {
        setObjectProperty( transaction, "setSeverity", KomodoLexicon.Rule.ENABLED, newLevel ); //$NON-NLS-1$
    }

}
