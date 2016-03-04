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

import java.io.File;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import org.komodo.core.Messages;
import org.komodo.spi.KException;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.spi.repository.validation.Rule.MessageKey;
import org.komodo.spi.repository.validation.RuleConstants;
import org.komodo.spi.utils.LocalizedMessage;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;
import org.komodo.utils.StringUtils;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.DefaultHandler;

/**
 * The <code>RuleValidationParser</code> parses a validation rule definition file.  This parser will generate the Rules in the repository and return the
 * array of generated rules.
 */
public class RuleParser {

    private static final KLog LOGGER = KLog.getLogger();

    private final Repository repo;
    private final UnitOfWork uow;
    private Handler handler;
    /**
     * The parser of the rules file
     */
    private SAXParser parser;

    /**
     * Constructs a parser.
     * @param rulesSchemaFile the rules schema file
     * @param repo
     *        the repository (cannot be <code>null</code>)
     * @param transaction
     *        the transaction (cannot be <code>null</code> or have a state that is not {@link State#NOT_STARTED})
     * @throws Exception if there were problems with the rules schema file
     */
    public RuleParser(final File rulesSchemaFile, final Repository repo, final UnitOfWork transaction) throws Exception {
        ArgCheck.isNotNull( rulesSchemaFile, "rulesSchemaFile" ); //$NON-NLS-1$
        ArgCheck.isNotNull( repo, "repo" ); //$NON-NLS-1$
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        this.repo = repo;
        this.uow = transaction;
        
        setupParser(rulesSchemaFile);
    }
    
    private void setupParser(File schemaFile) throws Exception {
        if (!schemaFile.exists()) {
            throw new IllegalStateException( Messages.getString(Messages.RuleParser.Rules_Schema_File_Not_Found));
        }
        
        // create parser
        final SAXParserFactory factory = SAXParserFactory.newInstance();
        factory.setNamespaceAware( true );
        factory.setValidating( true );

        this.parser = factory.newSAXParser();
        this.parser.setProperty( "http://java.sun.com/xml/jaxp/properties/schemaLanguage", "http://www.w3.org/2001/XMLSchema" ); //$NON-NLS-1$ //$NON-NLS-2$
        this.parser.setProperty( "http://java.sun.com/xml/jaxp/properties/schemaSource", schemaFile ); //$NON-NLS-1$
    }

    /**
     * @return the error messages output from the last parse operation 
     *   or <code>null</code> if parse has not been called
     */
    public Collection<String> getErrors() {
        if (this.handler == null) {
            return null;
        }

        return this.handler.getErrors();
    }

    /**
     * @return the fatal error messages output from the last parse operation 
     *   or <code>null</code> if parse has not been called
     */
    public Collection<String> getFatalErrors() {
        if (this.handler == null) {
            return null;
        }

        return this.handler.getFatalErrors();
    }

    /**
     * @return the information messages output from the last parse operation 
     *   or <code>null</code> if parse has not been called
     */
    public Collection<String> getInfos() {
        if (this.handler == null) {
            return null;
        }

        return this.handler.getInfos();
    }

    /**
     * @return the warning messages output from the last parse operation 
     *   or <code>null</code> if parse has not been called
     */
    public Collection<String> getWarnings() {
        if (this.handler == null) {
            return null;
        }

        return this.handler.getWarnings();
    }

    /**
     * @param rulesFile the rules file (cannot be <code>null</code>)
     * @return the Rules (never <code>null</code>)
     * @throws Exception if the definition file is <code>null</code> or if there is a problem parsing the file
     */
    public Rule[] parse( File rulesFile ) throws Exception {
        this.handler = new Handler();
        this.parser.parse(rulesFile, handler);
        return this.handler.getRules();
    }

    /**
     * The handler used by the parser. Each instance should be only used to parse one file.
     */
    class Handler extends DefaultHandler {

        private final Map<String,String> nodePropRestrictionMap;
        private final Map<String,String> childPropRestrictionMap;
        private final Map<String,String> nodeValidationMessageMap;
        private final Map<String,String> propertyValidationMessageMap;
        private final Map<String,String> childValidationMessageMap;
        private final List<LocalizedMessage> ruleLocalizedMessages;
        private final List<LocalizedMessage> ruleLocalizedDescriptions;
        
        private final Collection<String> fatals;
        private final Collection<String> errors;
        private final Collection<String> infos;
        private final Collection<String> warnings;
        
        private final Collection<Rule> rules;

        private StringBuilder description = new StringBuilder();
        private StringBuilder message = new StringBuilder();
        private StringBuilder pattern = new StringBuilder();
        private StringBuilder propExists = new StringBuilder();
        private StringBuilder propAbsent = new StringBuilder();
        private StringBuilder childExists = new StringBuilder();
        private StringBuilder childAbsent = new StringBuilder();
        private StringBuilder minValue = new StringBuilder();
        private StringBuilder maxValue = new StringBuilder();
        private StringBuilder propRestrictionName = new StringBuilder();
        
        private List<String> propMustExistList;
        private List<String> propMustNotExistList;
        private List<String> childTypeMustExistList;
        private List<String> childTypeMustNotExistList;
        
        private final Stack<String> elements;
        private String ruleId;
        private String nodeValidationJcrName;
        private String childValidationJcrName;
        private String propertyValidationJcrName;
        private String propertyValidationPropRequired;
        private String childValidationChildRequired;
        private String descriptionLocale;
        private String messageLocale;
        private String messageKey;
        private String minInclusive;
        private String maxInclusive;
        private String severity;
        private String matchType;
        private String propRestrictionValue;

        public Handler( ) {
            this.elements = new Stack<String>();
            this.fatals = new ArrayList<String>();
            this.errors = new ArrayList<String>();
            this.infos = new ArrayList<String>();
            this.warnings = new ArrayList<String>();
            this.rules = new ArrayList<Rule>();
            this.nodePropRestrictionMap = new HashMap<String,String>();
            this.childPropRestrictionMap = new HashMap<String,String>();
            this.nodeValidationMessageMap = new HashMap<String,String>();
            this.propertyValidationMessageMap = new HashMap<String,String>();
            this.childValidationMessageMap = new HashMap<String,String>();
            this.ruleLocalizedDescriptions = new ArrayList<LocalizedMessage>();
            this.ruleLocalizedMessages = new ArrayList<LocalizedMessage>();
            this.propMustExistList = new ArrayList<String>();
            this.propMustNotExistList = new ArrayList<String>();
            this.childTypeMustExistList = new ArrayList<String>();
            this.childTypeMustNotExistList = new ArrayList<String>();
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#characters(char[], int, int)
         */
        @Override
        public void characters( char[] ch,
                                int start,
                                int length ) throws SAXException {
            String value = new String(ch, start, length);

            if (RuleConstants.Elements.DESCRIPTION.equals(getCurrentElement())) {
                this.description.append(value);
            } else if (RuleConstants.Elements.MESSAGE.equals(getCurrentElement())) {
                this.message.append(value);
            } else if (RuleConstants.Elements.PATTERN.equals(getCurrentElement())) {
                this.pattern.append(value);
            } else if (RuleConstants.Elements.PROP_EXISTS.equals(getCurrentElement())) {
                this.propExists.append(value);
            } else if (RuleConstants.Elements.PROP_ABSENT.equals(getCurrentElement())) {
                this.propAbsent.append(value);
            } else if (RuleConstants.Elements.CHILD_EXISTS.equals(getCurrentElement())) {
                this.childExists.append(value);
            } else if (RuleConstants.Elements.CHILD_ABSENT.equals(getCurrentElement())) {
                this.childAbsent.append(value);
            } else if (RuleConstants.Elements.MIN_VALUE.equals(getCurrentElement())) {
                this.minValue.append(value);
            } else if (RuleConstants.Elements.MAX_VALUE.equals(getCurrentElement())) {
                this.maxValue.append(value);
            } else if (RuleConstants.Elements.PROP_RESTRICTION.equals(getCurrentElement())) {
                this.propRestrictionName.append(value);
            }

            super.characters(ch, start, length);
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#endElement(java.lang.String, java.lang.String, java.lang.String)
         */
        @Override
        public void endElement( String uri,
                                String localName,
                                String qName ) throws SAXException {

            if(RuleConstants.Elements.NODE_VALIDATION.equals(localName)) {
                this.nodeValidationJcrName = null;
            } else if(RuleConstants.Elements.NAME_VALIDATION.equals(localName)) {
                Rule theRule = null;
                try {
                    Outcome.Level levelSeverity = Outcome.Level.valueOf(severity);
                    theRule = RuleFactory.createNodeNameRule(uow, 
                                                             repo, 
                                                             ruleId, 
                                                             nodeValidationJcrName, 
                                                             nodePropRestrictionMap, 
                                                             pattern.toString(), 
                                                             levelSeverity, 
                                                             ruleLocalizedDescriptions, 
                                                             ruleLocalizedMessages);
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating NodeNameRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);
                
                this.ruleId = null;
                pattern.setLength(0);
                ruleLocalizedDescriptions.clear();
                ruleLocalizedMessages.clear();
            } else if(RuleConstants.Elements.PROPERTY_VALIDATION.equals(localName)) {
                this.propertyValidationJcrName = null;
                this.propertyValidationPropRequired = null;
            } else if(RuleConstants.Elements.SAME_NAME_SIBLING_VALIDATION.equals(localName)) {
                boolean bMatchType = true;
                if(matchType!=null) {
                    bMatchType = Boolean.parseBoolean(matchType);
                }
                
                Rule theRule = null;
                try {
                    boolean bChildRqd = false;
                    if(childValidationChildRequired!=null) {
                        bChildRqd = Boolean.parseBoolean(childValidationChildRequired);
                    }

                    Outcome.Level levelSeverity = Outcome.Level.valueOf(severity);
                    theRule = RuleFactory.createSameNameSiblingValidationRule(uow, 
                                                                              repo, 
                                                                              ruleId, 
                                                                              nodeValidationJcrName, 
                                                                              nodePropRestrictionMap, 
                                                                              childValidationJcrName,
                                                                              childPropRestrictionMap,
                                                                              bChildRqd, 
                                                                              bMatchType, 
                                                                              levelSeverity, 
                                                                              ruleLocalizedDescriptions, 
                                                                              ruleLocalizedMessages);
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating SameNameSiblingValidationRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);
                
                this.ruleId = null;
                this.matchType = null;
                ruleLocalizedDescriptions.clear();
                ruleLocalizedMessages.clear();
            } else if(RuleConstants.Elements.CHILD_COUNT_VALIDATION.equals(localName)) {
                boolean bMinInclusive = true;
                if(minInclusive!=null) {
                    bMinInclusive = Boolean.parseBoolean(minInclusive);
                }
                boolean bMaxInclusive = true;
                if(maxInclusive!=null) {
                    bMaxInclusive = Boolean.parseBoolean(maxInclusive);
                }
                
                Number nMinValue = null;
                Number nMaxValue = null;
                try {
                    if(minValue!=null && minValue.length()>0) {
                        nMinValue = NumberFormat.getInstance().parse( minValue.toString() );
                    }
                    if(maxValue!=null && maxValue.length()>0) {
                        nMaxValue = NumberFormat.getInstance().parse( maxValue.toString() );
                    }
                } catch (ParseException ex) {
                    LOGGER.error("RuleParser - error parsing property min-max values : ", ex); //$NON-NLS-1$
                }
                
                Rule theRule = null;
                try {
                    boolean bChildRqd = false;
                    if(childValidationChildRequired!=null) {
                        bChildRqd = Boolean.parseBoolean(childValidationChildRequired);
                    }
                    Outcome.Level levelSeverity = Outcome.Level.valueOf(severity);
                    theRule = RuleFactory.createChildCountValidationRule(uow, 
                                                                         repo, 
                                                                         ruleId, 
                                                                         nodeValidationJcrName, 
                                                                         nodePropRestrictionMap, 
                                                                         childValidationJcrName,
                                                                         childPropRestrictionMap,
                                                                         bChildRqd,
                                                                         nMinValue, 
                                                                         bMinInclusive, 
                                                                         nMaxValue, 
                                                                         bMaxInclusive,
                                                                         levelSeverity,
                                                                         ruleLocalizedDescriptions, 
                                                                         ruleLocalizedMessages);
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating ChildCountValidationRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);
                
                this.ruleId = null;
                minValue.setLength(0);
                maxValue.setLength(0);
                minInclusive = null;
                maxInclusive = null;
                ruleLocalizedDescriptions.clear();
                ruleLocalizedMessages.clear();
            } else if(RuleConstants.Elements.VALUE_VALIDATION.equals(localName)) {
                boolean hasChildNodeJcrName = (StringUtils.isEmpty(childValidationJcrName)) ? false : true;
                String nodeName = hasChildNodeJcrName ? childValidationJcrName : nodeValidationJcrName;

                Rule theRule = null;
                try {
                    boolean bPropRqd = false;
                    if(propertyValidationPropRequired!=null) {
                        bPropRqd = Boolean.parseBoolean(propertyValidationPropRequired);
                    }
                    
                    Outcome.Level levelSeverity = Outcome.Level.valueOf(severity);
                    theRule = RuleFactory.createPropertyPatternRule(uow, 
                                                                    repo, 
                                                                    ruleId, 
                                                                    nodeName, 
                                                                    nodePropRestrictionMap, 
                                                                    propertyValidationJcrName, 
                                                                    bPropRqd, 
                                                                    pattern.toString(), 
                                                                    levelSeverity, 
                                                                    ruleLocalizedDescriptions, 
                                                                    ruleLocalizedMessages);
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating PropertyPatternRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);

                this.ruleId = null;
                pattern.setLength(0);
                ruleLocalizedDescriptions.clear();
                ruleLocalizedMessages.clear();
            } else if(RuleConstants.Elements.RELATIONSHIP_VALIDATION.equals(localName)) {
                boolean hasChildNodeJcrName = (StringUtils.isEmpty(childValidationJcrName)) ? false : true;

                Rule theRule = null;
                try {
                    if(hasChildNodeJcrName) {
                        boolean bChildRqd = false;
                        if(childValidationChildRequired!=null) {
                            bChildRqd = Boolean.parseBoolean(childValidationChildRequired);
                        }
                        Outcome.Level levelSeverity = Outcome.Level.valueOf(severity);
                        theRule = RuleFactory.createChildRelationshipValidationRule(uow, 
                                                                                    repo, 
                                                                                    ruleId, 
                                                                                    nodeValidationJcrName, 
                                                                                    nodePropRestrictionMap, 
                                                                                    childValidationJcrName,
                                                                                    childPropRestrictionMap,
                                                                                    bChildRqd,
                                                                                    propMustExistList, 
                                                                                    propMustNotExistList, 
                                                                                    childTypeMustExistList, 
                                                                                    childTypeMustNotExistList,
                                                                                    levelSeverity,
                                                                                    ruleLocalizedDescriptions, 
                                                                                    ruleLocalizedMessages);
                    } else {
                        boolean bPropRqd = false;
                        if(propertyValidationPropRequired!=null) {
                            bPropRqd = Boolean.parseBoolean(propertyValidationPropRequired);
                        }
                        Outcome.Level levelSeverity = Outcome.Level.valueOf(severity);
                        theRule = RuleFactory.createPropertyRelationshipValidationRule(uow, 
                                                                                       repo, 
                                                                                       ruleId, 
                                                                                       nodeValidationJcrName, 
                                                                                       nodePropRestrictionMap, 
                                                                                       propertyValidationJcrName,
                                                                                       bPropRqd,
                                                                                       propMustExistList, 
                                                                                       propMustNotExistList, 
                                                                                       childTypeMustExistList, 
                                                                                       childTypeMustNotExistList,
                                                                                       levelSeverity,
                                                                                       ruleLocalizedDescriptions, 
                                                                                       ruleLocalizedMessages);
                    }
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating ChildRelationshipValidationRule : ", ex); //$NON-NLS-1$
                }

                if(theRule!=null) rules.add(theRule);
                
                this.ruleId = null;
                propMustExistList.clear();
                propMustNotExistList.clear();
                childTypeMustExistList.clear();
                childTypeMustNotExistList.clear();
                ruleLocalizedDescriptions.clear();
                ruleLocalizedMessages.clear();
            } else if(RuleConstants.Elements.VALUE_RANGE_VALIDATION.equals(localName)) {
                boolean hasChildNodeJcrName = (StringUtils.isEmpty(childValidationJcrName)) ? false : true;
                String nodeName = hasChildNodeJcrName ? childValidationJcrName : nodeValidationJcrName;

                boolean bMinInclusive = true;
                if(minInclusive!=null) {
                    bMinInclusive = Boolean.parseBoolean(minInclusive);
                }
                boolean bMaxInclusive = true;
                if(maxInclusive!=null) {
                    bMaxInclusive = Boolean.parseBoolean(maxInclusive);
                }
                
                Number nMinValue = null;
                Number nMaxValue = null;
                try {
                    if(minValue!=null && minValue.length()>0) {
                        nMinValue = NumberFormat.getInstance().parse( minValue.toString() );
                    }
                    if(maxValue!=null && maxValue.length()>0) {
                        nMaxValue = NumberFormat.getInstance().parse( maxValue.toString() );
                    }
                } catch (ParseException ex) {
                    LOGGER.error("RuleParser - error parsing property min-max values : ", ex); //$NON-NLS-1$
                }
                
                Rule theRule = null;
                try {
                    boolean bPropRqd = false;
                    if(propertyValidationPropRequired!=null) {
                        bPropRqd = Boolean.parseBoolean(propertyValidationPropRequired);
                    }
                    Outcome.Level levelSeverity = Outcome.Level.valueOf(severity);
                    theRule = RuleFactory.createPropertyValueNumberValidationRule(uow, 
                                                                                     repo, 
                                                                                     ruleId, 
                                                                                     nodeName, 
                                                                                     nodePropRestrictionMap, 
                                                                                     propertyValidationJcrName, 
                                                                                     bPropRqd,
                                                                                     nMinValue, 
                                                                                     bMinInclusive, 
                                                                                     nMaxValue, 
                                                                                     bMaxInclusive,
                                                                                     levelSeverity,
                                                                                     ruleLocalizedDescriptions, 
                                                                                     ruleLocalizedMessages);
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating PropertyValueNumberValidationRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);

                this.ruleId = null;
                minValue.setLength(0);
                maxValue.setLength(0);
                minInclusive = null;
                maxInclusive = null;
                ruleLocalizedDescriptions.clear();
                ruleLocalizedMessages.clear();
            } else if(RuleConstants.Elements.CHILD_VALIDATION.equals(localName)) {
                this.childValidationJcrName = null;
                this.childValidationChildRequired = null;
            } else if(RuleConstants.Elements.MESSAGE.equals(localName)) {
                // Put the locale and message into the proper message map
                if(this.ruleId!=null) {
                    this.ruleLocalizedMessages.add(new LocalizedMessage(this.messageKey,this.messageLocale, this.message.toString()));
                }
                
                this.messageLocale = null;
                this.messageKey = null;
                this.message.setLength(0);
            } else if(RuleConstants.Elements.PROP_RESTRICTION.equals(localName)) {
                // Put the prop name and value into the property restriction map
                if(this.childValidationJcrName != null) {
                    this.childPropRestrictionMap.put(this.propRestrictionName.toString(), this.propRestrictionValue);
                } else {
                    this.nodePropRestrictionMap.put(this.propRestrictionName.toString(), this.propRestrictionValue);
                }
                
                this.propRestrictionValue = null;
                this.propRestrictionName.setLength(0);
            } else if(RuleConstants.Elements.DESCRIPTION.equals(localName)) {
                // Put the locale and description into the proper description map
                if(this.ruleId!=null) {
                    this.ruleLocalizedDescriptions.add(new LocalizedMessage(MessageKey.DESCRIPTION.name(),this.descriptionLocale, this.description.toString()));
                }
                
                this.descriptionLocale = null;
                this.description.setLength(0);
            } else if(RuleConstants.Elements.PROP_EXISTS.equals(localName)) {
                this.propMustExistList.add(this.propExists.toString());
                this.propExists.setLength(0);
            } else if(RuleConstants.Elements.PROP_ABSENT.equals(localName)) {
                this.propMustNotExistList.add(this.propAbsent.toString());
                this.propAbsent.setLength(0);
            } else if(RuleConstants.Elements.CHILD_EXISTS.equals(localName)) {
                this.childTypeMustExistList.add(this.childExists.toString());
                this.childExists.setLength(0);
            } else if(RuleConstants.Elements.CHILD_ABSENT.equals(localName)) {
                this.childTypeMustNotExistList.add(this.childAbsent.toString());
                this.childAbsent.setLength(0);
            }

            this.elements.pop();

            super.endElement(uri, localName, qName);
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#error(org.xml.sax.SAXParseException)
         */
        @Override
        public void error( SAXParseException e ) {
            this.errors.add(e.getLocalizedMessage());
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#fatalError(org.xml.sax.SAXParseException)
         */
        @Override
        public void fatalError( SAXParseException e ) {
            this.fatals.add(e.getLocalizedMessage());
        }
        
        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#warning(org.xml.sax.SAXParseException)
         */
        @Override
        public void warning( SAXParseException e ) {
            this.warnings.add(e.getLocalizedMessage());
        }

        /**
         * @return the element currently being parsed
         */
        private String getCurrentElement() {
            if (this.elements.empty()) {
                return null;
            }

            return this.elements.peek();
        }

        /**
         * @return the error messages output from the last parse operation (never <code>null</code> but can be empty)
         */
        Collection<String> getErrors() {
            return this.errors;
        }

        /**
         * @return the fatal error messages output from the last parse operation (never <code>null</code> but can be empty)
         */
        Collection<String> getFatalErrors() {
            return this.fatals;
        }

        /**
         * @return the information messages output from the last parse operation (never <code>null</code> but can be empty)
         */
        Collection<String> getInfos() {
            return this.infos;
        }

        /**
         * @return the warning messages output from the last parse operation (never <code>null</code> but can be empty)
         */
        Collection<String> getWarnings() {
            return this.warnings;
        }

        /**
         * @return the rules (never <code>null</code> but can be empty)
         */
        Rule[] getRules() {
            return rules.toArray( new Rule[ rules.size() ] );
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String,
         *      org.xml.sax.Attributes)
         */
        @Override
        public void startElement( String uri,
                                  String localName,
                                  String qName,
                                  Attributes attributes ) throws SAXException {

            this.elements.push(localName);

            if(RuleConstants.Elements.VALIDATION_RULE_SET.equals(localName)) {
                this.rules.clear();
            } else if(RuleConstants.Elements.NODE_VALIDATION.equals(localName)) {
                this.nodeValidationJcrName = attributes.getValue(RuleConstants.Attributes.JCR_NAME);
                this.nodeValidationMessageMap.clear();
                this.nodePropRestrictionMap.clear();
            } else if(RuleConstants.Elements.NAME_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.ruleLocalizedMessages.clear();
                this.ruleLocalizedDescriptions.clear();
                this.severity = attributes.getValue(RuleConstants.Attributes.SEVERITY);
            } else if(RuleConstants.Elements.PROPERTY_VALIDATION.equals(localName)) {
                this.propertyValidationJcrName = attributes.getValue(RuleConstants.Attributes.JCR_NAME);
                this.propertyValidationPropRequired = attributes.getValue(RuleConstants.Attributes.REQUIRED);
                this.propertyValidationMessageMap.clear();
            } else if(RuleConstants.Elements.SAME_NAME_SIBLING_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.matchType = attributes.getValue(RuleConstants.Attributes.MATCH_TYPE);
                this.ruleLocalizedMessages.clear();
                this.ruleLocalizedDescriptions.clear();
                this.severity = attributes.getValue(RuleConstants.Attributes.SEVERITY);
            } else if(RuleConstants.Elements.CHILD_COUNT_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.ruleLocalizedMessages.clear();
                this.ruleLocalizedDescriptions.clear();
                this.severity = attributes.getValue(RuleConstants.Attributes.SEVERITY);
            } else if(RuleConstants.Elements.VALUE_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.ruleLocalizedMessages.clear();
                this.ruleLocalizedDescriptions.clear();
                this.severity = attributes.getValue(RuleConstants.Attributes.SEVERITY);
            } else if(RuleConstants.Elements.RELATIONSHIP_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.ruleLocalizedMessages.clear();
                this.ruleLocalizedDescriptions.clear();
                this.propMustExistList.clear();
                this.propMustNotExistList.clear();
                this.childTypeMustExistList.clear();
                this.childTypeMustNotExistList.clear();
                this.severity = attributes.getValue(RuleConstants.Attributes.SEVERITY);
            } else if(RuleConstants.Elements.VALUE_RANGE_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.ruleLocalizedMessages.clear();
                this.ruleLocalizedDescriptions.clear();
                this.severity = attributes.getValue(RuleConstants.Attributes.SEVERITY);
            } else if(RuleConstants.Elements.CHILD_VALIDATION.equals(localName)) {
                this.childValidationJcrName = attributes.getValue(RuleConstants.Attributes.JCR_NAME);
                this.childValidationChildRequired = attributes.getValue(RuleConstants.Attributes.REQUIRED);
                this.childValidationMessageMap.clear();
                this.childPropRestrictionMap.clear();
            } else if(RuleConstants.Elements.DESCRIPTION.equals(localName)) {
                this.descriptionLocale = attributes.getValue(RuleConstants.Attributes.LOCALE);
            } else if(RuleConstants.Elements.MESSAGE.equals(localName)) {
                this.messageLocale = attributes.getValue(RuleConstants.Attributes.LOCALE);
                this.messageKey = attributes.getValue(RuleConstants.Attributes.KEY);
            } else if(RuleConstants.Elements.PROP_RESTRICTION.equals(localName)) {
                this.propRestrictionValue = attributes.getValue(RuleConstants.Attributes.VALUE);
            } else if(RuleConstants.Elements.MIN_VALUE.equals(localName)) {
                this.minInclusive = attributes.getValue(RuleConstants.Attributes.INCLUSIVE);
            } else if(RuleConstants.Elements.MAX_VALUE.equals(localName)) {
                this.maxInclusive = attributes.getValue(RuleConstants.Attributes.INCLUSIVE);
            }
                        
            super.startElement(uri, localName, qName, attributes);
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#skippedEntity(java.lang.String)
         */
        @Override
        public void skippedEntity( String name ) {
            this.infos.add(Messages.getString(Messages.RuleParser.Rules_Schema_File_Not_Found,name));
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#unparsedEntityDecl(java.lang.String, java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        @Override
        public void unparsedEntityDecl( String name,
                                        String publicId,
                                        String systemId,
                                        String notationName ) {
            this.infos.add(Messages.getString(Messages.RuleParser.Rules_Schema_File_Not_Found,name));
        }

    }

}
