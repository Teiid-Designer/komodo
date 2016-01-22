/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
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
import org.komodo.spi.KException;
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
 * The <code>RuleParser</code> parses validation rule definition files.   Each file is validated
 * against a validation rule definition schema.
 */
public class RuleParser {

    private static final KLog LOGGER = KLog.getLogger();

    private final Repository repo;
    private final UnitOfWork uow;
    //private final File ruleSchemaFile;
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
        ArgCheck.isNotNull( repo, "repo" ); //$NON-NLS-1$
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        this.repo = repo;
        this.uow = transaction;
        
        // load validation XSD for validation using parser
        //this.ruleSchemaFile = new File( "../../plugins/org.komodo.core/resources/komodoValidation.xsd" ); //$NON-NLS-1$
        
        setupParser(rulesSchemaFile);
    }
    
    private void setupParser(File schemaFile) throws Exception {
        if (!schemaFile.exists()) {
            throw new IllegalStateException( "Validation schema file does not exist" ); //$NON-NLS-1$
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
     * @param rulesFile the rules input stream (cannot be <code>null</code>)
     * @return the Rules (never <code>null</code>)
     * @throws Exception if the definition file is <code>null</code> or if there is a problem parsing the file
     */
    public Collection<Rule> parse( File rulesFile ) throws Exception {
        this.handler = new Handler();
        this.parser.parse(rulesFile, handler);
        return this.handler.getRules();
    }

    /**
     * The handler used by the parser. Each instance should be only used to parse one file.
     */
    class Handler extends DefaultHandler {

        private final Map<String,String> nodePropRestrictionMap;
        private final Map<String,String> nodeValidationMessageMap;
        private final Map<String,String> propertyValidationMessageMap;
        private final Map<String,String> childValidationMessageMap;
        private final Map<String,String> ruleMessageMap;
        private final Map<String,String> ruleDescriptionMap;
        
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
            this.nodeValidationMessageMap = new HashMap<String,String>();
            this.propertyValidationMessageMap = new HashMap<String,String>();
            this.childValidationMessageMap = new HashMap<String,String>();
            this.ruleMessageMap = new HashMap<String,String>();
            this.ruleDescriptionMap = new HashMap<String,String>();
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
                List<LocalizedMessage> descriptions = getLocalizedMessageList(MessageKey.DESCRIPTION.name(), ruleDescriptionMap);
                List<LocalizedMessage> messages = getLocalizedMessageList(MessageKey.PATTERN_RULE_INVALID_NODE_NAME.name(), ruleMessageMap);
                
                Rule theRule = null;
                try {
                    theRule = RuleFactory.createNodeNameRule(uow, repo, ruleId, nodeValidationJcrName, nodePropRestrictionMap, pattern.toString(), descriptions, messages);
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating NodeNameRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);
                
                this.ruleId = null;
                pattern.setLength(0);
                ruleDescriptionMap.clear();
                ruleMessageMap.clear();
            } else if(RuleConstants.Elements.PROPERTY_VALIDATION.equals(localName)) {
                this.propertyValidationJcrName = null;
            } else if(RuleConstants.Elements.REQUIRED_VALIDATION.equals(localName)) {
                List<LocalizedMessage> descriptions = getLocalizedMessageList(MessageKey.DESCRIPTION.name(), ruleDescriptionMap);
                List<LocalizedMessage> messages = getLocalizedMessageList(MessageKey.REQUIRED_PROPERTY_NOT_FOUND.name(), ruleMessageMap);
                
                boolean hasChildNodeJcrName = (StringUtils.isEmpty(childValidationJcrName)) ? false : true;

                Rule theRule = null;
                try {
                    if( hasChildNodeJcrName ) {
                        theRule = RuleFactory.createChildTypeRequiredRule(uow, repo, ruleId, nodeValidationJcrName, nodePropRestrictionMap, childValidationJcrName, descriptions, messages);
                    } else {
                        theRule = RuleFactory.createPropertyRequiredRule(uow, repo, ruleId, nodeValidationJcrName, nodePropRestrictionMap, propertyValidationJcrName, descriptions, messages);
                    }
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating PropertyRequiredRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);

                this.ruleId = null;
                ruleDescriptionMap.clear();
                ruleMessageMap.clear();
            } else if(RuleConstants.Elements.SAME_NAME_SIBLING_VALIDATION.equals(localName)) {
                List<LocalizedMessage> descriptions = getLocalizedMessageList(MessageKey.DESCRIPTION.name(), ruleDescriptionMap);
                List<LocalizedMessage> messages = getLocalizedMessageList(MessageKey.RELATIONSHIP_RULE_SNS_FOUND.name(), ruleMessageMap);

                boolean bMatchType = true;
                if(matchType!=null) {
                    bMatchType = Boolean.parseBoolean(matchType);
                }
                
                Rule theRule = null;
                try {
                    theRule = RuleFactory.createSameNameSiblingValidationRule(uow, repo, ruleId, nodeValidationJcrName, nodePropRestrictionMap, childValidationJcrName, bMatchType, descriptions, messages);
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating SameNameSiblingValidationRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);
                
                this.ruleId = null;
                this.matchType = null;
                ruleDescriptionMap.clear();
                ruleMessageMap.clear();
            } else if(RuleConstants.Elements.CHILD_COUNT_VALIDATION.equals(localName)) {
                List<LocalizedMessage> descriptions = getLocalizedMessageList(MessageKey.DESCRIPTION.name(), ruleDescriptionMap);
                List<LocalizedMessage> messages = getLocalizedMessageList(MessageKey.CHILD_COUNT_ABOVE_MAX_VALUE.name(), ruleMessageMap);
                
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
                    nMinValue = NumberFormat.getInstance().parse( minValue.toString() );
                    nMaxValue = NumberFormat.getInstance().parse( maxValue.toString() );
                } catch (ParseException ex) {
                    LOGGER.error("RuleParser - error parsing property min-max values : ", ex); //$NON-NLS-1$
                }
                
                Rule theRule = null;
                try {
                    theRule = RuleFactory.createChildCountValidationRule(uow, 
                                                                            repo, 
                                                                            ruleId, 
                                                                            nodeValidationJcrName, 
                                                                            nodePropRestrictionMap, 
                                                                            childValidationJcrName, 
                                                                            nMinValue, 
                                                                            bMinInclusive, 
                                                                            nMaxValue, 
                                                                            bMaxInclusive, 
                                                                            descriptions, 
                                                                            messages);
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating ChildCountValidationRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);
                
                this.ruleId = null;
                minValue.setLength(0);
                maxValue.setLength(0);
                minInclusive = null;
                maxInclusive = null;
                ruleDescriptionMap.clear();
                ruleMessageMap.clear();
            } else if(RuleConstants.Elements.VALUE_VALIDATION.equals(localName)) {
                List<LocalizedMessage> descriptions = getLocalizedMessageList(MessageKey.DESCRIPTION.name(), ruleDescriptionMap);
                List<LocalizedMessage> messages = getLocalizedMessageList(MessageKey.PROPERTY_RULE_ABSENT_PROPERTY_FOUND.name(), ruleMessageMap);
                
                boolean hasChildNodeJcrName = (StringUtils.isEmpty(childValidationJcrName)) ? false : true;
                String nodeName = hasChildNodeJcrName ? childValidationJcrName : nodeValidationJcrName;

                Rule theRule = null;
                try {
                    theRule = RuleFactory.createPropertyPatternRule(uow, repo, ruleId, nodeName, nodePropRestrictionMap, propertyValidationJcrName, pattern.toString(), descriptions, messages);
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating PropertyPatternRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);

                this.ruleId = null;
                pattern.setLength(0);
                ruleDescriptionMap.clear();
                ruleMessageMap.clear();
            } else if(RuleConstants.Elements.RELATIONSHIP_VALIDATION.equals(localName)) {
                List<LocalizedMessage> descriptions = getLocalizedMessageList(MessageKey.DESCRIPTION.name(), ruleDescriptionMap);
                List<LocalizedMessage> messages = getLocalizedMessageList(MessageKey.PROPERTY_RULE_ABSENT_PROPERTY_FOUND.name(), ruleMessageMap);
 
                boolean hasChildNodeJcrName = (StringUtils.isEmpty(childValidationJcrName)) ? false : true;

                Rule theRule = null;
                try {
                    if(hasChildNodeJcrName) {
                        theRule = RuleFactory.createChildRelationshipValidationRule(uow, 
                                                                                    repo, 
                                                                                    ruleId, 
                                                                                    nodeValidationJcrName, 
                                                                                    nodePropRestrictionMap, 
                                                                                    childValidationJcrName, 
                                                                                    propMustExistList, 
                                                                                    propMustNotExistList, 
                                                                                    childTypeMustExistList, 
                                                                                    childTypeMustNotExistList, 
                                                                                    descriptions, 
                                                                                    messages);
                    } else {
                        theRule = RuleFactory.createPropertyRelationshipValidationRule(uow, 
                                                                                       repo, 
                                                                                       ruleId, 
                                                                                       nodeValidationJcrName, 
                                                                                       nodePropRestrictionMap, 
                                                                                       propertyValidationJcrName, 
                                                                                       propMustExistList, 
                                                                                       propMustNotExistList, 
                                                                                       childTypeMustExistList, 
                                                                                       childTypeMustNotExistList, 
                                                                                       descriptions, 
                                                                                       messages);
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
                ruleDescriptionMap.clear();
                ruleMessageMap.clear();
            } else if(RuleConstants.Elements.VALUE_RANGE_VALIDATION.equals(localName)) {
                List<LocalizedMessage> descriptions = getLocalizedMessageList(MessageKey.DESCRIPTION.name(), ruleDescriptionMap);
                List<LocalizedMessage> messages = getLocalizedMessageList(MessageKey.PROPERTY_RULE_ABSENT_PROPERTY_FOUND.name(), ruleMessageMap);
                
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
                    nMinValue = NumberFormat.getInstance().parse( minValue.toString() );
                    nMaxValue = NumberFormat.getInstance().parse( maxValue.toString() );
                } catch (ParseException ex) {
                    LOGGER.error("RuleParser - error parsing property min-max values : ", ex); //$NON-NLS-1$
                }
                
                Rule theRule = null;
                try {
                    theRule = RuleFactory.createPropertyValueNumberValidationRule(uow, 
                                                                                     repo, 
                                                                                     ruleId, 
                                                                                     nodeName, 
                                                                                     nodePropRestrictionMap, 
                                                                                     propertyValidationJcrName, 
                                                                                     nMinValue, 
                                                                                     bMinInclusive, 
                                                                                     nMaxValue, 
                                                                                     bMaxInclusive, 
                                                                                     descriptions, 
                                                                                     messages);
                } catch (KException ex) {
                    LOGGER.error("RuleParser - error creating PropertyValueNumberValidationRule : ", ex); //$NON-NLS-1$
                }
                if(theRule!=null) rules.add(theRule);

                this.ruleId = null;
                minValue.setLength(0);
                maxValue.setLength(0);
                minInclusive = null;
                maxInclusive = null;
                ruleDescriptionMap.clear();
                ruleMessageMap.clear();
            } else if(RuleConstants.Elements.CHILD_VALIDATION.equals(localName)) {
                this.childValidationJcrName = null;
            } else if(RuleConstants.Elements.MESSAGE.equals(localName)) {
                // Put the locale and message into the proper message map
                if(this.ruleId!=null) {
                    this.ruleMessageMap.put(this.messageLocale, this.message.toString());
                }
                
                this.messageLocale = null;
                this.messageKey = null;
                this.message.setLength(0);
            } else if(RuleConstants.Elements.PROP_RESTRICTION.equals(localName)) {
                // Put the prop name and value into the map
                this.nodePropRestrictionMap.put(this.propRestrictionName.toString(), this.propRestrictionValue);
                
                this.propRestrictionValue = null;
                this.propRestrictionName.setLength(0);
            } else if(RuleConstants.Elements.DESCRIPTION.equals(localName)) {
                // Put the locale and description into the proper description map
                if(this.ruleId!=null) {
                    this.ruleDescriptionMap.put(this.descriptionLocale, this.description.toString());
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

        // Creates the collection of localized messages using the supplied key and locale-message map
        private List<LocalizedMessage> getLocalizedMessageList(String messageKey, Map<String,String> localeMessageMap) {
            List<LocalizedMessage> localizedList = new ArrayList(localeMessageMap.size());
            for(String locale : localeMessageMap.keySet()) {
                LocalizedMessage message = new LocalizedMessage(messageKey, locale, localeMessageMap.get(locale));
                localizedList.add(message);
            }
            return localizedList;
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

        Collection<Rule> getRules() {
            return this.rules;
        }

        /**
         * {@inheritDoc}
         * 
         * @see org.xml.sax.helpers.DefaultHandler#skippedEntity(java.lang.String)
         */
        @Override
        public void skippedEntity( String name ) {
            // this.infos.add(NLS.bind(Messages.parserFoundUnparsedEntityDeclaration, name));
            this.infos.add("Found Unparsed Entity Declaration: "+name);
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
                this.ruleMessageMap.clear();
                this.ruleDescriptionMap.clear();
            } else if(RuleConstants.Elements.PROPERTY_VALIDATION.equals(localName)) {
                this.propertyValidationJcrName = attributes.getValue(RuleConstants.Attributes.JCR_NAME);
                this.propertyValidationMessageMap.clear();
            } else if(RuleConstants.Elements.REQUIRED_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.ruleMessageMap.clear();
                this.ruleDescriptionMap.clear();
            } else if(RuleConstants.Elements.SAME_NAME_SIBLING_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.matchType = attributes.getValue(RuleConstants.Attributes.MATCH_TYPE);
                this.ruleMessageMap.clear();
                this.ruleDescriptionMap.clear();
            } else if(RuleConstants.Elements.CHILD_COUNT_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.ruleMessageMap.clear();
                this.ruleDescriptionMap.clear();
                this.severity = attributes.getValue(RuleConstants.Attributes.SEVERITY);
            } else if(RuleConstants.Elements.VALUE_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.ruleMessageMap.clear();
                this.ruleDescriptionMap.clear();
            } else if(RuleConstants.Elements.RELATIONSHIP_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.ruleMessageMap.clear();
                this.ruleDescriptionMap.clear();
                this.propMustExistList.clear();
                this.propMustNotExistList.clear();
                this.childTypeMustExistList.clear();
                this.childTypeMustNotExistList.clear();
            } else if(RuleConstants.Elements.VALUE_RANGE_VALIDATION.equals(localName)) {
                this.ruleId = attributes.getValue(RuleConstants.Attributes.ID);
                this.ruleMessageMap.clear();
                this.ruleDescriptionMap.clear();
            } else if(RuleConstants.Elements.CHILD_VALIDATION.equals(localName)) {
                this.childValidationJcrName = attributes.getValue(RuleConstants.Attributes.JCR_NAME);
                this.childValidationMessageMap.clear();
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
         * @see org.xml.sax.helpers.DefaultHandler#unparsedEntityDecl(java.lang.String, java.lang.String, java.lang.String,
         *      java.lang.String)
         */
        @Override
        public void unparsedEntityDecl( String name,
                                        String publicId,
                                        String systemId,
                                        String notationName ) {
//            this.infos.add(NLS.bind(Messages.parserFoundUnparsedEntityDeclaration, name));
            this.infos.add("Unparsed entity declaration for: "+name);
        }

    }

}
