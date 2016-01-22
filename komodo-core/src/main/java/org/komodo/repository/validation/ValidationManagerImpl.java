/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.repository.validation;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.komodo.core.KomodoLexicon;
import org.komodo.spi.KException;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.ValidationManager;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.utils.ArgCheck;

/**
 * An implementation of an {@link ValidationManager} that can be used by repositories.
 */
public class ValidationManagerImpl implements ValidationManager {

    private final Repository repo;
    private final File rulesSchemaFile = new File(getClass().getClassLoader().getResource("komodoValidation.xsd").getFile());

    /**
     * Constructs an environment store delegate.
     *
     * @param repo
     *        the repository this is a delegate for (cannot be <code>null</code>)
     */
    public ValidationManagerImpl( final Repository repo ) {
        ArgCheck.isNotNull( repo, "repo" ); //$NON-NLS-1$
        
        this.repo = repo;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.ValidationManager#validateRules(java.io.File)
     */
    @Override
    public List< String > validateRules( final File rulesXmlFile ) throws KException {
        ArgCheck.isNotNull( rulesXmlFile, "rulesXmlFile" ); //$NON-NLS-1$

        try {
            RuleValidationParser parser = new RuleValidationParser(rulesSchemaFile);
            parser.parse(rulesXmlFile);

            // return any errors
            List< String > result = new ArrayList<>();
            result.addAll( parser.getFatalErrors() );
            result.addAll( parser.getErrors() );
            return result;
        } catch ( final Exception e ) {
            throw new KException( e );
        }
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.repository.ValidationManager#importRules(java.io.File)
     */
    @Override
    public void importRules(final File rulesXmlFile, final UnitOfWork uow, boolean overwriteExisting) throws KException {
        ArgCheck.isNotNull( rulesXmlFile, "rulesXmlFile" ); //$NON-NLS-1$

        try {
            RuleParser parser = new RuleParser(rulesSchemaFile, this.repo, uow);
            
            // If overwriting existing rules, then clear them first
            if(overwriteExisting) {
                clearValidationRules(uow);
            }
            
            // Successful parsing adds rules to the repo
            parser.parse(rulesXmlFile);
        } catch ( final Exception e ) {
            throw new KException( e );
        }
    }
    
    private void clearValidationRules(final UnitOfWork uow) throws Exception {
        KomodoObject defaultValidationArea = RuleFactory.getValidationDefaultAreaNode(uow, this.repo);
        KomodoObject[] rules = defaultValidationArea.getChildren(uow);
        for(KomodoObject rule : rules) {
            rule.remove(uow);
        }
    }
    
    /* (non-Javadoc)
     * @see org.komodo.spi.repository.ValidationManager#getAllRules(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Rule[] getAllRules( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Rule > result = new ArrayList<>();
        KomodoObject defaultValidationArea = RuleFactory.getValidationDefaultAreaNode(transaction, this.repo);

        // Add SNS_RULE
        for ( final KomodoObject kobject : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.SNS_RULE ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, kobject.getAbsolutePath() );
            result.add( rule );
        }

        // Add NUMBER_RULE
        for ( final KomodoObject kobject : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.NUMBER_RULE ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, kobject.getAbsolutePath() );
            result.add( rule );
        }

        // Add PATTERN_RULE
        for ( final KomodoObject kobject : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.PATTERN_RULE ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, kobject.getAbsolutePath() );
            result.add( rule );
        }

        // Add RELATIONSHIP_RULE
        for ( final KomodoObject kobject : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.RELATIONSHIP_RULE ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, kobject.getAbsolutePath() );
            result.add( rule );
        }

        // Add REQUIRED_RULE
        for ( final KomodoObject kobject : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.REQUIRED_RULE ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, kobject.getAbsolutePath() );
            result.add( rule );
        }

        if ( result.isEmpty() ) {
            return Rule.NO_RULES;
        }

        return result.toArray( new Rule[ result.size() ] );
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.repository.ValidationManager#getRule(java.lang.String,org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Rule getRule( String ruleId, final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject defaultValidationArea = RuleFactory.getValidationDefaultAreaNode(transaction, this.repo);

        // Check SNS_RULE
        for ( final KomodoObject ruleObj : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.SNS_RULE ) ) {
            if(ruleObj.getName(transaction).equals(ruleId)) {
                return new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            }
        }

        // Check NUMBER_RULE
        for ( final KomodoObject ruleObj : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.NUMBER_RULE ) ) {
            if(ruleObj.getName(transaction).equals(ruleId)) {
                return new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            }
        }

        // Check PATTERN_RULE
        for ( final KomodoObject ruleObj : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.PATTERN_RULE ) ) {
            if(ruleObj.getName(transaction).equals(ruleId)) {
                return new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            }
        }

        // Check RELATIONSHIP_RULE
        for ( final KomodoObject ruleObj : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.RELATIONSHIP_RULE ) ) {
            if(ruleObj.getName(transaction).equals(ruleId)) {
                return new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            }
        }

        // Check REQUIRED_RULE
        for ( final KomodoObject ruleObj : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.REQUIRED_RULE ) ) {
            if(ruleObj.getName(transaction).equals(ruleId)) {
                return new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            }
        }

        return null;
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.repository.ValidationManager#ruleValid(java.lang.String, org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public boolean ruleValid(String ruleId,
                             UnitOfWork uow,
                             KomodoObject kObj) throws KException {
        
        Rule rule = getRule(ruleId,uow);
        return ruleValidForKObj(uow,rule,kObj);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.repository.ValidationManager#getRules(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public Rule[] getRules(UnitOfWork transaction, KomodoObject kObj) throws KException {
        ArgCheck.isNotNull( kObj, "kObj" ); //$NON-NLS-1$
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Rule > result = new ArrayList<>();
        KomodoObject defaultValidationArea = RuleFactory.getValidationDefaultAreaNode(transaction, this.repo);

        // Add SNS_RULE(s) for the nodeType
        for ( final KomodoObject ruleObj : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.SNS_RULE ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            if(ruleValidForKObj(transaction,rule,kObj)) {
                result.add( rule );
            }
        }

        // Add NUMBER_RULE(s) for the nodeType
        for ( final KomodoObject ruleObj : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.NUMBER_RULE ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            if(ruleValidForKObj(transaction,rule,kObj)) {
                result.add( rule );
            }
        }

        // Add PATTERN_RULE(s) for the nodeType
        for ( final KomodoObject ruleObj : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.PATTERN_RULE ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            if(ruleValidForKObj(transaction,rule,kObj)) {
                result.add( rule );
            }
        }

        // Add RELATIONSHIP_RULE(s) for the nodeType
        for ( final KomodoObject ruleObj : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.RELATIONSHIP_RULE ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            if(ruleValidForKObj(transaction,rule,kObj)) {
                result.add( rule );
            }
        }

        // Add REQUIRED_RULE(s) for the nodeType
        for ( final KomodoObject ruleObj : defaultValidationArea.getChildrenOfType( transaction,
                                                                                    KomodoLexicon.Rule.REQUIRED_RULE ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            if(ruleValidForKObj(transaction,rule,kObj)) {
                result.add( rule );
            }
        }

        if ( result.isEmpty() ) {
            return Rule.NO_RULES;
        }

        return result.toArray( new Rule[ result.size() ] );
    }

    private boolean ruleValidForKObj(final UnitOfWork transaction, final Rule rule, final KomodoObject kObj) throws KException {
        // Object type must be compatible with the rule
        if (!objectTypeMatches(rule.getNodeType(transaction),transaction,kObj)) return false;
        
        if (!objectPropsPassRestrictions(rule.getNodePropRestrictions(transaction), transaction, kObj)) return false;
        
        return true;
   }
    
    /* (non-Javadoc)
     * @see org.komodo.spi.repository.ValidationManager#setRuleEnabled(java.lang.String, boolean,org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void setRuleEnabled(String ruleId,
                               boolean isEnabled,
                               final UnitOfWork transaction ) throws KException {
        Rule theRule = getRule(ruleId,transaction);
        if(theRule!=null) theRule.setEnabled(transaction, isEnabled);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.repository.ValidationManager#setRuleSeverity(java.lang.String, java.lang.String, org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public void setRuleSeverity(String ruleId,
                                Outcome.Level severity,
                                final UnitOfWork transaction ) throws KException {
        Rule theRule = getRule(ruleId,transaction);
        if(theRule!=null) theRule.setSeverity(transaction, severity);
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.repository.ValidationManager#evaluate(org.komodo.spi.repository.KomodoObject, org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Result[] evaluate(KomodoObject kObject, final UnitOfWork transaction ) throws KException {
        // Evaluate against all rules that are valid for this object.
        Rule[] rules = getRules(transaction, kObject);
        
        final Result[] results = new Result[rules.length];
        int i = 0;
        for(Rule rule : rules) {
            Result result = rule.evaluate(transaction, kObject);
            results[i++] = result;
        }
        return results;
    }

    /* (non-Javadoc)
     * @see org.komodo.spi.repository.ValidationManager#evaluate(org.komodo.spi.repository.KomodoObject, org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Result[] evaluate(final KomodoObject kObject,
                             final UnitOfWork transaction,
                             final String... ruleIds) throws KException {


        final Result[] results = new Result[ruleIds.length];
        int i = 0;
        for(String ruleId : ruleIds) {
            Rule rule = getRule(ruleId,transaction);
            // Rule not found
            if(rule==null) throw new KException("The ruleId is not valid: "+ruleId);
            // Rule not valid for the kObject type
            if(!objectTypeMatches(rule.getNodeType(transaction), transaction, kObject)) {
                throw new KException("The ruleId is not valid for nodeType: "+kObject.getPrimaryType(transaction).getName());
            }
            // Rule not valid for the kObject type
            if(!objectPropsPassRestrictions(rule.getNodePropRestrictions(transaction), transaction, kObject)) {
                throw new KException("The object does not have the required properties ");
            }

            Result result = rule.evaluate(transaction, kObject);
            results[i++] = result;
        }
        return results;
    }

    private boolean objectTypeMatches (final String ruleNodeType, final UnitOfWork transaction, final KomodoObject kObj) throws KException {
        String primaryType = kObj.getPrimaryType(transaction).getName();
        if(primaryType.equals(ruleNodeType)) {
            return true;
        }
        
        Descriptor[] mixinTypes = kObj.getDescriptors(transaction);
        for(Descriptor mixinType : mixinTypes) {
            if(mixinType.getName().equals(ruleNodeType)) {
                return true;
            }
        }
        return false;
    }

    private boolean objectPropsPassRestrictions (final Map<String,String> propRestrictionMap, final UnitOfWork transaction, final KomodoObject kObj) throws KException {
        if(propRestrictionMap.isEmpty()) return true;
        
        String propKey = propRestrictionMap.keySet().iterator().next();
        String rqdValue = propRestrictionMap.get(propKey);
        
        // Check that object has the property
        if(!kObj.hasRawProperty(transaction, propKey)) {
            return false;
        }
        
        // Check that object has correct property value
        String objPropValue = kObj.getRawProperty(transaction, propKey).getStringValue(transaction);
        if(!rqdValue.equals(objPropValue)) {
            return false;
        }
        return true;
    }

}
