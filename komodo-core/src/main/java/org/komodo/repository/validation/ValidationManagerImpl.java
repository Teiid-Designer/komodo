/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.repository.validation;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import org.komodo.spi.KException;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.repository.ValidationManager;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.KLog;

/**
 * An implementation of an {@link ValidationManager} that can be used by repositories.
 */
public class ValidationManagerImpl implements ValidationManager {

    private static final KLog LOGGER = KLog.getLogger();
    
    private final Repository repo;
    private final String RULES_SCHEMA_FILE = "komodoValidation.xsd"; //$NON-NLS-1$
    private File rulesSchemaFile;
    private boolean defaultRulesExist = false;

    /**
     * Constructs an environment store delegate.
     *
     * @param repo
     *        the repository this is a delegate for (cannot be <code>null</code>)
     */
    public ValidationManagerImpl( final Repository repo ) {
        ArgCheck.isNotNull( repo, "repo" ); //$NON-NLS-1$
        
        this.repo = repo;
        initRulesXsd();
    }
    
    private void initRulesXsd() {
        InputStream schemaStream = getClass().getClassLoader().getResourceAsStream(RULES_SCHEMA_FILE);
        try {
            rulesSchemaFile = File.createTempFile("rulesSchemaFile", ".xsd"); //$NON-NLS-1$  //$NON-NLS-2$
            Files.copy(schemaStream, rulesSchemaFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException ex) {
            LOGGER.error("ValidationManagerImpl - error processing Validation Rules schema : ", ex); //$NON-NLS-1$
        }
        rulesSchemaFile.deleteOnExit();
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

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.ValidationManager#importRules(org.komodo.spi.repository.Repository.UnitOfWork, java.io.File, boolean)
     */
    @Override
    public void importRules(final UnitOfWork uow, final File rulesXmlFile, boolean overwriteExisting) throws KException {
        ArgCheck.isNotNull( rulesXmlFile, "rulesXmlFile" ); //$NON-NLS-1$
        
        // If rules exist, no need to reload - unless overwriting
        if(defaultRulesExist && !overwriteExisting) return;

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
    
    protected void clearValidationRules(final UnitOfWork uow) throws Exception {
        KomodoObject defaultValidationArea = RuleFactory.getValidationDefaultAreaNode(uow, this.repo);
        KomodoObject[] rules = defaultValidationArea.getChildren(uow);
        for(KomodoObject rule : rules) {
            rule.remove(uow);
        }
    }
    
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.ValidationManager#getAllRules(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Rule[] getAllRules( final UnitOfWork transaction ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Rule > result = new ArrayList<>();
        KomodoObject defaultValidationArea = RuleFactory.getValidationDefaultAreaNode(transaction, this.repo);

        // Collect all available Rules
        for ( final KomodoObject kobject : defaultValidationArea.getChildren( transaction ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, kobject.getAbsolutePath() );
            if(rule.isEnabled(transaction)) {
                result.add( rule );
            } else {
                result.add( new ProblemRule(rule.getName(transaction),ProblemRule.Type.NOT_ENABLED) );
            }
        }

        if ( result.isEmpty() ) {
            return Rule.NO_RULES;
        }

        return result.toArray( new Rule[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.ValidationManager#getRule(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public Rule getRule( final UnitOfWork transaction, String ruleId ) throws KException {
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        KomodoObject defaultValidationArea = RuleFactory.getValidationDefaultAreaNode(transaction, this.repo);

        // Check all available Rules for a match.
        if(defaultValidationArea.hasChild(transaction, ruleId)) {
            KomodoObject ruleObj = defaultValidationArea.getChild(transaction, ruleId);
            if(ruleObj!=null) {
                return new RuleImpl( transaction, this.repo, ruleObj.getAbsolutePath() );
            }
        }

        return new ProblemRule(ruleId, ProblemRule.Type.NOT_FOUND);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.ValidationManager#isApplicable(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String, org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public boolean isApplicable(UnitOfWork uow,
                                String ruleId,
                                KomodoObject kObj) throws KException {

        return getRule(uow,ruleId).isApplicable(uow, kObj);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.ValidationManager#getRules(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public Rule[] getRules(UnitOfWork transaction, KomodoObject kObj) throws KException {
        ArgCheck.isNotNull( kObj, "kObj" ); //$NON-NLS-1$
        ArgCheck.isNotNull( transaction, "transaction" ); //$NON-NLS-1$
        ArgCheck.isTrue( ( transaction.getState() == State.NOT_STARTED ), "transaction state is not NOT_STARTED" ); //$NON-NLS-1$

        final List< Rule > result = new ArrayList<>();
        KomodoObject defaultValidationArea = RuleFactory.getValidationDefaultAreaNode(transaction, this.repo);

        // Check all available Rules - determine if they are applicable for the supplied object.
        for ( final KomodoObject kobject : defaultValidationArea.getChildren( transaction ) ) {
            final Rule rule = new RuleImpl( transaction, this.repo, kobject.getAbsolutePath() );
            if(rule.isApplicable(transaction, kObj)) {
                if(rule.isEnabled(transaction)) {
                    result.add( rule );
                } else {
                    result.add(new ProblemRule(rule.getName(transaction),ProblemRule.Type.NOT_ENABLED));
                }
            }
        }
        
        if ( result.isEmpty() ) {
            return Rule.NO_RULES;
        }

        return result.toArray( new Rule[ result.size() ] );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.ValidationManager#setRuleEnabled(org.komodo.spi.repository.Repository.UnitOfWork, boolean, String...)
     */
    @Override
    public void setRuleEnabled(final UnitOfWork transaction,
                               boolean isEnabled,
                               String... ruleIds ) throws KException {
        for(String ruleId : ruleIds) {
            getRule(transaction,ruleId).setEnabled(transaction, isEnabled);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.ValidationManager#setRuleSeverity(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.outcome.Outcome.Level, String...)
     */
    @Override
    public void setRuleSeverity(final UnitOfWork transaction,
                                Outcome.Level severity,
                                String... ruleIds ) throws KException {
        for(String ruleId : ruleIds) {
            getRule(transaction,ruleId).setSeverity(transaction, severity);
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.ValidationManager#evaluate(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.repository.KomodoObject, boolean)
     */
    @Override
    public Result[] evaluate(final UnitOfWork transaction, KomodoObject kObject, boolean full ) throws KException {
        List<Result> allResults = new ArrayList<Result>();
        
        // Evaluate against all rules that are valid for this object.
        Rule[] rules = getRules(transaction, kObject);
        
        for(Rule rule : rules) {
            Result result = rule.evaluate(transaction, kObject);
            allResults.add(result);
        }
        
        // If doing full validation, also validate all children recursively
        if(full) { 
            KomodoObject[] kids = kObject.getChildren(transaction);
            for(KomodoObject kid : kids) {
                Result[] kidResults = evaluate(transaction, kid, full);
                for(Result kidResult : kidResults) {
                    allResults.add(kidResult);
                }
                
            }
            
        }
        return allResults.toArray(new Result[ allResults.size() ]);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.ValidationManager#evaluate(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.repository.KomodoObject, java.lang.String...)
     */
    @Override
    public Result[] evaluate(final UnitOfWork transaction,
                             final KomodoObject kObject,
                             final String... ruleIds) throws KException {

        final Result[] results = new Result[ruleIds.length];
        int i = 0;
        for(String ruleId : ruleIds) {
            Rule rule = getRule(transaction,ruleId);
            Result result = rule.evaluate(transaction, kObject);
            results[i++] = result;
        }
        return results;
    }
    
}
