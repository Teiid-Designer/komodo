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
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;

/**
 * Validation Manager interface
 */
public interface ValidationManager {

    /**
     * @param rulesXmlFile
     *        the file whose rule definitions are being validated (cannot be <code>null</code>)
     * @return a list of errors (never <code>null</code> but can be empty)
     * @throws KException
     *         if an error occurs
     */
    List< String > validateRules( final File rulesXmlFile ) throws KException;

    /**
     * @param rulesXmlFile
     *        the file whose rule definitions are being imported (cannot be <code>null</code>)
     * @param uow the transaction
     * @param overwriteExisting 'true' will replace all existing rules.
     * @throws KException
     *         if an error occurs
     */
    void importRules( final File rulesXmlFile, final UnitOfWork uow, boolean overwriteExisting ) throws KException;
    
    /**
     * Gets all of the current rules
     * @param uow the transaction
     * @return all rules (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    Rule[] getAllRules( final UnitOfWork uow ) throws KException;

    /**
     * Gets the Rules that are valid for the supplied KomodoObject
     * @param uow the transaction
     * @param kObj the KomodoObject
     * @return applicable rules (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    Rule[] getRules( final UnitOfWork uow, final KomodoObject kObj ) throws KException;

    /**
     * Get the rule with the specified RuleId
     * @param ruleId the Rule ID
     * @param uow the transaction
     * @return the rule or <code>null</code> if not found
     * @throws KException if an error occurs
     */
    Rule getRule( final String ruleId, final UnitOfWork uow ) throws KException;

    /**
     * Determine if the specified RuleId is valid for the supplied KomodoObject
     * @param ruleId the Rule ID
     * @param uow the transaction
     * @param kObj the Komodo Object
     * @return <code>true</code> if the rule is applicable, <code>false</code> if not.
     * @throws KException if an error occurs
     */
    boolean ruleValid( final String ruleId, final UnitOfWork uow, final KomodoObject kObj ) throws KException;

    /**
     * Set the Rule enablement
     * @param ruleId the Rule ID
     * @param isEnabled <code>true</code> to enable Rule, <code>false</code> to disable.
     * @param uow the transaction
     * @throws KException if an error occurs
     */
    void setRuleEnabled(final String ruleId, final boolean isEnabled, final UnitOfWork uow ) throws KException; 

    /**
     * Set the Rule severity
     * @param ruleId the Rule ID
     * @param severity the rule severity
     * @param uow the transaction
     * @throws KException if an error occurs
     */
    void setRuleSeverity(final String ruleId, final Outcome.Level severity, final UnitOfWork uow ) throws KException; 
    
    /**
     * Evaluate the supplied object using all applicable rules for the object.
     * @param kObject
     * @param uow the transaction
     * @return the results from all rule validations (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    Result[] evaluate( final KomodoObject kObject, final UnitOfWork uow ) throws KException;

    /**
     * Evaluate the supplied object using the specified rule (if the rule is applicable for the object).
     * @param kObject the kObj
     * @param uow the transaction
     * @param ruleId the rule IDs
     * @return the results of the evaluation (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    Result[] evaluate( final KomodoObject kObject, final UnitOfWork uow, final String... ruleId ) throws KException;

}
