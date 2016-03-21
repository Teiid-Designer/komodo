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
     * @param uow the transaction
     * @param rulesXmlFile
     *        the file whose rule definitions are being imported (cannot be <code>null</code>)
     * @param overwriteExisting 'true' will replace all existing rules.
     * @throws KException
     *         if an error occurs
     */
    void importRules( final UnitOfWork uow, final File rulesXmlFile, boolean overwriteExisting ) throws KException;
    
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
     * @param uow the transaction
     * @param ruleId the Rule ID
     * @return the rule or <code>null</code> if not found
     * @throws KException if an error occurs
     */
    Rule getRule( final UnitOfWork uow, final String ruleId ) throws KException;

    /**
     * Determine if the specified RuleId is applicable for the supplied KomodoObject
     * @param uow the transaction
     * @param ruleId the Rule ID
     * @param kObj the Komodo Object
     * @return <code>true</code> if the rule is applicable, <code>false</code> if not.
     * @throws KException if an error occurs
     */
    boolean isApplicable( final UnitOfWork uow, final String ruleId, final KomodoObject kObj ) throws KException;

    /**
     * Set the Rule enablement
     * @param uow the transaction
     * @param isEnabled <code>true</code> to enable Rule, <code>false</code> to disable.
     * @param ruleId the Rule IDs
     * @throws KException if an error occurs
     */
    void setRuleEnabled(final UnitOfWork uow, final boolean isEnabled, final String... ruleId ) throws KException; 

    /**
     * Set the Rule severity
     * @param uow the transaction
     * @param severity the rule severity
     * @param ruleId the Rule IDs
     * @throws KException if an error occurs
     */
    void setRuleSeverity(final UnitOfWork uow, final Outcome.Level severity, final String... ruleId ) throws KException; 
    
    /**
     * Evaluate the supplied object using all applicable rules for the object.
     * @param uow the transaction
     * @param kObject the object to validate
     * @param full <code>true</code> will validate the node and all of its ancestors.  <code>false</code> only validates the supplied node.
     * @return the results from all rule validations (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    Result[] evaluate( final UnitOfWork uow, final KomodoObject kObject, boolean full  ) throws KException;

    /**
     * Evaluate the supplied object using the specified rules (if the rule is applicable for the object).
     * @param uow the transaction
     * @param kObject the kObj
     * @param ruleId the rule IDs
     * @return the results of the evaluation (never <code>null</code> but can be empty)
     * @throws KException if an error occurs
     */
    Result[] evaluate( final UnitOfWork uow, final KomodoObject kObject, final String... ruleId ) throws KException;

}
