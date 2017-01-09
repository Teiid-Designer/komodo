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
import org.komodo.core.Messages;
import org.komodo.spi.KException;
import org.komodo.spi.outcome.Outcome;
import org.komodo.spi.outcome.Outcome.Level;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.validation.Result;
import org.komodo.spi.repository.validation.Rule;

/**
 *  Rule and Results for Problem conditions
 *  1) Rule not found
 *  2) Rule not enabled
 *  3) Rule not applicable
 */
public class ProblemRule implements Rule {

    /**
     * ProblemRule types
     */
    public enum Type {
        /**
         * Rule not found
         */
        NOT_FOUND,

        /**
         * Rule not enabled
         */
        NOT_ENABLED,

        /**
         * Rule not applicable
         */
        NOT_APPLICABLE
    }

    private String ruleId;
    private Type type;


    /**
     * Constructor
     * @param ruleId the Rule id.
     * @param type the problem rule type.
     */
    public ProblemRule(String ruleId, Type type) {
        this.ruleId = ruleId;
        this.type = type;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#evaluate(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public Result evaluate(UnitOfWork transaction,
                           KomodoObject kobject) throws KException {

        return new ProblemRuleResult(transaction, kobject, this.ruleId, this.type);
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getDescription(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getDescription(UnitOfWork transaction) {
        String description = null;
        if(this.type==ProblemRule.Type.NOT_FOUND) {
            description = Messages.getString(Messages.ProblemRule.Rule_Not_Found_Rule_Description, this.ruleId);
        } else if(this.type==ProblemRule.Type.NOT_APPLICABLE) {
            description = Messages.getString(Messages.ProblemRule.Rule_Not_Applicable_Rule_Description, this.ruleId);
        } else if(this.type==ProblemRule.Type.NOT_ENABLED) {
            description = Messages.getString(Messages.ProblemRule.Rule_Not_Enabled_Rule_Description, this.ruleId);
        }
        return description;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getJcrName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getJcrName(UnitOfWork transaction) {
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getMessage(org.komodo.spi.repository.Repository.UnitOfWork, java.lang.String)
     */
    @Override
    public String getMessage(UnitOfWork transaction,
                             String key) {
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getName(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getName(UnitOfWork transaction) {
        String name = null;
        if(this.type==ProblemRule.Type.NOT_FOUND) {
            name = Messages.getString(Messages.ProblemRule.Rule_Not_Found_Rule_Id);
        } else if(this.type==ProblemRule.Type.NOT_APPLICABLE) {
            name = Messages.getString(Messages.ProblemRule.Rule_Not_Applicable_Rule_Id);
        } else if(this.type==ProblemRule.Type.NOT_ENABLED) {
            name = Messages.getString(Messages.ProblemRule.Rule_Not_Enabled_Rule_Id);
        }
        return name;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getNodeType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public String getNodeType(UnitOfWork transaction) {
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getPropRestrictions(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.repository.validation.Rule.PropertyRestriction)
     */
    @Override
    public Map<String, String> getPropRestrictions(UnitOfWork transaction,
                                                   PropertyRestriction restrictionType) {
        return Collections.emptyMap();
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getRuleType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public RuleType getRuleType(UnitOfWork transaction) {
        return RuleType.RELATIONSHIP;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getSeverity(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public Level getSeverity(UnitOfWork transaction) {
        return Level.INFO;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#getValidationType(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public ValidationType getValidationType(UnitOfWork transaction) {
        return ValidationType.NODE;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#isRequired(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isRequired(UnitOfWork transaction) {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#isApplicable(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public boolean isApplicable(UnitOfWork transaction,
                                KomodoObject kObject) {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#setRequired(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setRequired(UnitOfWork transaction,
                            boolean newRequired) {
        // Nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#isEnabled(org.komodo.spi.repository.Repository.UnitOfWork)
     */
    @Override
    public boolean isEnabled(UnitOfWork transaction) {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#setEnabled(org.komodo.spi.repository.Repository.UnitOfWork, boolean)
     */
    @Override
    public void setEnabled(UnitOfWork transaction,
                           boolean newEnabled) {
        // Nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.spi.repository.validation.Rule#setSeverity(org.komodo.spi.repository.Repository.UnitOfWork, org.komodo.spi.outcome.Outcome.Level)
     */
    @Override
    public void setSeverity(UnitOfWork transaction,
                            Level newLevel) {
        // Nothing to do
    }

    /*
     * Result for ProblemRule
     */
    class ProblemRuleResult implements Result {
        private String objName;
        private String path;
        private String ruleId;
        private ProblemRule.Type ruleType;

        /**
         * Constructor
         * @param transaction the transaction
         * @param kobject the KomodoObject
         * @param ruleId the Rule Id
         * @param problemRuleType the Rule type
         * @throws KException the exception
         */
        public ProblemRuleResult(UnitOfWork transaction,
                                 KomodoObject kobject,
                                 String ruleId,
                                 ProblemRule.Type problemRuleType) throws KException {
            this.objName = kobject.getName(transaction);
            this.path = kobject.getAbsolutePath();
            this.ruleId = ruleId;
            this.ruleType = problemRuleType;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.outcome.Outcome#getOutcomes()
         */
        @Override
        public List<Outcome> getOutcomes() {
            return Collections.emptyList();
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.outcome.Outcome#addOutcome(org.komodo.spi.outcome.Outcome)
         */
        @Override
        public void addOutcome(Outcome outcome) {
            // Nothing to do
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.outcome.Outcome#addOutcomes(java.util.List)
         */
        @Override
        public void addOutcomes(List<Outcome> outcomes) {
            // Nothing to do
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.outcome.Outcome#getMessage()
         */
        @Override
        public String getMessage() {
            String message = null;
            if(this.ruleType==ProblemRule.Type.NOT_FOUND) {
                message = Messages.getString(Messages.ProblemRule.Rule_Not_Found_Rule_Result_Message, this.ruleId);
            } else if(this.ruleType==ProblemRule.Type.NOT_APPLICABLE) {
                message = Messages.getString(Messages.ProblemRule.Rule_Not_Applicable_Rule_Result_Message, this.ruleId, this.objName);
            } else if(this.ruleType==ProblemRule.Type.NOT_ENABLED) {
                message = Messages.getString(Messages.ProblemRule.Rule_Not_Enabled_Rule_Result_Message, this.ruleId);
            }
            return message;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.outcome.Outcome#getException()
         */
        @Override
        public Exception getException() {
            return null;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.outcome.Outcome#getLevel()
         */
        @Override
        public Level getLevel() {
            return Outcome.Level.INFO;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.outcome.Outcome#isOK()
         */
        @Override
        public boolean isOK() {
            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.spi.outcome.Outcome#isMultiOutcome()
         */
        @Override
        public boolean isMultiOutcome() {
            return false;
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
            return 0;
        }

    }

}
