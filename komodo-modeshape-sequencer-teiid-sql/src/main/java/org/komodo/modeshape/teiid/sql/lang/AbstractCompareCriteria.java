/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.komodo.modeshape.teiid.sql.lang;

import org.komodo.modeshape.teiid.Messages;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.CriteriaOperator;
import org.komodo.spi.query.CriteriaOperator.Operator;

/**
 *
 */
public abstract class AbstractCompareCriteria extends CriteriaImpl implements PredicateCriteria {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public AbstractCompareCriteria(TeiidSeqParser p, int id) {
        super(p, id);
    }

    /**
     * Returns the operator.
     * @return The operator
     */
    public int getOperator() {
        Object property = getProperty(TeiidSqlLexicon.AbstractCompareCriteria.OPERATOR_PROP_NAME);
        if (property == null)
            return -1;

        Operator operator = CriteriaOperator.Operator.findOperator(property.toString());
        return operator.getIndex();
    }

    /**
     * @return string representation of operator
     */
    public String getOperatorAsString() {
        Object property = getProperty(TeiidSqlLexicon.AbstractCompareCriteria.OPERATOR_PROP_NAME);
        if (property == null)
            return null;

        Operator operator = CriteriaOperator.Operator.findOperator(property.toString());
        return operator.getSymbols().iterator().next();
    }

    /**
     * Sets the operator.
     * @param operator value
     */
    public void setOperator(CriteriaOperator.Operator operator ) {
        if (operator.isLessThan(CriteriaOperator.Operator.EQ) || operator.isGreaterThan(CriteriaOperator.Operator.GE)) {
            throw new IllegalArgumentException(Messages.getString(Messages.ERR.ERR_015_010_0001, operator));
        }

        setProperty(TeiidSqlLexicon.AbstractCompareCriteria.OPERATOR_PROP_NAME, operator.name());
    }

    /**
     * Get left expression.
     * @return Left expression
     */
    public BaseExpression getLeftExpression() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, BaseExpression.class);
    }

    /**
     * Set left expression.
     * @param expression Left expression
     */
    public void setLeftExpression(BaseExpression expression) {
        setChild(TeiidSqlLexicon.AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, expression);
    }

    /**
     * @return right expression
     */
    public abstract BaseExpression getRightExpression();

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getLeftExpression() == null) ? 0 : this.getLeftExpression().hashCode());
        result = prime * result + ((this.getOperatorAsString() == null) ? 0 : this.getOperatorAsString().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        AbstractCompareCriteria other = (AbstractCompareCriteria)obj;
        if (this.getLeftExpression() == null) {
            if (other.getLeftExpression() != null) return false;
        } else if (!this.getLeftExpression().equals(other.getLeftExpression())) return false;
        if (this.getOperator() != other.getOperator()) return false;
        return true;
    }
}
