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
package org.teiid.query.sql.lang;

import org.teiid.query.parser.TeiidClientParser;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.runtime.client.Messages;

/**
 *
 */
public abstract class AbstractCompareCriteria extends CriteriaImpl implements PredicateCriteria, CriteriaOperator {

    /** The left-hand expression. */
    private BaseExpression leftExpression;
    /**
     * The operator used in the clause.
     * @see #Operator.EQ
     * @see #Operator.NE
     * @see #Operator.LT
     * @see #Operator.GT
     * @see #Operator.LE
     * @see #Operator.GE
     */
    protected Operator operator = Operator.EQ;

    /**
     * @param p
     * @param id
     */
    public AbstractCompareCriteria(TeiidClientParser p, int id) {
        super(p, id);
    }

    /**
     * Returns the operator.
     * @return The operator
     */
    public int getOperator() {
        return this.operator.getIndex();
    }

    /**
     * @return string representation of operator
     */
    public String getOperatorAsString() {
        return this.operator.toString();
    }

    /**
     * Sets the operator.
     * @param operator
     */
    public void setOperator( Operator operator ) {
        if (operator.isLessThan(Operator.EQ) || operator.isGreaterThan(Operator.GE)) {
            throw new IllegalArgumentException(Messages.getString(Messages.ERR.ERR_015_010_0001, operator));
        }
        this.operator = operator;
    }

    /**
     * Get left expression.
     * @return Left expression
     */
    public BaseExpression getLeftExpression() {
        return this.leftExpression;
    }

    /**
     * Set left expression.
     * @param expression Left expression
     */
    public void setLeftExpression(BaseExpression expression) {
        this.leftExpression = expression;
    }

    /**
     * @return right expression
     */
    public abstract BaseExpression getRightExpression();

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.leftExpression == null) ? 0 : this.leftExpression.hashCode());
        result = prime * result + ((this.operator == null) ? 0 : this.operator.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        AbstractCompareCriteria other = (AbstractCompareCriteria)obj;
        if (this.leftExpression == null) {
            if (other.leftExpression != null) return false;
        } else if (!this.leftExpression.equals(other.leftExpression)) return false;
        if (this.operator != other.operator) return false;
        return true;
    }
}
