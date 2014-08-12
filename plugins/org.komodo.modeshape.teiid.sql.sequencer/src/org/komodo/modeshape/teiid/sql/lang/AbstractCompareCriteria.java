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
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;

/**
 *
 */
public abstract class AbstractCompareCriteria extends Criteria implements PredicateCriteria {

    /**
     * @param p
     * @param id
     */
    public AbstractCompareCriteria(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * Returns the operator.
     * @return The operator
     */
    public int getOperator() {
        return -1;
    }

    /**
     * @return string representation of operator
     */
    public String getOperatorAsString() {
        return null;
    }

    /**
     * Sets the operator.
     * @param operator
     */
    public void setOperator(CriteriaOperator.Operator operator ) {
        if (operator.isLessThan(CriteriaOperator.Operator.EQ) || operator.isGreaterThan(CriteriaOperator.Operator.GE)) {
            throw new IllegalArgumentException(Messages.getString(Messages.ERR.ERR_015_010_0001, operator));
        }
    }

    /**
     * Get left expression.
     * @return Left expression
     */
    public Expression getLeftExpression() {
        return this.getLeftExpression();
    }

    /**
     * Set left expression.
     * @param expression Left expression
     */
    public void setLeftExpression(Expression expression) {
    }

    /**
     * @return right expression
     */
    public abstract Expression getRightExpression();

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
