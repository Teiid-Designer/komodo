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

package org.komodo.modeshape.teiid.sql.symbol;

import java.util.List;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.lang.IExpression;
import org.komodo.spi.query.sql.symbol.ICaseExpression;

public class CaseExpression extends ASTNode implements Expression, ICaseExpression<LanguageVisitor> {

    public CaseExpression(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public <T> Class<T> getType() {
        return null;
    }

    /**
     * Sets the type to which this expression has resolved.
     * @param type
     */
    public void setType(Class type) {

    }

    @Override
    public Expression getExpression() {
        return null;
    }

    /**
     * @param expression
     */
    public void setExpression(Expression expression) {
    }

    public List<Expression> getWhenExpressions() {
        return null;
    }

    @Override
    public IExpression getWhenExpression(int index) {
        return null;
    }

    @Override
    public int getWhenCount() {
        return 0;
    }

    /**
     * Sets the WHEN and THEN parts of this CASE expression.
     * Both lists should have the same number of Expressions.
     * @param when a non-null List of at least one Expression
     * @param then a non-null List of at least one Expression
     */
    public void setWhen(List<Expression> when, List<Expression> then) {
    }

    public List<Expression> getThenExpressions() {
        return null;
    }

    @Override
    public Expression getThenExpression(int index) {
        return null;
    }

    @Override
    public Expression getElseExpression() {
        return null;
    }

    /**
     * @param elseExpression
     */
    public void setElseExpression(Expression elseExpression) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getElseExpression() == null) ? 0 : this.getElseExpression().hashCode());
        result = prime * result + ((this.getExpression() == null) ? 0 : this.getExpression().hashCode());
        result = prime * result + ((this.getThenExpressions() == null) ? 0 : this.getThenExpressions().hashCode());
        result = prime * result + ((this.getType() == null) ? 0 : this.getType().hashCode());
        result = prime * result + ((this.getWhenExpressions() == null) ? 0 : this.getWhenExpressions().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        CaseExpression other = (CaseExpression)obj;
        if (this.getElseExpression() == null) {
            if (other.getElseExpression() != null)
                return false;
        } else if (!this.getElseExpression().equals(other.getElseExpression()))
            return false;
        if (this.getExpression() == null) {
            if (other.getExpression() != null)
                return false;
        } else if (!this.getExpression().equals(other.getExpression()))
            return false;
        if (this.getThenExpressions() == null) {
            if (other.getThenExpressions() != null)
                return false;
        } else if (!this.getThenExpressions().equals(other.getThenExpressions()))
            return false;
        if (this.getType() == null) {
            if (other.getType() != null)
                return false;
        } else if (!this.getType().equals(other.getType()))
            return false;
        if (this.getWhenExpressions() == null) {
            if (other.getWhenExpressions() != null)
                return false;
        } else if (!this.getWhenExpressions().equals(other.getWhenExpressions()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public CaseExpression clone() {
        CaseExpression clone = new CaseExpression(this.getTeiidParser(), this.getId());

        if (getExpression() != null)
            clone.setExpression(getExpression().clone());
        if (getWhenExpressions() != null)
            clone.setWhen(cloneList(getWhenExpressions()), cloneList(getThenExpressions()));
        if (getElseExpression() != null)
            clone.setElseExpression(getElseExpression().clone());
        if (getType() != null)
            clone.setType(getType());

        return clone;
    }

}
