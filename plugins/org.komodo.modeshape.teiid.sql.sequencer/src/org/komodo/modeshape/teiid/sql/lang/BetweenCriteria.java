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

package org.komodo.modeshape.teiid.sql.lang;

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.IBetweenCriteria;


/**
 *
 */
public class BetweenCriteria extends Criteria implements PredicateCriteria, IBetweenCriteria<LanguageVisitor> {

    /**
     * @param p
     * @param id
     */
    public BetweenCriteria(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * @return the expression
     */
    @Override
    public Expression getExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.BetweenCriteria.EXPRESSION_REF_NAME, Expression.class);
    }

    /**
     * @param expression the expression to set
     */
    public void setExpression(Expression expression) {
        addLastChild(TeiidSqlLexicon.BetweenCriteria.EXPRESSION_REF_NAME, expression);
    }

    /**
     * @return the lower expression
     */
    @Override
    public Expression getLowerExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.BetweenCriteria.LOWER_EXPRESSION_REF_NAME, Expression.class);
    }

    /**
     * @param lowerExpression
     */
    public void setLowerExpression(Expression lowerExpression) {
        addLastChild(TeiidSqlLexicon.BetweenCriteria.LOWER_EXPRESSION_REF_NAME, lowerExpression);
    }

    /**
     * @return the upper expression
     */
    @Override
    public Expression getUpperExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.BetweenCriteria.UPPER_EXPRESSION_REF_NAME, Expression.class);
    }

    /**
     * @param upperExpression
     */
    public void setUpperExpression(Expression upperExpression) {
        addLastChild(TeiidSqlLexicon.BetweenCriteria.UPPER_EXPRESSION_REF_NAME, upperExpression);
    }

    /**
     * @return the negated
     */
    @Override
    public boolean isNegated() {
        Object property = getProperty(TeiidSqlLexicon.BetweenCriteria.NEGATED_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    /**
     * @param negated the negated to set
     */
    @Override
    public void setNegated(boolean negated) {
        setProperty(TeiidSqlLexicon.BetweenCriteria.NEGATED_PROP_NAME, negated);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getExpression() == null) ? 0 : this.getExpression().hashCode());
        result = prime * result + ((this.getLowerExpression() == null) ? 0 : this.getLowerExpression().hashCode());
        result = prime * result + (this.isNegated() ? 1231 : 1237);
        result = prime * result + ((this.getUpperExpression() == null) ? 0 : this.getUpperExpression().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        BetweenCriteria other = (BetweenCriteria)obj;
        if (this.getExpression() == null) {
            if (other.getExpression() != null) return false;
        } else if (!this.getExpression().equals(other.getExpression())) return false;
        if (this.getLowerExpression() == null) {
            if (other.getLowerExpression() != null) return false;
        } else if (!this.getLowerExpression().equals(other.getLowerExpression())) return false;
        if (this.isNegated() != other.isNegated()) return false;
        if (this.getUpperExpression() == null) {
            if (other.getUpperExpression() != null) return false;
        } else if (!this.getUpperExpression().equals(other.getUpperExpression())) return false;
        return true;
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public BetweenCriteria clone() {
        BetweenCriteria clone = new BetweenCriteria(this.getTeiidParser(), this.getId());

        if(getExpression() != null)
            clone.setExpression(getExpression().clone());
        if(getLowerExpression() != null)
            clone.setLowerExpression(getLowerExpression().clone());
        if(getUpperExpression() != null)
            clone.setUpperExpression(getUpperExpression().clone());
        clone.setNegated(isNegated());

        return clone;
    }

}
