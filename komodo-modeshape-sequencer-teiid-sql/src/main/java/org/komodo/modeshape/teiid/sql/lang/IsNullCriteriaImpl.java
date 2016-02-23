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

import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.lang.IsNullCriteria;

/**
 *
 */
public class IsNullCriteriaImpl extends CriteriaImpl implements PredicateCriteria, IsNullCriteria<BaseExpression, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public IsNullCriteriaImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public BaseExpression getExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.IsNullCriteria.EXPRESSION_REF_NAME, BaseExpression.class);
    }

    @Override
    public void setExpression(BaseExpression expression) {
        setChild(TeiidSqlLexicon.IsNullCriteria.EXPRESSION_REF_NAME, expression);
    }

    @Override
    public boolean isNegated() {
        Object property = getProperty(TeiidSqlLexicon.IsNullCriteria.NEGATED_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    @Override
    public void setNegated(boolean value) {
        setProperty(TeiidSqlLexicon.IsNullCriteria.NEGATED_PROP_NAME, value);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getExpression() == null) ? 0 : this.getExpression().hashCode());
        result = prime * result + (this.isNegated() ? 1231 : 1237);
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
        IsNullCriteriaImpl other = (IsNullCriteriaImpl)obj;
        if (this.getExpression() == null) {
            if (other.getExpression() != null)
                return false;
        } else if (!this.getExpression().equals(other.getExpression()))
            return false;
        if (this.isNegated() != other.isNegated())
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public IsNullCriteriaImpl clone() {
        IsNullCriteriaImpl clone = new IsNullCriteriaImpl(this.getTeiidParser(), this.getId());

        if (getExpression() != null)
            clone.setExpression(getExpression().clone());
        clone.setNegated(isNegated());

        return clone;
    }

}
