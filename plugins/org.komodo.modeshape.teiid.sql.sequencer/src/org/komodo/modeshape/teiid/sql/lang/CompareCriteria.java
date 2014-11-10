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
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ICompareCriteria;

/**
 *
 */
public class CompareCriteria extends AbstractCompareCriteria implements ICompareCriteria<Expression, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public CompareCriteria(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public Expression getRightExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.CompareCriteria.RIGHT_EXPRESSION_REF_NAME, Expression.class);
    }

    @Override
    public void setRightExpression(Expression expression) {
        setChild(TeiidSqlLexicon.CompareCriteria.RIGHT_EXPRESSION_REF_NAME, expression);
    }

    /**
     * @return true if the compare criteria is used as join criteria, but not needed
     * during processing.
     */
    public Boolean isOptional() {
        Object property = getProperty(TeiidSqlLexicon.AbstractCompareCriteria.OPERATOR_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    /**
     * Set during planning to indicate that this criteria is no longer needed
     * to correctly process a join
     *
     * @param isOptional value
     */
    public void setOptional(Boolean isOptional) {
        setProperty(TeiidSqlLexicon.AbstractCompareCriteria.OPERATOR_PROP_NAME, isOptional);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.isOptional() == null) ? 0 : this.isOptional().hashCode());
        result = prime * result + ((this.getRightExpression() == null) ? 0 : this.getRightExpression().hashCode());
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
        CompareCriteria other = (CompareCriteria)obj;
        if (this.isOptional() == null) {
            if (other.isOptional() != null)
                return false;
        } else if (!this.isOptional().equals(other.isOptional()))
            return false;
        if (this.getRightExpression() == null) {
            if (other.getRightExpression() != null)
                return false;
        } else if (!this.getRightExpression().equals(other.getRightExpression()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public CompareCriteria clone() {
        CompareCriteria clone = new CompareCriteria(this.getTeiidParser(), this.getId());

        if (getRightExpression() != null)
            clone.setRightExpression(getRightExpression().clone());
        clone.setOptional(isOptional());
        if (getOperatorAsString() != null)
            clone.setOperator(CriteriaOperator.Operator.findOperator(getOperator()));

        if (getLeftExpression() != null)
            clone.setLeftExpression(getLeftExpression().clone());

        return clone;
    }

}
