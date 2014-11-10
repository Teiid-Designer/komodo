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
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.IMatchCriteria;

/**
 *
 */
public class MatchCriteria extends Criteria
    implements PredicateCriteria, IMatchCriteria<Expression, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public MatchCriteria(TeiidSeqParser p, int id) {
        super(p, id);
        setMode(MatchMode.LIKE);
        setEscapeChar(NULL_ESCAPE_CHAR);
    }

    @Override
    public Expression getLeftExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.MatchCriteria.LEFT_EXPRESSION_REF_NAME, Expression.class);
    }

    @Override
    public void setLeftExpression(Expression expression) {
        setChild(TeiidSqlLexicon.MatchCriteria.LEFT_EXPRESSION_REF_NAME, expression);
    }

    @Override
    public Expression getRightExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.MatchCriteria.RIGHT_EXPRESSION_REF_NAME, Expression.class);
    }

    @Override
    public void setRightExpression(Expression expression) {
        setChild(TeiidSqlLexicon.MatchCriteria.RIGHT_EXPRESSION_REF_NAME, expression);
    }

    @Override
    public char getEscapeChar() {
        Object property = getProperty(TeiidSqlLexicon.MatchCriteria.ESCAPE_CHAR_PROP_NAME);
        return property == null ? NULL_ESCAPE_CHAR : property.toString().charAt(0);
    }

    @Override
    public void setEscapeChar(char escapeChar) {
        setProperty(TeiidSqlLexicon.MatchCriteria.ESCAPE_CHAR_PROP_NAME, escapeChar);
    }

    @Override
    public boolean isNegated() {
        Object property = getProperty(TeiidSqlLexicon.MatchCriteria.NEGATED_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    @Override
    public void setNegated(boolean value) {
        setProperty(TeiidSqlLexicon.MatchCriteria.NEGATED_PROP_NAME, value);
    }

    @Override
    public MatchMode getMode() {
        Object property = getProperty(TeiidSqlLexicon.MatchCriteria.MODE_PROP_NAME);
        return property == null ? MatchMode.LIKE : MatchMode.findMatchMode(property.toString());
    }

    /**
     * @param matchMode
     */
    public void setMode(MatchMode matchMode) {
        setProperty(TeiidSqlLexicon.MatchCriteria.MODE_PROP_NAME, matchMode.name());
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + this.getEscapeChar();
        result = prime * result + ((this.getLeftExpression() == null) ? 0 : this.getLeftExpression().hashCode());
        result = prime * result + ((this.getMode() == null) ? 0 : this.getMode().hashCode());
        result = prime * result + (this.isNegated() ? 1231 : 1237);
        result = prime * result + ((this.getRightExpression() == null) ? 0 : this.getRightExpression().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        MatchCriteria other = (MatchCriteria)obj;
        if (this.getEscapeChar() != other.getEscapeChar()) return false;
        if (this.getLeftExpression() == null) {
            if (other.getLeftExpression() != null) return false;
        } else if (!this.getLeftExpression().equals(other.getLeftExpression())) return false;
        if (this.getMode() != other.getMode()) return false;
        if (this.isNegated() != other.isNegated()) return false;
        if (this.getRightExpression() == null) {
            if (other.getRightExpression() != null) return false;
        } else if (!this.getRightExpression().equals(other.getRightExpression())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public MatchCriteria clone() {
        MatchCriteria clone = new MatchCriteria(this.getTeiidParser(), this.getId());
    
        if(getRightExpression() != null)
            clone.setRightExpression(getRightExpression().clone());
        if(getLeftExpression() != null)
            clone.setLeftExpression(getLeftExpression().clone());
        if(getMode() != null)
            clone.setMode(getMode());
        clone.setNegated(isNegated());
        clone.setEscapeChar(getEscapeChar());
    
        return clone;
    }

}
