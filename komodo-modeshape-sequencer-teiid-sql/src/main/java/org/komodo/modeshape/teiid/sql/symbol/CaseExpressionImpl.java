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
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.symbol.CaseExpression;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
public class CaseExpressionImpl extends ASTNode implements BaseExpression, CaseExpression<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public CaseExpressionImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public Class<?> getType() {
        return convertTypeClassPropertyToClass(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME);
    }

    public void setType(Class<?> type) {
        DataTypeName dataTypeName = getDataTypeService().retrieveDataTypeName(type);
        setProperty(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME, dataTypeName.name());
    }

    @Override
    public BaseExpression getExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.CaseExpression.EXPRESSION_REF_NAME, BaseExpression.class);
    }

    public void setExpression(BaseExpression expression) {
        setChild(TeiidSqlLexicon.CaseExpression.EXPRESSION_REF_NAME, expression);
    }

    public List<BaseExpression> getWhenExpressions() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.CaseExpression.WHEN_REF_NAME, BaseExpression.class);
    }

    @Override
    public BaseExpression getWhenExpression(int index) {
        List<BaseExpression> whenExpressions = getWhenExpressions();
        if (index < 0 || index >= whenExpressions.size())
            return null;

        return whenExpressions.get(index);
    }

    @Override
    public int getWhenCount() {
        return getWhenExpressions().size();
    }

    public void setWhen(List<BaseExpression> when) {
        setChildren(TeiidSqlLexicon.CaseExpression.WHEN_REF_NAME, when);
    }

    public List<BaseExpression> getThenExpressions() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.CaseExpression.THEN_REF_NAME, BaseExpression.class);
    }

    @Override
    public BaseExpression getThenExpression(int index) {
        List<BaseExpression> thenExpressions = getThenExpressions();
        if (index < 0 || index >= thenExpressions.size())
            return null;

        return thenExpressions.get(index);
    }

    public void setThen(List<BaseExpression> then) {
        setChildren(TeiidSqlLexicon.CaseExpression.THEN_REF_NAME, then);
    }

    @Override
    public BaseExpression getElseExpression() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.CaseExpression.ELSE_EXPRESSION_REF_NAME, BaseExpression.class);
    }

    public void setElseExpression(BaseExpression elseExpression) {
        setChild(TeiidSqlLexicon.CaseExpression.ELSE_EXPRESSION_REF_NAME, elseExpression);
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
        CaseExpressionImpl other = (CaseExpressionImpl)obj;
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
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public CaseExpressionImpl clone() {
        CaseExpressionImpl clone = new CaseExpressionImpl(this.getTeiidParser(), this.getId());

        if (getExpression() != null)
            clone.setExpression(getExpression().clone());
        if (getWhenExpressions() != null)
            clone.setWhen(cloneList(getWhenExpressions()));
        if (getThenExpressions() != null)
            clone.setThen(cloneList(getThenExpressions()));
        if (getElseExpression() != null)
            clone.setElseExpression(getElseExpression().clone());
        if (getType() != null)
            clone.setType(getType());

        return clone;
    }

}
