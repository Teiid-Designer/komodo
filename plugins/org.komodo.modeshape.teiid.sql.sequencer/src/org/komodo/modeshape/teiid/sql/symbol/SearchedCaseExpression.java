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
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.lang.Criteria;
import org.komodo.spi.query.sql.symbol.ISearchedCaseExpression;
import org.komodo.spi.type.IDataTypeManagerService.DataTypeName;

public class SearchedCaseExpression extends ASTNode implements Expression, ISearchedCaseExpression<LanguageVisitor> {

    public SearchedCaseExpression(TeiidParser p, int id) {
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

    public List<? extends Criteria> getWhen() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.SearchedCaseExpression.WHEN_REF_NAME, Criteria.class);
    }

    public void setWhen(List<? extends Criteria> when) {
        setChildren(TeiidSqlLexicon.SearchedCaseExpression.WHEN_REF_NAME, when);
    }

    public List<? extends Expression> getThen() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.SearchedCaseExpression.THEN_REF_NAME, Expression.class);
    }

    public void setThen(List<? extends Expression> then) {
        setChildren(TeiidSqlLexicon.SearchedCaseExpression.THEN_REF_NAME, then);
    }

    public Expression getElseExpression() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.SearchedCaseExpression.ELSE_EXPRESSION_REF_NAME, Expression.class);
    }

    public void setElseExpression(Expression elseExpression) {
        setChild(TeiidSqlLexicon.SearchedCaseExpression.ELSE_EXPRESSION_REF_NAME, elseExpression);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getElseExpression() == null) ? 0 : this.getElseExpression().hashCode());
        result = prime * result + ((this.getThen() == null) ? 0 : this.getThen().hashCode());
        result = prime * result + ((this.getType() == null) ? 0 : this.getType().hashCode());
        result = prime * result + ((this.getWhen() == null) ? 0 : this.getWhen().hashCode());
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
        SearchedCaseExpression other = (SearchedCaseExpression)obj;
        if (this.getElseExpression() == null) {
            if (other.getElseExpression() != null)
                return false;
        } else if (!this.getElseExpression().equals(other.getElseExpression()))
            return false;
        if (this.getThen() == null) {
            if (other.getThen() != null)
                return false;
        } else if (!this.getThen().equals(other.getThen()))
            return false;
        if (this.getType() == null) {
            if (other.getType() != null)
                return false;
        } else if (!this.getType().equals(other.getType()))
            return false;
        if (this.getWhen() == null) {
            if (other.getWhen() != null)
                return false;
        } else if (!this.getWhen().equals(other.getWhen()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public SearchedCaseExpression clone() {
        SearchedCaseExpression clone = new SearchedCaseExpression(this.getTeiidParser(), this.getId());

        if (getWhen() != null)
            clone.setWhen(cloneList(getWhen()));
        if (getThen() != null)
            clone.setThen(cloneList(getThen()));
        if (getElseExpression() != null)
            clone.setElseExpression(getElseExpression().clone());
        if (getType() != null)
            clone.setType(getType());

        return clone;
    }

}
