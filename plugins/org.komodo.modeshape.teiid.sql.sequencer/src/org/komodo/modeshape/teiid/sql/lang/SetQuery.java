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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ISetQuery;

public class SetQuery extends QueryCommand
    implements ISetQuery<QueryCommand, OrderBy, Query, Expression, LanguageVisitor>{

    public SetQuery(ITeiidParser p, int id) {
        super(p, id);
        setType(TYPE_QUERY);
    }

    @Override
    public Query getProjectedQuery() {
        throw new UnsupportedOperationException();
    }

    @Override
    public List<Expression> getProjectedSymbols() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean isAll() {
        Object property = getProperty(TeiidSqlLexicon.SetQuery.ALL_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    @Override
    public void setAll(boolean value) {
        setProperty(TeiidSqlLexicon.SetQuery.ALL_PROP_NAME, value);
    }

    @Override
    public Operation getOperation() {
        Object property = getProperty(TeiidSqlLexicon.SetQuery.OPERATION_PROP_NAME);
        return property == null ? null : Operation.findOperation(property.toString());
    }

    public void setOperation(Operation operation) {
        setProperty(TeiidSqlLexicon.SetQuery.OPERATION_PROP_NAME, operation.name());
    }

    @Override
    public QueryCommand getLeftQuery() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.SetQuery.LEFT_QUERY_REF_NAME, QueryCommand.class);
    }

    @Override
    public void setLeftQuery(QueryCommand query) {
        setChild(TeiidSqlLexicon.SetQuery.LEFT_QUERY_REF_NAME, query);
    }

    @Override
    public QueryCommand getRightQuery() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.SetQuery.RIGHT_QUERY_REF_NAME, QueryCommand.class);
    }

    @Override
    public void setRightQuery(QueryCommand query) {
        setChild(TeiidSqlLexicon.SetQuery.RIGHT_QUERY_REF_NAME, query);
    }

    @Override
    public List<QueryCommand> getQueryCommands() {
        return Collections.unmodifiableList(Arrays.asList(getLeftQuery(), getRightQuery()));
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isAll() ? 1231 : 1237);
        result = prime * result + ((this.getLeftQuery() == null) ? 0 : this.getLeftQuery().hashCode());
        result = prime * result + ((this.getOperation() == null) ? 0 : this.getOperation().hashCode());
        result = prime * result + ((this.getRightQuery() == null) ? 0 : this.getRightQuery().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        SetQuery other = (SetQuery)obj;
        if (this.isAll() != other.isAll()) return false;
        if (this.getLeftQuery() == null) {
            if (other.getLeftQuery() != null) return false;
        } else if (!this.getLeftQuery().equals(other.getLeftQuery())) return false;
        if (this.getOperation() != other.getOperation()) return false;
        if (this.getRightQuery() == null) {
            if (other.getRightQuery() != null) return false;
        } else if (!this.getRightQuery().equals(other.getRightQuery())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public SetQuery clone() {
        SetQuery clone = new SetQuery(this.getTeiidParser(), this.getId());
    
        this.copyMetadataState(clone);
    
        if(getOperation() != null)
            clone.setOperation(getOperation());
        clone.setAll(isAll());
        if(getLeftQuery() != null)
            clone.setLeftQuery(getLeftQuery().clone());
        if(getRightQuery() != null)
            clone.setRightQuery(getRightQuery().clone());
        if(getOrderBy() != null)
            clone.setOrderBy(getOrderBy().clone());
        if(getLimit() != null)
            clone.setLimit(getLimit().clone());
        if(getWith() != null)
            clone.setWith(cloneList(getWith()));
        if(getSourceHint() != null)
            clone.setSourceHint(getSourceHint());
        if(getOption() != null)
            clone.setOption(getOption().clone());
    
        return clone;
    }

}
