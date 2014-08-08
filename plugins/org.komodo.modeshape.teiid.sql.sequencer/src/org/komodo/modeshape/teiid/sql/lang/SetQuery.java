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

import java.util.List;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ISetQuery;

public class SetQuery extends QueryCommand
    implements ISetQuery<QueryCommand, OrderBy, Query, Expression, LanguageVisitor>{

    public SetQuery(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * Return type of command.
     * @return TYPE_QUERY
     */
    @Override
    public int getType() {
        return TYPE_QUERY;
    }

    @Override
    public Query getProjectedQuery() {
        return null;
    }

    @Override
    public List<Expression> getProjectedSymbols() {
        return null;
    }

    @Override
    public boolean isAll() {
        return false;
    }

    @Override
    public void setAll(boolean value) {
    }

    @Override
    public org.komodo.spi.query.sql.lang.ISetQuery.Operation getOperation() {
        return null;
    }

    /**
     * @param operation
     */
    public void setOperation(Operation operation) {
    }

    @Override
    public QueryCommand getLeftQuery() {
        return null;
    }

    @Override
    public void setLeftQuery(QueryCommand query) {
    }

    @Override
    public QueryCommand getRightQuery() {
        return null;
    }

    @Override
    public void setRightQuery(QueryCommand query) {
    }

    @Override
    public List<QueryCommand> getQueryCommands() {
        return null;
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
