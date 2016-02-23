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
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.lang.Query;

/**
 *
 */
public class QueryImpl extends QueryCommandImpl implements Query<SelectImpl, FromImpl, IntoImpl, CriteriaImpl, GroupByImpl, OrderByImpl, QueryImpl, BaseExpression, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public QueryImpl(TeiidSeqParser p, int id) {
        super(p, id);
        setType(TYPE_QUERY);
    }

    /**
     * @return row constructor flag
     */
    public boolean isRowConstructor() {
        Object property = getProperty(TeiidSqlLexicon.Query.ROW_CONSTRUCTOR_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    /**
     * @param rowConstructor
     */
    public void setRowConstructor(boolean rowConstructor) {
        setProperty(TeiidSqlLexicon.Query.ROW_CONSTRUCTOR_PROP_NAME, rowConstructor);
    }

    @Override
    public QueryImpl getProjectedQuery() {
        return this;
    }

    @Override
    public List<BaseExpression> getProjectedSymbols() {
        throw new UnsupportedOperationException();
    }

    @Override
    public SelectImpl getSelect() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.Query.SELECT_REF_NAME, SelectImpl.class);
    }

    @Override
    public void setSelect(SelectImpl select) {
        setChild(TeiidSqlLexicon.Query.SELECT_REF_NAME, select);
    }

    @Override
    public FromImpl getFrom() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.Query.FROM_REF_NAME, FromImpl.class);
    }

    @Override
    public void setFrom(FromImpl from) {
        setChild(TeiidSqlLexicon.Query.FROM_REF_NAME, from);
    }

    @Override
    public IntoImpl getInto() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.Query.INTO_REF_NAME, IntoImpl.class);
    }

    @Override
    public void setInto(IntoImpl into) {
        setChild(TeiidSqlLexicon.Query.INTO_REF_NAME, into);
    }

    @Override
    public CriteriaImpl getCriteria() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.Query.CRITERIA_REF_NAME, CriteriaImpl.class);
    }

    @Override
    public void setCriteria(CriteriaImpl where) {
        setChild(TeiidSqlLexicon.Query.CRITERIA_REF_NAME, where);
    }

    @Override
    public CriteriaImpl getHaving() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.Query.HAVING_REF_NAME, CriteriaImpl.class);
    }

    @Override
    public void setHaving(CriteriaImpl having) {
        setChild(TeiidSqlLexicon.Query.HAVING_REF_NAME, having);
    }

    @Override
    public GroupByImpl getGroupBy() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.Query.GROUP_BY_REF_NAME, GroupByImpl.class);
    }

    @Override
    public void setGroupBy(GroupByImpl groupBy) {
        setChild(TeiidSqlLexicon.Query.GROUP_BY_REF_NAME, groupBy);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCriteria() == null) ? 0 : this.getCriteria().hashCode());
        result = prime * result + ((this.getFrom() == null) ? 0 : this.getFrom().hashCode());
        result = prime * result + ((this.getGroupBy() == null) ? 0 : this.getGroupBy().hashCode());
        result = prime * result + ((this.getHaving() == null) ? 0 : this.getHaving().hashCode());
        result = prime * result + ((this.getInto() == null) ? 0 : this.getInto().hashCode());
        result = prime * result + ((this.getSelect() == null) ? 0 : this.getSelect().hashCode());
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
        QueryImpl other = (QueryImpl)obj;
        if (this.getCriteria() == null) {
            if (other.getCriteria() != null)
                return false;
        } else if (!this.getCriteria().equals(other.getCriteria()))
            return false;
        if (this.getFrom() == null) {
            if (other.getFrom() != null)
                return false;
        } else if (!this.getFrom().equals(other.getFrom()))
            return false;
        if (this.getGroupBy() == null) {
            if (other.getGroupBy() != null)
                return false;
        } else if (!this.getGroupBy().equals(other.getGroupBy()))
            return false;
        if (this.getHaving() == null) {
            if (other.getHaving() != null)
                return false;
        } else if (!this.getHaving().equals(other.getHaving()))
            return false;
        if (this.getInto() == null) {
            if (other.getInto() != null)
                return false;
        } else if (!this.getInto().equals(other.getInto()))
            return false;
        if (this.getSelect() == null) {
            if (other.getSelect() != null)
                return false;
        } else if (!this.getSelect().equals(other.getSelect()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public QueryImpl clone() {
        QueryImpl clone = new QueryImpl(this.getTeiidParser(), this.getId());

        if (getCriteria() != null)
            clone.setCriteria(getCriteria().clone());
        if (getSelect() != null)
            clone.setSelect(getSelect().clone());
        if (getFrom() != null)
            clone.setFrom(getFrom().clone());
        if (getGroupBy() != null)
            clone.setGroupBy(getGroupBy().clone());
        if (getHaving() != null)
            clone.setHaving(getHaving().clone());
        if (getInto() != null)
            clone.setInto(getInto().clone());
        if (getOrderBy() != null)
            clone.setOrderBy(getOrderBy().clone());
        if (getLimit() != null)
            clone.setLimit(getLimit().clone());
        if (getWith() != null)
            clone.setWith(cloneList(getWith()));
        if (getSourceHint() != null)
            clone.setSourceHint(getSourceHint().clone());
        if (getOption() != null)
            clone.setOption(getOption().clone());

        clone.setRowConstructor(isRowConstructor());

        copyMetadataState(clone);

        return clone;
    }

}
