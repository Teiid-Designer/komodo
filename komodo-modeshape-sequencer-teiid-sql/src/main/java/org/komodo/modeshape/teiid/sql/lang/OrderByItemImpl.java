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

import org.komodo.modeshape.teiid.language.SortSpecification;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.lang.OrderByItem;

/**
 *
 */
public class OrderByItemImpl extends ASTNode implements OrderByItem<BaseExpression, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public OrderByItemImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public BaseExpression getSymbol() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.OrderByItem.SYMBOL_REF_NAME, BaseExpression.class);
    }

    @Override
    public void setSymbol(BaseExpression symbol) {
        setChild(TeiidSqlLexicon.OrderByItem.SYMBOL_REF_NAME, symbol);
    }

    @Override
    public boolean isAscending() {
        Object property = getProperty(TeiidSqlLexicon.OrderByItem.ASCENDING_PROP_NAME);
        return property == null ? true : Boolean.parseBoolean(property.toString());
    }

    public void setAscending(boolean ascending) {
        setProperty(TeiidSqlLexicon.OrderByItem.ASCENDING_PROP_NAME, ascending);
    }

    public SortSpecification.NullOrdering getNullOrdering() {
        Object property = getProperty(TeiidSqlLexicon.OrderByItem.NULL_ORDERING_PROP_NAME);
        return property == null ? null : SortSpecification.NullOrdering.findNullOrdering(property.toString());
    }

    public void setNullOrdering(SortSpecification.NullOrdering nullOrdering) {
        setProperty(TeiidSqlLexicon.OrderByItem.NULL_ORDERING_PROP_NAME, nullOrdering.name());
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isAscending() ? 1231 : 1237);
        result = prime * result + ((this.getNullOrdering() == null) ? 0 : this.getNullOrdering().hashCode());
        result = prime * result + ((this.getSymbol() == null) ? 0 : this.getSymbol().hashCode());
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
        OrderByItemImpl other = (OrderByItemImpl)obj;
        if (this.isAscending() != other.isAscending())
            return false;
        if (this.getNullOrdering() != other.getNullOrdering())
            return false;
        if (this.getSymbol() == null) {
            if (other.getSymbol() != null)
                return false;
        } else if (!this.getSymbol().equals(other.getSymbol()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public OrderByItemImpl clone() {
        OrderByItemImpl clone = new OrderByItemImpl(this.getTeiidParser(), this.getId());

        if (getSymbol() != null)
            clone.setSymbol(getSymbol().clone());
        if (getNullOrdering() != null)
            clone.setNullOrdering(getNullOrdering());
        clone.setAscending(isAscending());

        return clone;
    }

}
