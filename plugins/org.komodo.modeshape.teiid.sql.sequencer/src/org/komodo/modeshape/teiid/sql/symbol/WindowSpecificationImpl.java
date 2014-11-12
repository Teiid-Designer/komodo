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
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.lang.OrderByImpl;
import org.komodo.spi.query.sql.symbol.WindowSpecification;

/**
 *
 */
public class WindowSpecificationImpl extends ASTNode implements WindowSpecification<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public WindowSpecificationImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    public List<BaseExpression> getPartition() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.WindowSpecification.PARTITION_REF_NAME, BaseExpression.class);
    }

    public void setPartition(List<BaseExpression> partitionList) {
        setChildren(TeiidSqlLexicon.WindowSpecification.PARTITION_REF_NAME, partitionList);
    }

    public OrderByImpl getOrderBy() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.WindowSpecification.ORDER_BY_REF_NAME, OrderByImpl.class);
    }

    public void setOrderBy(OrderByImpl orderBy) {
        setChild(TeiidSqlLexicon.WindowSpecification.ORDER_BY_REF_NAME, orderBy);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getOrderBy() == null) ? 0 : this.getOrderBy().hashCode());
        result = prime * result + ((this.getPartition() == null) ? 0 : this.getPartition().hashCode());
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
        WindowSpecificationImpl other = (WindowSpecificationImpl)obj;
        if (this.getOrderBy() == null) {
            if (other.getOrderBy() != null)
                return false;
        } else if (!this.getOrderBy().equals(other.getOrderBy()))
            return false;
        if (this.getPartition() == null) {
            if (other.getPartition() != null)
                return false;
        } else if (!this.getPartition().equals(other.getPartition()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public WindowSpecificationImpl clone() {
        WindowSpecificationImpl clone = new WindowSpecificationImpl(this.getTeiidParser(), this.getId());

        if (getOrderBy() != null)
            clone.setOrderBy(getOrderBy().clone());
        if (getPartition() != null)
            clone.setPartition(cloneList(getPartition()));

        return clone;
    }

}
