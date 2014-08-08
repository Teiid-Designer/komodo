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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.symbol.IXMLQuery;

public class XMLQuery extends ASTNode implements Expression, IXMLQuery<LanguageVisitor> {

    public XMLQuery(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public <T> Class<T> getType() {
        return null;
    }

    /**
     * @return
     */
    public String getXquery() {
        return null;
    }

    /**
     * @param xquery
     */
    public void setXquery(String xquery) {
    }

    /**
     * @return
     */
    public XMLNamespaces getNamespaces() {
        return null;
    }

    /**
     * @param xmlNamespaces
     */
    public void setNamespaces(XMLNamespaces xmlNamespaces) {
    }

    /**
     * @return
     */
    public List<DerivedColumn> getPassing() {
        return null;
    }

    /**
     * @param passingValues
     */
    public void setPassing(List<DerivedColumn> passingValues) {
    }

    /**
     * @return
     */
    public Boolean isEmptyOnEmpty() {
        return null;
    }

    /**
     * @param empty
     */
    public void setEmptyOnEmpty(Boolean empty) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.isEmptyOnEmpty() == null) ? 0 : this.isEmptyOnEmpty().hashCode());
        result = prime * result + ((this.getNamespaces() == null) ? 0 : this.getNamespaces().hashCode());
        result = prime * result + ((this.getPassing() == null) ? 0 : this.getPassing().hashCode());
        result = prime * result + ((this.getXquery() == null) ? 0 : this.getXquery().hashCode());
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
        XMLQuery other = (XMLQuery)obj;
        if (this.isEmptyOnEmpty() == null) {
            if (other.isEmptyOnEmpty() != null)
                return false;
        } else if (!this.isEmptyOnEmpty().equals(other.isEmptyOnEmpty()))
            return false;
        if (this.getNamespaces() == null) {
            if (other.getNamespaces() != null)
                return false;
        } else if (!this.getNamespaces().equals(other.getNamespaces()))
            return false;
        if (this.getPassing() == null) {
            if (other.getPassing() != null)
                return false;
        } else if (!this.getPassing().equals(other.getPassing()))
            return false;
        if (this.getXquery() == null) {
            if (other.getXquery() != null)
                return false;
        } else if (!this.getXquery().equals(other.getXquery()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public XMLQuery clone() {
        XMLQuery clone = new XMLQuery(this.getTeiidParser(), this.getId());

        if (getPassing() != null)
            clone.setPassing(cloneList(getPassing()));
        if (getNamespaces() != null)
            clone.setNamespaces(getNamespaces().clone());
        clone.setEmptyOnEmpty(isEmptyOnEmpty());
        if (getXquery() != null)
            clone.setXquery(getXquery());

        return clone;
    }

}
