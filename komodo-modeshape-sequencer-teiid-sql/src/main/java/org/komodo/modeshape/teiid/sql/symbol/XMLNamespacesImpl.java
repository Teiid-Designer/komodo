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

import java.util.ArrayList;
import java.util.List;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.lang.NamespaceItem;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.symbol.XMLNamespaces;

/**
 *
 */
public class XMLNamespacesImpl extends ASTNode implements XMLNamespaces<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public XMLNamespacesImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    public List<NamespaceItem> getNamespaceItems() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.XMLNamespaces.NAMESPACE_ITEMS_REF_NAME, NamespaceItem.class);
    }

    public void setNamespaceItems(List<NamespaceItem> namespaces) {
        setChildren(TeiidSqlLexicon.XMLNamespaces.NAMESPACE_ITEMS_REF_NAME, namespaces);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getNamespaceItems() == null) ? 0 : this.getNamespaceItems().hashCode());
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
        XMLNamespacesImpl other = (XMLNamespacesImpl)obj;
        if (this.getNamespaceItems() == null) {
            if (other.getNamespaceItems() != null)
                return false;
        } else if (!this.getNamespaceItems().equals(other.getNamespaceItems()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public XMLNamespacesImpl clone() {
        XMLNamespacesImpl clone = new XMLNamespacesImpl(this.getTeiidParser(), this.getId());

        if (getNamespaceItems() != null) {
            List<NamespaceItem> cloneList = new ArrayList<NamespaceItem>();
            for (NamespaceItem item : getNamespaceItems()) {
                cloneList.add(item.clone());
            }
            clone.setNamespaceItems(cloneList);
        }

        return clone;
    }

}
