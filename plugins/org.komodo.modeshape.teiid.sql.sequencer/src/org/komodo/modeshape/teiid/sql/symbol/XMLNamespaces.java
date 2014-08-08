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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.lang.NamespaceItem;
import org.komodo.spi.query.sql.symbol.IXMLNamespaces;

public class XMLNamespaces extends ASTNode implements IXMLNamespaces<LanguageVisitor> {

    public XMLNamespaces(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * @return
     */
    public List<NamespaceItem> getNamespaceItems() {
        return null;
    }

    /**
     * @param namespaces
     */
    public void setNamespaces(List<NamespaceItem> namespaces) {
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
        XMLNamespaces other = (XMLNamespaces)obj;
        if (this.getNamespaceItems() == null) {
            if (other.getNamespaceItems() != null)
                return false;
        } else if (!this.getNamespaceItems().equals(other.getNamespaceItems()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public XMLNamespaces clone() {
        XMLNamespaces clone = new XMLNamespaces(this.getTeiidParser(), this.getId());

        if (getNamespaceItems() != null) {
            List<NamespaceItem> cloneList = new ArrayList<NamespaceItem>();
            for (NamespaceItem item : getNamespaceItems()) {
                cloneList.add(item.clone());
            }
            clone.setNamespaces(cloneList);
        }

        return clone;
    }

}
