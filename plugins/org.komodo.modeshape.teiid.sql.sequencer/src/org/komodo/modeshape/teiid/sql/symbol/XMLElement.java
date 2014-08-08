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
import org.komodo.spi.query.sql.symbol.IXMLElement;

public class XMLElement extends ASTNode implements Expression, IXMLElement<LanguageVisitor> {

    public XMLElement(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public <T> Class<T> getType() {
        return null;
    }

    /**
     * @return
     */
    public List<Expression> getContent() {
        return null;
    }

    /**
     * @param content
     */
    public void setContent(List<Expression> content) {
    }

    /**
     * @param xmlNamespaces
     */
    public void setNamespaces(XMLNamespaces xmlNamespaces) {
    }

    /**
     * @param xmlAttributes
     */
    public void setAttributes(XMLAttributes xmlAttributes) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getAttributes() == null) ? 0 : this.getAttributes().hashCode());
        result = prime * result + ((this.getContent() == null) ? 0 : this.getContent().hashCode());
        result = prime * result + ((this.getName() == null) ? 0 : this.getName().hashCode());
        result = prime * result + ((this.getNamespaces() == null) ? 0 : this.getNamespaces().hashCode());
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
        XMLElement other = (XMLElement)obj;
        if (this.getAttributes() == null) {
            if (other.getAttributes() != null)
                return false;
        } else if (!this.getAttributes().equals(other.getAttributes()))
            return false;
        if (this.getContent() == null) {
            if (other.getContent() != null)
                return false;
        } else if (!this.getContent().equals(other.getContent()))
            return false;
        if (this.getName() == null) {
            if (other.getName() != null)
                return false;
        } else if (!this.getName().equals(other.getName()))
            return false;
        if (this.getNamespaces() == null) {
            if (other.getNamespaces() != null)
                return false;
        } else if (!this.getNamespaces().equals(other.getNamespaces()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public XMLElement clone() {
        XMLElement clone = new XMLElement(this.getTeiidParser(), this.getId());

        if (getContent() != null)
            clone.setContent(cloneList(getContent()));
        if (getNamespaces() != null)
            clone.setNamespaces(getNamespaces().clone());
        if (getAttributes() != null)
            clone.setAttributes(getAttributes().clone());
        if (getName() != null)
            clone.setName(getName());

        return clone;
    }

    /**
     * @return
     */
    public XMLAttributes getAttributes() {
        return null;
    }

    /**
     * @return
     */
    public XMLNamespaces getNamespaces() {
        return null;
    }

}
