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

import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.symbol.IXMLSerialize;

public class XMLSerialize extends ASTNode implements Expression, IXMLSerialize<LanguageVisitor> {

    public XMLSerialize(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public <T> Class<T> getType() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return
     */
    public String getEncoding() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param enc
     */
    public void setEncoding(String enc) {
    }

    /**
     * @return
     */
    public String getVersion() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param ver
     */
    public void setVersion(String ver) {
    }

    /**
     * @return
     */
    public Boolean getDeclaration() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param declr
     */
    public void setDeclaration(Boolean declr) {
    }

    /**
     * @return
     */
    public Expression getExpression() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param expr
     */
    public void setExpression(Expression expr) {
    }

    /**
     * @return
     */
    public Boolean getDocument() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param doc
     */
    public void setDocument(Boolean doc) {
    }

    /**
     * @return
     */
    public String getTypeString() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param image
     */
    public void setTypeString(String image) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getDeclaration() == null) ? 0 : this.getDeclaration().hashCode());
        result = prime * result + ((this.getDocument() == null) ? 0 : this.getDocument().hashCode());
        result = prime * result + ((this.getEncoding() == null) ? 0 : this.getEncoding().hashCode());
        result = prime * result + ((this.getExpression() == null) ? 0 : this.getExpression().hashCode());
        result = prime * result + ((this.getType() == null) ? 0 : this.getType().hashCode());
        result = prime * result + ((this.getTypeString() == null) ? 0 : this.getTypeString().hashCode());
        result = prime * result + ((this.getVersion() == null) ? 0 : this.getVersion().hashCode());
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
        XMLSerialize other = (XMLSerialize)obj;
        if (this.getDeclaration() == null) {
            if (other.getDeclaration() != null)
                return false;
        } else if (!this.getDeclaration().equals(other.getDeclaration()))
            return false;
        if (this.getDocument() == null) {
            if (other.getDocument() != null)
                return false;
        } else if (!this.getDocument().equals(other.getDocument()))
            return false;
        if (this.getEncoding() == null) {
            if (other.getEncoding() != null)
                return false;
        } else if (!this.getEncoding().equals(other.getEncoding()))
            return false;
        if (this.getExpression() == null) {
            if (other.getExpression() != null)
                return false;
        } else if (!this.getExpression().equals(other.getExpression()))
            return false;
        if (this.getType() == null) {
            if (other.getType() != null)
                return false;
        } else if (!this.getType().equals(other.getType()))
            return false;
        if (this.getTypeString() == null) {
            if (other.getTypeString() != null)
                return false;
        } else if (!this.getTypeString().equals(other.getTypeString()))
            return false;
        if (this.getVersion() == null) {
            if (other.getVersion() != null)
                return false;
        } else if (!this.getVersion().equals(other.getVersion()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public XMLSerialize clone() {
        XMLSerialize clone = new XMLSerialize(this.getTeiidParser(), this.getId());

        if (getExpression() != null)
            clone.setExpression(getExpression().clone());
        if (getEncoding() != null)
            clone.setEncoding(getEncoding());
        clone.setDocument(getDocument());
        clone.setDeclaration(getDeclaration());
        if (getVersion() != null)
            clone.setVersion(getVersion());
        if (getTypeString() != null)
            clone.setTypeString(getTypeString());

        return clone;
    }

}
