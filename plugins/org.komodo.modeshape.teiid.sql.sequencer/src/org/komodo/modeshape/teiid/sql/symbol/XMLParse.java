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
import org.komodo.spi.query.sql.symbol.IXMLParse;

public class XMLParse extends ASTNode implements Expression, IXMLParse<LanguageVisitor> {

    public XMLParse(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * @return
     */
    private boolean isDocument() {
        return false;
    }

    /**
     * @param doc
     */
    public void setDocument(boolean doc) {
    }

    /**
     * @return
     */
    private Expression getExpression() {
        return null;
    }

    /**
     * @param expr
     */
    public void setExpression(Expression expr) {
    }

    /**
     * @return
     */
    private boolean isWellFormed() {
        return false;
    }

    /**
     * @param wellformed
     */
    public void setWellFormed(boolean wellformed) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isDocument() ? 1231 : 1237);
        result = prime * result + ((this.getExpression() == null) ? 0 : this.getExpression().hashCode());
        result = prime * result + (this.isWellFormed() ? 1231 : 1237);
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
        XMLParse other = (XMLParse)obj;
        if (this.isDocument() != other.isDocument())
            return false;
        if (this.getExpression() == null) {
            if (other.getExpression() != null)
                return false;
        } else if (!this.getExpression().equals(other.getExpression()))
            return false;
        if (this.isWellFormed() != other.isWellFormed())
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public XMLParse clone() {
        XMLParse clone = new XMLParse(this.getTeiidParser(), this.getId());

        if (getExpression() != null)
            clone.setExpression(getExpression().clone());
        clone.setDocument(isDocument());
        clone.setWellFormed(isWellFormed());

        return clone;
    }

    @Override
    public <T> Class<T> getType() {
        return null;
    }

}
