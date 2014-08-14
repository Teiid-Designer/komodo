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

import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.IXMLColumn;

public class XMLColumn extends ProjectedColumn implements IXMLColumn<LanguageVisitor> {

    public XMLColumn(TeiidParser p, int id) {
        super(p, id);
    }

    public String getName() {
        throw new UnsupportedOperationException();
    }

    public void setName(String name) {
    }

    public boolean isOrdinal() {
        return false;
    }

    /**
     * @param b
     */
    public void setOrdinal(boolean b) {
    }

    /**
     * @return
     */
    public String getPath() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param path
     */
    public void setPath(String path) {
    }

    /**
     * @return
     */
    public Expression getDefaultExpression() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param defaultExpr
     */
    public void setDefaultExpression(Expression defaultExpr) {
    }
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getDefaultExpression() == null) ? 0 : this.getDefaultExpression().hashCode());
        result = prime * result + (this.isOrdinal() ? 1231 : 1237);
        result = prime * result + ((this.getPath() == null) ? 0 : this.getPath().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        XMLColumn other = (XMLColumn)obj;
        if (this.getDefaultExpression() == null) {
            if (other.getDefaultExpression() != null) return false;
        } else if (!this.getDefaultExpression().equals(other.getDefaultExpression())) return false;
        if (this.isOrdinal() != other.isOrdinal()) return false;
        if (this.getPath() == null) {
            if (other.getPath() != null) return false;
        } else if (!this.getPath().equals(other.getPath())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public XMLColumn clone() {
        XMLColumn clone = new XMLColumn(this.getTeiidParser(), this.getId());

        if(getPath() != null)
            clone.setPath(getPath());
        if(getDefaultExpression() != null)
            clone.setDefaultExpression(getDefaultExpression().clone());
        clone.setOrdinal(isOrdinal());
        if(getName() != null)
            clone.setName(getName());
        if(getType() != null)
            clone.setType(getType());

        return clone;
    }

}
