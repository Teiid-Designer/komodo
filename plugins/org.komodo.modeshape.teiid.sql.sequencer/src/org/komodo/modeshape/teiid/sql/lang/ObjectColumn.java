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
import org.komodo.spi.query.sql.lang.IObjectColumn;

public class ObjectColumn extends ProjectedColumn implements IObjectColumn<LanguageVisitor> {

    public ObjectColumn(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * @param path
     */
    public void setPath(String path) {
    }

    /**
     * @return
     */
    public Object getPath() {
        return null;
    }

    /**
     * @param path
     */
    public void setPath(Object path) {
    }

    /**
     * @return
     */
    public Expression getDefaultExpression() {
        return null;
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
        result = prime * result + ((this.getPath() == null) ? 0 : this.getPath().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        ObjectColumn other = (ObjectColumn)obj;
        if (this.getDefaultExpression() == null) {
            if (other.getDefaultExpression() != null) return false;
        } else if (!this.getDefaultExpression().equals(other.getDefaultExpression())) return false;
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
    public ObjectColumn clone() {
        ObjectColumn clone = new ObjectColumn(this.getTeiidParser(), this.getId());

        if(getPath() != null)
            clone.setPath(getPath());
        if(getDefaultExpression() != null)
            clone.setDefaultExpression(getDefaultExpression().clone());
        if(getType() != null)
            clone.setType(getType());
        if(getName() != null)
            clone.setName(getName());

        return clone;
    }

}
