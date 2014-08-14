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
import org.komodo.spi.query.sql.lang.ILimit;

public class Limit extends ASTNode implements ILimit<LanguageVisitor> {

    /**
     * Non Strict token
     */
    public static String NON_STRICT = "NON_STRICT"; //$NON-NLS-1$

    public Limit(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * @return
     */
    public Expression getOffset() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param offset
     */
    public void setOffset(Expression offset) {
    }

    /**
     * @return
     */
    public Expression getRowLimit() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param limit
     */
    public void setRowLimit(Expression limit) {
    }

    /**
     * @return
     */
    public boolean isStrict() {
        return false;
    }

    /**
     * @param b
     */
    public void setStrict(boolean b) {
    }
    /**
     * @return
     */
    public boolean isImplicit() {
        return false;
    }

    /**
     * @param implicit
     */
    public void setImplicit(boolean implicit) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isImplicit() ? 1231 : 1237);
        result = prime * result + ((this.getOffset() == null) ? 0 : this.getOffset().hashCode());
        result = prime * result + ((this.getRowLimit() == null) ? 0 : this.getRowLimit().hashCode());
        result = prime * result + (this.isStrict() ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        Limit other = (Limit)obj;
        if (this.isImplicit() != other.isImplicit()) return false;
        if (this.getOffset() == null) {
            if (other.getOffset() != null) return false;
        } else if (!this.getOffset().equals(other.getOffset())) return false;
        if (this.getRowLimit() == null) {
            if (other.getRowLimit() != null) return false;
        } else if (!this.getRowLimit().equals(other.getRowLimit())) return false;
        if (this.isStrict() != other.isStrict()) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Limit clone() {
        Limit clone = new Limit(this.getTeiidParser(), this.getId());

        clone.setStrict(isStrict());
        clone.setImplicit(isImplicit());
        if(getRowLimit() != null)
            clone.setRowLimit(getRowLimit().clone());
        if(getOffset() != null)
            clone.setOffset(getOffset().clone());

        return clone;
    }

}
