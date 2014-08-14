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
import org.komodo.spi.query.sql.lang.ILanguageObject;
import org.komodo.spi.query.sql.symbol.IElementSymbol;
import org.komodo.spi.query.sql.symbol.IReference;

public class Reference extends ASTNode implements Expression, IReference<LanguageVisitor> {

    public Reference(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public <T> Class<T> getType() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param type
     */
    public void setType(Class<Object> type) {
    }

    @Override
    public boolean isPositional() {
        return false;
    }

    /**
     * @param b
     */
    public void setPositional(boolean b) {
    }

    @Override
    public IElementSymbol getExpression() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param clone
     */
    public void setExpression(ILanguageObject clone) {
    }

    /**
     * @return
     */
    public int getIndex() {
        return 0;
    }

    /**
     * @param referenceIndex
     */
    public void setIndex(int referenceIndex) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getExpression() == null) ? 0 : this.getExpression().hashCode());
        result = prime * result + (this.isPositional() ? 1231 : 1237);
//        result = prime * result + this.getRefIndex();
        result = prime * result + ((this.getType() == null) ? 0 : this.getType().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        Reference other = (Reference)obj;
        if (this.getExpression() == null) {
            if (other.getExpression() != null) return false;
        } else if (!this.getExpression().equals(other.getExpression())) return false;
        if (this.isPositional() != other.isPositional()) return false;
//        if (this.getRefIndex() != other.getRefIndex()) return false;
        if (this.getType() == null) {
            if (other.getType() != null) return false;
        } else if (!this.getType().equals(other.getType())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Reference clone() {
        Reference clone = new Reference(this.getTeiidParser(), this.getId());

        if(getExpression() != null)
            clone.setExpression(getExpression().clone());
        if(this.getType() != null)
            clone.setType(this.getType());
        clone.setPositional(isPositional());
        clone.setIndex(getIndex());
//        clone.setConstraint(clone.getConstraint());
        
        return clone;
    }

}
