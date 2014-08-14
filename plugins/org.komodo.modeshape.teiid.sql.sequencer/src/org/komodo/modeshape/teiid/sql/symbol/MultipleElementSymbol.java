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
import org.komodo.spi.query.sql.symbol.IMultipleElementSymbol;

public class MultipleElementSymbol extends ASTNode implements Expression, IMultipleElementSymbol<ElementSymbol, LanguageVisitor> {

    public MultipleElementSymbol(TeiidParser p, int id) {
        super(p, id);
    }

    public String getName() {
        throw new UnsupportedOperationException();
    }

    public void setName(String name) {
    }

    @Override
    public <T> Class<T> getType() {
        return null;
    }

    /**
     * @return null if selecting all groups, otherwise the specific group
     */
    public GroupSymbol getGroup() {
        return null;
    }

    /**
     * @param group
     */
    public void setGroup(GroupSymbol group) {
    }

    @Override
    public List<ElementSymbol> getElementSymbols() {
        return null;
    }

    public void setElementSymbols(List<ElementSymbol> elementSymbols) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getElementSymbols() == null) ? 0 : this.getElementSymbols().hashCode());
        result = prime * result + ((this.getGroup() == null) ? 0 : this.getGroup().hashCode());
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
        MultipleElementSymbol other = (MultipleElementSymbol)obj;
        if (this.getElementSymbols() == null) {
            if (other.getElementSymbols() != null)
                return false;
        } else if (!this.getElementSymbols().equals(other.getElementSymbols()))
            return false;
        if (this.getGroup() == null) {
            if (other.getGroup() != null)
                return false;
        } else if (!this.getGroup().equals(other.getGroup()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public MultipleElementSymbol clone() {
        MultipleElementSymbol clone = new MultipleElementSymbol(this.getTeiidParser(), this.getId());

        if (getGroup() != null)
            clone.setGroup(getGroup().clone());
        if (getElementSymbols() != null)
            clone.setElementSymbols(cloneList(getElementSymbols()));

        return clone;
    }

}
