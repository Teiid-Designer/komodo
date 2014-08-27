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
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.symbol.IMultipleElementSymbol;

public class MultipleElementSymbol extends ASTNode implements Expression, IMultipleElementSymbol<ElementSymbol, LanguageVisitor> {

    public MultipleElementSymbol(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public Class<?> getType() {
        return null;
    }

    public String getName() {
        Object property = getProperty(TeiidSqlLexicon.MultipleElementSymbol.NAME_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setName(String name) {
        setProperty(TeiidSqlLexicon.MultipleElementSymbol.NAME_PROP_NAME, name);
    }

    public GroupSymbol getGroup() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.MultipleElementSymbol.GROUP_REF_NAME, GroupSymbol.class);
    }

    public void setGroup(GroupSymbol group) {
        setChild(TeiidSqlLexicon.MultipleElementSymbol.GROUP_REF_NAME, group);
    }

    @Override
    public List<ElementSymbol> getElementSymbols() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.MultipleElementSymbol.ELEMENT_SYMBOLS_REF_NAME, ElementSymbol.class);
    }

    public void setElementSymbols(List<ElementSymbol> elementSymbols) {
        setChildren(TeiidSqlLexicon.MultipleElementSymbol.ELEMENT_SYMBOLS_REF_NAME, elementSymbols);
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
