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
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.symbol.MultipleElementSymbol;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;

/**
 *
 */
public class MultipleElementSymbolImpl extends ASTNode implements BaseExpression, MultipleElementSymbol<ElementSymbolImpl, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public MultipleElementSymbolImpl(TeiidSeqParser p, int id) {
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
        GroupSymbolImpl group = getTeiidParser().createASTNode(ASTNodes.GROUP_SYMBOL);
        group.setName(name);
        setGroup(group);
    }

    public GroupSymbolImpl getGroup() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.MultipleElementSymbol.GROUP_REF_NAME, GroupSymbolImpl.class);
    }

    public void setGroup(GroupSymbolImpl group) {
        setChild(TeiidSqlLexicon.MultipleElementSymbol.GROUP_REF_NAME, group);
    }

    @Override
    public List<ElementSymbolImpl> getElementSymbols() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.MultipleElementSymbol.ELEMENT_SYMBOLS_REF_NAME, ElementSymbolImpl.class);
    }

    public void setElementSymbols(List<ElementSymbolImpl> elementSymbols) {
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
        MultipleElementSymbolImpl other = (MultipleElementSymbolImpl)obj;
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
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public MultipleElementSymbolImpl clone() {
        MultipleElementSymbolImpl clone = new MultipleElementSymbolImpl(this.getTeiidParser(), this.getId());

        if (getGroup() != null)
            clone.setGroup(getGroup().clone());
        if (getElementSymbols() != null)
            clone.setElementSymbols(cloneList(getElementSymbols()));

        return clone;
    }

}
