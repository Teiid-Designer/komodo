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

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.spi.query.sql.symbol.IAliasSymbol;

/**
 *
 */
public class AliasSymbol extends Symbol implements Expression, IAliasSymbol<Expression, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public AliasSymbol(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public Class<?> getType() {
        return this.getSymbol().getType();
    }

    @Override
    public Expression getSymbol() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.AliasSymbol.SYMBOL_REF_NAME, Expression.class);
    }

    @Override
    public void setSymbol(Expression symbol) {
        setChild(TeiidSqlLexicon.AliasSymbol.SYMBOL_REF_NAME, symbol);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getSymbol() == null) ? 0 : this.getSymbol().hashCode());
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
        AliasSymbol other = (AliasSymbol)obj;
        if (this.getSymbol() == null) {
            if (other.getSymbol() != null)
                return false;
        } else if (!this.getSymbol().equals(other.getSymbol()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public AliasSymbol clone() {
        AliasSymbol clone = new AliasSymbol(this.getTeiidParser(), this.getId());

        if (getSymbol() != null)
            clone.setSymbol(getSymbol().clone());
        if (getShortName() != null)
            clone.setShortName(getShortName());
        if (getName() != null)
            clone.setName(getName());
        if (getOutputName() != null)
            clone.setOutputName(getOutputName());

        return clone;
    }

}
