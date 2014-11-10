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

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ISetClause;

/**
 *
 */
public class SetClause extends ASTNode implements ISetClause<LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public SetClause(TeiidSeqParser p, int id) {
        super(p, id);
    }

    public ElementSymbol getSymbol() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.SetClause.SYMBOL_REF_NAME, ElementSymbol.class);
    }

    public void setSymbol(ElementSymbol symbol) {
        setChild(TeiidSqlLexicon.SetClause.SYMBOL_REF_NAME, symbol);
    }

    public Expression getValue() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.SetClause.VALUE_REF_NAME, Expression.class);
    }

    public void setValue(Expression value) {
        setChild(TeiidSqlLexicon.SetClause.VALUE_REF_NAME, value);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getSymbol() == null) ? 0 : this.getSymbol().hashCode());
        result = prime * result + ((this.getValue() == null) ? 0 : this.getValue().hashCode());
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
        SetClause other = (SetClause)obj;
        if (this.getSymbol() == null) {
            if (other.getSymbol() != null)
                return false;
        } else if (!this.getSymbol().equals(other.getSymbol()))
            return false;
        if (this.getValue() == null) {
            if (other.getValue() != null)
                return false;
        } else if (!this.getValue().equals(other.getValue()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public SetClause clone() {
        SetClause clone = new SetClause(this.getTeiidParser(), this.getId());

        if (getSymbol() != null)
            clone.setSymbol(getSymbol().clone());
        if (getValue() != null)
            clone.setValue(getValue().clone());

        return clone;
    }

}
