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

import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbolImpl;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.lang.SetClause;

/**
 *
 */
public class SetClauseImpl extends ASTNode implements SetClause<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public SetClauseImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    public ElementSymbolImpl getSymbol() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.SetClause.SYMBOL_REF_NAME, ElementSymbolImpl.class);
    }

    public void setSymbol(ElementSymbolImpl symbol) {
        setChild(TeiidSqlLexicon.SetClause.SYMBOL_REF_NAME, symbol);
    }

    public BaseExpression getValue() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.SetClause.VALUE_REF_NAME, BaseExpression.class);
    }

    public void setValue(BaseExpression value) {
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
        SetClauseImpl other = (SetClauseImpl)obj;
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
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public SetClauseImpl clone() {
        SetClauseImpl clone = new SetClauseImpl(this.getTeiidParser(), this.getId());

        if (getSymbol() != null)
            clone.setSymbol(getSymbol().clone());
        if (getValue() != null)
            clone.setValue(getValue().clone());

        return clone;
    }

}
