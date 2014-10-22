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

import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.modeshape.teiid.sql.symbol.ExpressionSymbol;
import org.komodo.modeshape.teiid.sql.symbol.MultipleElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Symbol;
import org.komodo.spi.query.sql.lang.ISelect;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;

/**
 *
 */
public class Select extends ASTNode implements ISelect<Expression, LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public Select(ITeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public List<Expression> getSymbols() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.Select.SYMBOLS_REF_NAME, Expression.class);
    }

    public Expression getSymbol( int index ) {
        return getSymbols().get(index);
    }

    @Override
    public void addSymbol(Expression expression) {
        Expression symbol = wrapSymbol(expression);
        addLastChild(TeiidSqlLexicon.Select.SYMBOLS_REF_NAME, symbol);
    }

    /**
     * @param expression
     * @return
     */
    private Expression wrapSymbol(Expression expression) {
        if (expression == null)
            return null;

        if (expression instanceof Symbol)
            return expression;

        if (expression instanceof MultipleElementSymbol)
            return expression;

        ExpressionSymbol exSymbol = getTeiidParser().createASTNode(ASTNodes.EXPRESSION_SYMBOL);
        exSymbol.setName("expr" + (getSymbols().size() + 1)); //$NON-NLS-1$
        exSymbol.setExpression(expression);
        expression = exSymbol;

        return exSymbol;
    }

    @Override
    public void setSymbols(List<? extends Expression> symbols) {
        setChildren(TeiidSqlLexicon.Select.SYMBOLS_REF_NAME, symbols);
    }

    /**
     * Checks for a Select * clause
     * @return True if Select * is used
     */
    @Override
    public boolean isStar() {
        List<Expression> symbols = getSymbols();
        return (symbols.size() == 1 && symbols.get(0) instanceof MultipleElementSymbol && ((MultipleElementSymbol)symbols.get(0)).getGroup() == null);
    }

    @Override
    public boolean isDistinct() {
        Object property = getProperty(TeiidSqlLexicon.Select.DISTINCT_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    @Override
    public void setDistinct(boolean isDistinct) {
        setProperty(TeiidSqlLexicon.Select.DISTINCT_PROP_NAME, isDistinct);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isDistinct() ? 1231 : 1237);
        result = prime * result + ((this.getSymbols() == null) ? 0 : this.getSymbols().hashCode());
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
        Select other = (Select)obj;
        if (this.isDistinct() != other.isDistinct())
            return false;
        if (this.getSymbols() == null) {
            if (other.getSymbols() != null)
                return false;
        } else if (!this.getSymbols().equals(other.getSymbols()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Select clone() {
        Select clone = new Select(this.getTeiidParser(), this.getId());

        clone.setDistinct(isDistinct());
        if (getSymbols() != null)
            clone.setSymbols(cloneList(getSymbols()));

        return clone;
    }

}
