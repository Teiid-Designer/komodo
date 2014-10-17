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

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.modeshape.teiid.sql.util.SymbolMap;
import org.komodo.spi.query.sql.lang.IInsert;

public class Insert extends ProcedureContainer implements TargetedCommand, IInsert<ElementSymbol, Expression, GroupSymbol, QueryCommand, LanguageVisitor> {

    public Insert(ITeiidParser p, int id) {
        super(p, id);
        setType(TYPE_INSERT);
    }

    @Override
    public List<Expression> getProjectedSymbols() {
        throw new UnsupportedOperationException();
    }

    @Override
    public List<ElementSymbol> getVariables() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.Insert.VARIABLES_REF_NAME, ElementSymbol.class);
    }

    @Override
    public void addVariable(ElementSymbol symbol) {
        addLastChild(TeiidSqlLexicon.Insert.VARIABLES_REF_NAME, symbol);
    }

    @Override
    public void addVariables(Collection<ElementSymbol> symbols) {
        for (ElementSymbol es : symbols) {
            addVariable(es);
        }
    }

    @Override
    public void setVariables(Collection<ElementSymbol> vars) {
        setChildren(TeiidSqlLexicon.Insert.VARIABLES_REF_NAME, vars);
    }

    @Override
    public List<Expression> getValues() {
        return getChildrenforIdentifierAndRefType(
                                               TeiidSqlLexicon.Insert.VALUES_REF_NAME, Expression.class);
    }

    public void addValue(Expression value) {
        addLastChild(TeiidSqlLexicon.Insert.VALUES_REF_NAME, value);
    }

    @Override
    public void setValues(List<? extends Expression> values) {
        setChildren(TeiidSqlLexicon.Insert.VALUES_REF_NAME, values);
    }

    @Override
    public QueryCommand getQueryExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.Insert.QUERY_EXPRESSION_REF_NAME, QueryCommand.class);
    }

    /**
     * @param query
     */
    public void setQueryExpression(QueryCommand query) {
        if (isTeiid8OrGreater() && query instanceof Query) {
            /*
             * Modified in Teiid 8.6 due to TEIID-2698.
             * This moves the addition of values from the parser to here
             * and is backward compatible with all previous version 8+ parsers.
             */
            Query expr = (Query)query;
            //a singl row constructor query is the same as values 
            if (expr.isRowConstructor()) {
                List<Expression> emptyList = Collections.emptyList();
                setValues(emptyList);
                removeChildren(TeiidSqlLexicon.Insert.QUERY_EXPRESSION_REF_NAME);
                for (Expression ex : expr.getSelect().getSymbols()) {
                    addValue(SymbolMap.getExpression(ex));
                }
                if (expr.getOption() != null && this.getOption() == null) {
                    //this isn't ideal, parsing associates the option with values
                    this.setOption(expr.getOption());
                }
                return;
            }
        }

        setChild(TeiidSqlLexicon.Insert.QUERY_EXPRESSION_REF_NAME, query);
    }

    @Override
    public GroupSymbol getGroup() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.TargetedCommand.GROUP_REF_NAME, GroupSymbol.class);
    }

    @Override
    public void setGroup(GroupSymbol group) {
        setChild(TeiidSqlLexicon.TargetedCommand.GROUP_REF_NAME, group);
    }

    private boolean isMerge() {
        Object property = getProperty(TeiidSqlLexicon.Insert.MERGE_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    /**
     * @param merge
     */
    public void setMerge(boolean merge) {
        setProperty(TeiidSqlLexicon.Insert.MERGE_PROP_NAME, merge);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getGroup() == null) ? 0 : this.getGroup().hashCode());
        result = prime * result + (this.isMerge() ? 1231 : 1237);
        result = prime * result + ((this.getQueryExpression() == null) ? 0 : this.getQueryExpression().hashCode());
        result = prime * result + ((this.getValues() == null) ? 0 : this.getValues().hashCode());
        result = prime * result + ((this.getVariables() == null) ? 0 : this.getVariables().hashCode());
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
        Insert other = (Insert)obj;
        if (this.getGroup() == null) {
            if (other.getGroup() != null)
                return false;
        } else if (!this.getGroup().equals(other.getGroup()))
            return false;
        if (this.isMerge() != other.isMerge())
            return false;
        if (this.getQueryExpression() == null) {
            if (other.getQueryExpression() != null)
                return false;
        } else if (!this.getQueryExpression().equals(other.getQueryExpression()))
            return false;
        if (this.getValues() == null) {
            if (other.getValues() != null)
                return false;
        } else if (!this.getValues().equals(other.getValues()))
            return false;
        if (this.getVariables() == null) {
            if (other.getVariables() != null)
                return false;
        } else if (!this.getVariables().equals(other.getVariables()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Insert clone() {
        Insert clone = new Insert(this.getTeiidParser(), this.getId());

        if (getGroup() != null)
            clone.setGroup(getGroup().clone());
        if (getVariables() != null)
            clone.setVariables(cloneList(getVariables()));
        if (getValues() != null)
            clone.setValues(cloneList(getValues()));
        if (getQueryExpression() != null)
            clone.setQueryExpression(getQueryExpression().clone());
        clone.setMerge(isMerge());
        if (getSourceHint() != null)
            clone.setSourceHint(getSourceHint());
        if (getOption() != null)
            clone.setOption(getOption().clone());

        copyMetadataState(clone);
        return clone;
    }

}
