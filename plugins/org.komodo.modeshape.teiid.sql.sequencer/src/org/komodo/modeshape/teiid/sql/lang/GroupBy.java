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
import org.komodo.spi.annotation.Since;
import org.komodo.spi.query.sql.lang.IGroupBy;
import org.komodo.spi.runtime.version.TeiidVersion.Version;

public class GroupBy extends ASTNode implements IGroupBy<Expression, LanguageVisitor> {

    public GroupBy(ITeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public int getCount() {
        return 0;
    }

    @Override
    public List<Expression> getSymbols() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.GroupBy.SYMBOLS_REF_NAME, Expression.class);
    }

    @Override
    public void addSymbol(Expression symbol) {
        addLastChild(TeiidSqlLexicon.GroupBy.SYMBOLS_REF_NAME, symbol);
    }

    /**
     * @param expressions
     */
    public void setSymbols(List<Expression> expressions) {
        setChildren(TeiidSqlLexicon.GroupBy.SYMBOLS_REF_NAME, expressions);
    }

    @Since(Version.TEIID_8_5)
    public boolean isRollup() {
        if (isLessThanTeiidVersion(TeiidSqlLexicon.GroupBy.ROLLUP_PROP_SINCE_VERSION.get()))
            return false;

        Object property = getProperty(TeiidSqlLexicon.GroupBy.ROLLUP_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    /**
     * @param rollup
     */
    @Since(Version.TEIID_8_5)
    public void setRollup(boolean rollup) {
        if (isLessThanTeiidVersion(TeiidSqlLexicon.GroupBy.ROLLUP_PROP_SINCE_VERSION.get()))
            return;

        setProperty(TeiidSqlLexicon.GroupBy.ROLLUP_PROP_NAME, rollup);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getSymbols() == null) ? 0 : this.getSymbols().hashCode());
        result = prime * result + (this.isRollup() ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        GroupBy other = (GroupBy)obj;
        if (this.getSymbols() == null) {
            if (other.getSymbols() != null) return false;
        } else if (!this.getSymbols().equals(other.getSymbols())) return false;

        if (this.isRollup() != other.isRollup())
            return false;

        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public GroupBy clone() {
        GroupBy clone = new GroupBy(this.getTeiidParser(), this.getId());

        if(getSymbols() != null)
            clone.setSymbols(cloneList(getSymbols()));

        clone.setRollup(isRollup());

        return clone;
    }

    

}
