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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.IGroupBy;

public class GroupBy extends ASTNode implements IGroupBy<Expression, LanguageVisitor> {

    public GroupBy(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public int getCount() {
        return 0;
    }

    @Override
    public List<Expression> getSymbols() {
        return null;
    }

    @Override
    public void addSymbol(Expression symbol) {
    }

    /**
     * @param expressions
     */
    public void setSymbols(List<Expression> expressions) {
    }

    public boolean isRollup() {
        return false;
    }

    /**
     * @param rollup
     */
    public void setRollup(boolean rollup) {
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
