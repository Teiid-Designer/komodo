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
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.lang.IFrom;

public class From extends ASTNode implements IFrom<FromClause, GroupSymbol, LanguageVisitor> {

    public From(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public List<FromClause> getClauses() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setClauses(List<? extends FromClause> clauses) {
    }

    @Override
    public void addClause(FromClause clause) {
    }

    @Override
    public List<? extends GroupSymbol> getGroups() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean containsGroup(GroupSymbol group) {
        return false;
    }

    @Override
    public void addGroup(GroupSymbol group) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getClauses() == null) ? 0 : this.getClauses().hashCode());
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
        From other = (From)obj;
        if (this.getClauses() == null) {
            if (other.getClauses() != null)
                return false;
        } else if (!this.getClauses().equals(other.getClauses()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public From clone() {
        From clone = new From(this.getTeiidParser(), this.getId());

        if (getClauses() != null)
            clone.setClauses(cloneList(getClauses()));

        return clone;
    }
}
