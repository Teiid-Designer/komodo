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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.lang.ISubqueryFromClause;

public class SubqueryFromClause extends FromClause implements SubqueryContainer<Command>, ISubqueryFromClause<LanguageVisitor, Command> {

    public SubqueryFromClause(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public String getName() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setName(String name) {
    }

    public boolean isTable() {
        return false;
    }

    /**
     * @param table
     */
    public void setTable(boolean table) {
    }

    @Override
    public Command getCommand() {
        return null;
    }

    @Override
    public void setCommand(Command command) {
    }

    @Override
    public void collectGroups(Collection<GroupSymbol> groups) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCommand() == null) ? 0 : this.getCommand().hashCode());
        result = prime * result + (this.isTable() ? 1231 : 1237);
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
        SubqueryFromClause other = (SubqueryFromClause)obj;
        if (this.getCommand() == null) {
            if (other.getCommand() != null)
                return false;
        } else if (!this.getCommand().equals(other.getCommand()))
            return false;
        if (this.getName() == null) {
            if (other.getName() != null)
                return false;
        } else if (!this.getName().equalsIgnoreCase(getName()))
            return false;
        if (this.isTable() != other.isTable())
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public SubqueryFromClause clone() {
        SubqueryFromClause clone = new SubqueryFromClause(this.getTeiidParser(), this.getId());

        if (getCommand() != null)
            clone.setCommand(getCommand().clone());
        clone.setTable(isTable());
        if (getName() != null)
            clone.setName(getName());
        clone.setOptional(isOptional());
        clone.setMakeInd(isMakeInd());
        clone.setNoUnnest(isNoUnnest());
        clone.setMakeDep(isMakeDep());
        clone.setMakeNotDep(isMakeNotDep());
        clone.setPreserve(isPreserve());

        return clone;
    }

}
