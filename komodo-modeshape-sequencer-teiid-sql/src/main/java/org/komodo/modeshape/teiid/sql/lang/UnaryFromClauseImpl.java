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
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbolImpl;
import org.komodo.spi.query.sql.lang.UnaryFromClause;

/**
 *
 */
public class UnaryFromClauseImpl extends FromClauseImpl implements UnaryFromClause<GroupSymbolImpl, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public UnaryFromClauseImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public GroupSymbolImpl getGroup() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.UnaryFromClause.GROUP_REF_NAME, GroupSymbolImpl.class);
    }

    @Override
    public void setGroup(GroupSymbolImpl group) {
        setChild(TeiidSqlLexicon.UnaryFromClause.GROUP_REF_NAME, group);
    }

    public CommandImpl getExpandedCommand() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.UnaryFromClause.EXPANDED_COMMAND_REF_NAME, CommandImpl.class);
    }

    public void setExpandedCommand(CommandImpl command) {
        setChild(TeiidSqlLexicon.UnaryFromClause.EXPANDED_COMMAND_REF_NAME, command);
    }

    @Override
    public void collectGroups(Collection<GroupSymbolImpl> groups) {
        groups.add(getGroup());
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getExpandedCommand() == null) ? 0 : this.getExpandedCommand().hashCode());
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
        UnaryFromClauseImpl other = (UnaryFromClauseImpl)obj;
        if (this.getExpandedCommand() == null) {
            if (other.getExpandedCommand() != null)
                return false;
        } else if (!this.getExpandedCommand().equals(other.getExpandedCommand()))
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
    public UnaryFromClauseImpl clone() {
        UnaryFromClauseImpl clone = new UnaryFromClauseImpl(this.getTeiidParser(), this.getId());

        if (getGroup() != null)
            clone.setGroup(getGroup().clone());
        if (getExpandedCommand() != null)
            clone.setExpandedCommand(getExpandedCommand().clone());
        clone.setOptional(isOptional());
        clone.setMakeInd(isMakeInd());
        clone.setNoUnnest(isNoUnnest());
        if (getMakeDependency() != null)
            clone.setMakeDependency(getMakeDependency().clone());
        clone.setMakeNotDep(isMakeNotDep());
        clone.setPreserve(isPreserve());

        return clone;
    }

}
