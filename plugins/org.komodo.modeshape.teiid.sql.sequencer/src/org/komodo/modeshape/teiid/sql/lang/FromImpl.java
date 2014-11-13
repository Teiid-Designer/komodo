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

import java.util.ArrayList;
import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbolImpl;
import org.komodo.spi.query.sql.lang.From;

/**
 *
 */
public class FromImpl extends ASTNode implements From<FromClauseImpl, GroupSymbolImpl, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public FromImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public List<FromClauseImpl> getClauses() {
        return getChildrenforIdentifierAndRefType(
                                               TeiidSqlLexicon.From.CLAUSES_REF_NAME, FromClauseImpl.class);
    }

    @Override
    public void setClauses(List<? extends FromClauseImpl> clauses) {
        setChildren(TeiidSqlLexicon.From.CLAUSES_REF_NAME, clauses);
    }

    @Override
    public void addClause(FromClauseImpl clause) {
        addLastChild(TeiidSqlLexicon.From.CLAUSES_REF_NAME, clause);
    }

    @Override
    public List<? extends GroupSymbolImpl> getGroups() {
        List<GroupSymbolImpl> groups = new ArrayList<GroupSymbolImpl>();
        List<FromClauseImpl> clauses = getClauses();
        if (clauses.isEmpty())
            return groups;

        for (int i = 0; i < clauses.size(); i++) {
            FromClauseImpl clause = clauses.get(i);
            clause.collectGroups(groups);
        }
            
        return groups;
    }

    @Override
    public boolean containsGroup(GroupSymbolImpl group) {
        return getGroups().contains(group);
    }

    @Override
    public void addGroup(GroupSymbolImpl group) {
        if( group == null )
            return;

        UnaryFromClauseImpl unaryFromClause = getTeiidParser().createASTNode(ASTNodes.UNARY_FROM_CLAUSE);
        unaryFromClause.setGroup(group);
        addClause(unaryFromClause);
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
        FromImpl other = (FromImpl)obj;
        if (this.getClauses() == null) {
            if (other.getClauses() != null)
                return false;
        } else if (!this.getClauses().equals(other.getClauses()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public FromImpl clone() {
        FromImpl clone = new FromImpl(this.getTeiidParser(), this.getId());

        if (getClauses() != null)
            clone.setClauses(cloneList(getClauses()));

        return clone;
    }
}
