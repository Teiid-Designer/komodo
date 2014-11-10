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
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.spi.query.sql.lang.ISetClauseList;

/**
 *
 */
public class SetClauseList extends ASTNode implements ISetClauseList<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public SetClauseList(TeiidSeqParser p, int id) {
        super(p, id);
    }

    public List<SetClause> getSetClauses() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.SetClauseList.SET_CLAUSES_REF_NAME, SetClause.class);
    }

    public void addSetClause(SetClause setClause) {
        addLastChild(TeiidSqlLexicon.SetClauseList.SET_CLAUSES_REF_NAME, setClause);
    }

    public void setSetClauses(List<SetClause> setClauses) {
        setChildren(TeiidSqlLexicon.SetClauseList.SET_CLAUSES_REF_NAME, setClauses);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getSetClauses() == null) ? 0 : this.getSetClauses().hashCode());
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
        SetClauseList other = (SetClauseList)obj;
        if (this.getSetClauses() == null) {
            if (other.getSetClauses() != null)
                return false;
        } else if (!this.getSetClauses().equals(other.getSetClauses()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public SetClauseList clone() {
        SetClauseList clone = new SetClauseList(this.getTeiidParser(), this.getId());
        clone.getSetClauses().addAll(cloneList(getSetClauses()));
        return clone;
    }

}
