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
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.lang.IUpdate;

/**
 *
 */
public class Update extends ProcedureContainer
    implements TargetedCommand, IUpdate<Expression, LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public Update(TeiidSeqParser p, int id) {
        super(p, id);
        setType(TYPE_UPDATE);
    }

    public SetClauseList getChangeList() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.Update.CHANGE_LIST_REF_NAME, SetClauseList.class);
    }

    public void setChangeList(SetClauseList setClauseList) {
        setChild(TeiidSqlLexicon.Update.CHANGE_LIST_REF_NAME, setClauseList);
    }

    @Override
    public GroupSymbol getGroup() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.TargetedCommand.GROUP_REF_NAME, GroupSymbol.class);
    }

    public void setGroup(GroupSymbol groupSymbol) {
        setChild(TeiidSqlLexicon.TargetedCommand.GROUP_REF_NAME, groupSymbol);
    }

    public Criteria getCriteria() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.Update.CRITERIA_REF_NAME, Criteria.class);
    }

    public void setCriteria(Criteria criteria) {
        setChild(TeiidSqlLexicon.Update.CRITERIA_REF_NAME, criteria);
    }

    @Override
    public List<Expression> getProjectedSymbols() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getChangeList() == null) ? 0 : this.getChangeList().hashCode());
        result = prime * result + ((this.getCriteria() == null) ? 0 : this.getCriteria().hashCode());
        result = prime * result + ((this.getGroup() == null) ? 0 : this.getGroup().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        Update other = (Update)obj;
        if (this.getChangeList() == null) {
            if (other.getChangeList() != null) return false;
        } else if (!this.getChangeList().equals(other.getChangeList())) return false;
        if (this.getCriteria() == null) {
            if (other.getCriteria() != null) return false;
        } else if (!this.getCriteria().equals(other.getCriteria())) return false;
        if (this.getGroup() == null) {
            if (other.getGroup() != null) return false;
        } else if (!this.getGroup().equals(other.getGroup())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Update clone() {
        Update clone = new Update(this.getTeiidParser(), this.getId());

        if(getCriteria() != null)
            clone.setCriteria(getCriteria().clone());
        if(getGroup() != null)
            clone.setGroup(getGroup().clone());
        if(getChangeList() != null)
            clone.setChangeList(getChangeList().clone());
        if(getSourceHint() != null)
            clone.setSourceHint(getSourceHint());
        if(getOption() != null)
            clone.setOption(getOption().clone());

        copyMetadataState(clone);
        return clone;
    }

}
