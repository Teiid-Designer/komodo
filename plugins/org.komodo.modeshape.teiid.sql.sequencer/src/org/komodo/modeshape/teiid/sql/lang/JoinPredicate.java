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
import java.util.Collection;
import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.lang.IJoinPredicate;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;


public class JoinPredicate extends FromClause implements IJoinPredicate<FromClause, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public JoinPredicate(TeiidSeqParser p, int id) {
        super(p, id);
        JoinType joinType = p.createASTNode(ASTNodes.JOIN_TYPE);
        joinType.setKind(JoinType.Types.JOIN_INNER);
        setJoinType(joinType);
    }

    @Override
    public FromClause getLeftClause() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.JoinPredicate.LEFT_CLAUSE_REF_NAME, FromClause.class);
    }

    @Override
    public void setLeftClause(FromClause fromClause) {
        setChild(TeiidSqlLexicon.JoinPredicate.LEFT_CLAUSE_REF_NAME, fromClause);
    }

    @Override
    public FromClause getRightClause() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.JoinPredicate.RIGHT_CLAUSE_REF_NAME, FromClause.class);
    }

    @Override
    public void setRightClause(FromClause fromClause) {
        setChild(TeiidSqlLexicon.JoinPredicate.RIGHT_CLAUSE_REF_NAME, fromClause);
    }

    public JoinType getJoinType() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.JoinPredicate.JOIN_TYPE_REF_NAME, JoinType.class);
    }

    /**
     * @param joinType
     */
    public void setJoinType(JoinType joinType) {
        setChild(TeiidSqlLexicon.JoinPredicate.JOIN_TYPE_REF_NAME, joinType);
    }

    public List<Criteria> getJoinCriteria() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.JoinPredicate.JOIN_CRITERIA_REF_NAME, Criteria.class);
    }

    /**
     * @param separateCriteriaByAnd
     */
    public void setJoinCriteria(List<Criteria> criteria) {
        List<Criteria> newCriteria = new ArrayList<Criteria>();
        for (Criteria criterium : criteria) {
            newCriteria.addAll(Criteria.separateCriteriaByAnd(criterium));
        }

        setChildren(TeiidSqlLexicon.JoinPredicate.JOIN_CRITERIA_REF_NAME, newCriteria);
    }

    @Override
    public void collectGroups(Collection<GroupSymbol> groups) {
        FromClause leftClause = getLeftClause();
        FromClause rightClause = getRightClause();
        if (leftClause != null)
            leftClause.collectGroups(groups);

        if (rightClause != null)
            rightClause.collectGroups(groups);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getJoinCriteria() == null) ? 0 : this.getJoinCriteria().hashCode());
        result = prime * result + ((this.getJoinType() == null) ? 0 : this.getJoinType().hashCode());
        result = prime * result + ((this.getLeftClause() == null) ? 0 : this.getLeftClause().hashCode());
        result = prime * result + ((this.getRightClause() == null) ? 0 : this.getRightClause().hashCode());
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
        JoinPredicate other = (JoinPredicate)obj;
        if (this.getJoinCriteria() == null) {
            if (other.getJoinCriteria() != null)
                return false;
        } else if (!this.getJoinCriteria().equals(other.getJoinCriteria()))
            return false;
        if (this.getJoinType() == null) {
            if (other.getJoinType() != null)
                return false;
        } else if (!this.getJoinType().equals(other.getJoinType()))
            return false;
        if (this.getLeftClause() == null) {
            if (other.getLeftClause() != null)
                return false;
        } else if (!this.getLeftClause().equals(other.getLeftClause()))
            return false;
        if (this.getRightClause() == null) {
            if (other.getRightClause() != null)
                return false;
        } else if (!this.getRightClause().equals(other.getRightClause()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public JoinPredicate clone() {
        JoinPredicate clone = new JoinPredicate(this.getTeiidParser(), this.getId());

        if (getLeftClause() != null)
            clone.setLeftClause(getLeftClause().clone());
        if (getRightClause() != null)
            clone.setRightClause(getRightClause().clone());
        if (getJoinType() != null)
            clone.setJoinType(getJoinType().clone());
        if (getJoinCriteria() != null)
            clone.setJoinCriteria(cloneList(getJoinCriteria()));

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
