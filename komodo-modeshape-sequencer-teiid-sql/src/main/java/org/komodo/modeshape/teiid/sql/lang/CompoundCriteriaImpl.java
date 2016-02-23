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
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.lang.CompoundCriteria;

/**
 *
 */
public class CompoundCriteriaImpl extends CriteriaImpl implements CompoundCriteria<CriteriaImpl, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public CompoundCriteriaImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public int getOperator() {
        Object property = getProperty(TeiidSqlLexicon.CompoundCriteria.OPERATOR_PROP_NAME);
        return property == null ? -1 : Integer.parseInt(property.toString());
    }

    /**
     * @param operator value
     */
    public void setOperator(int operator) {
        setProperty(TeiidSqlLexicon.CompoundCriteria.OPERATOR_PROP_NAME, operator);
    }

    @Override
    public List<CriteriaImpl> getCriteria() {
        return getChildrenforIdentifierAndRefType(
                                               TeiidSqlLexicon.CompoundCriteria.CRITERIA_REF_NAME, CriteriaImpl.class);
    }

    @Override
    public int getCriteriaCount() {
        return getCriteria().size();
    }

    @Override
    public CriteriaImpl getCriteria(int index) {
        List<CriteriaImpl> criteria = getCriteria();
        if (index >= criteria.size() || index < 0)
            return null;

        return criteria.get(index);
    }

    @Override
    public void addCriteria(CriteriaImpl criteria) {
        addLastChild(TeiidSqlLexicon.CompoundCriteria.CRITERIA_REF_NAME, criteria);
    }

    /**
     * @param criteria value
     */
    public void setCriteria(List<CriteriaImpl> criteria) {
        setChildren(TeiidSqlLexicon.CompoundCriteria.CRITERIA_REF_NAME, criteria);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCriteria() == null) ? 0 : this.getCriteria().hashCode());
        result = prime * result + this.getOperator();
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
        CompoundCriteriaImpl other = (CompoundCriteriaImpl)obj;
        if (this.getCriteria() == null) {
            if (other.getCriteria() != null)
                return false;
        } else if (!this.getCriteria().equals(other.getCriteria()))
            return false;
        if (this.getOperator() != other.getOperator())
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public CompoundCriteriaImpl clone() {
        CompoundCriteriaImpl clone = new CompoundCriteriaImpl(this.getTeiidParser(), this.getId());

        clone.setOperator(getOperator());
        if (getCriteria() != null)
            clone.setCriteria(cloneList(getCriteria()));

        return clone;
    }

}
