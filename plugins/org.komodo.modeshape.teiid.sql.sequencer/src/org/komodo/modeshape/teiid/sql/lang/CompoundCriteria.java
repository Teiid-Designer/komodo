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
import org.komodo.spi.query.sql.lang.ICompoundCriteria;

/**
 *
 */
public class CompoundCriteria extends Criteria implements ICompoundCriteria<Criteria, LanguageVisitor> {

    /**
     * @param p
     * @param id
     */
    public CompoundCriteria(ITeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public int getOperator() {
        Object property = getProperty(TeiidSqlLexicon.CompoundCriteria.OPERATOR_PROP_NAME);
        return property == null ? -1 : Integer.parseInt(property.toString());
    }

    /**
     * @param operator
     */
    public void setOperator(int operator) {
        setProperty(TeiidSqlLexicon.CompoundCriteria.OPERATOR_PROP_NAME, operator);
    }

    @Override
    public List<Criteria> getCriteria() {
        return getChildrenforIdentifierAndRefType(
                                               TeiidSqlLexicon.CompoundCriteria.CRITERIA_REF_NAME, Criteria.class);
    }

    @Override
    public int getCriteriaCount() {
        return getCriteria().size();
    }

    @Override
    public Criteria getCriteria(int index) {
        List<Criteria> criteria = getCriteria();
        if (index >= criteria.size() || index < 0)
            return null;

        return criteria.get(index);
    }

    @Override
    public void addCriteria(Criteria criteria) {
        addLastChild(TeiidSqlLexicon.CompoundCriteria.CRITERIA_REF_NAME, criteria);
    }

    /**
     * @param criteria
     */
    public void setCriteria(List<Criteria> criteria) {
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
        CompoundCriteria other = (CompoundCriteria)obj;
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
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public CompoundCriteria clone() {
        CompoundCriteria clone = new CompoundCriteria(this.getTeiidParser(), this.getId());

        clone.setOperator(getOperator());
        if (getCriteria() != null)
            clone.setCriteria(cloneList(getCriteria()));

        return clone;
    }

}
