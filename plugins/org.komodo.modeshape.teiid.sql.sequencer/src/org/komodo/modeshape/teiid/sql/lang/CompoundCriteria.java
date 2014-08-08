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
import org.komodo.spi.query.sql.lang.ICompoundCriteria;

/**
 *
 */
public class CompoundCriteria extends Criteria implements ICompoundCriteria<Criteria, LanguageVisitor> {

    /**
     * @param p
     * @param id
     */
    public CompoundCriteria(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public int getOperator() {
        return 0;
    }

    /**
     * @param or
     */
    public void setOperator(int or) {
    }

    @Override
    public List<Criteria> getCriteria() {
        return null;
    }

    @Override
    public int getCriteriaCount() {
        return 0;
    }

    @Override
    public Criteria getCriteria(int index) {
        return null;
    }

    @Override
    public void addCriteria(Criteria criteria) {
    }

    /**
     * @param criteria
     */
    public void setCriteria(List<Criteria> criteria) {
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
