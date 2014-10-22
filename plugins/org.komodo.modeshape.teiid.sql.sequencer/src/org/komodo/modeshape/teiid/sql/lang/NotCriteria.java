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

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.spi.query.sql.lang.INotCriteria;

/**
 *
 */
public class NotCriteria extends Criteria implements INotCriteria<LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public NotCriteria(ITeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public Criteria getCriteria() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.NotCriteria.CRITERIA_REF_NAME, Criteria.class);
    }

    public void setCriteria(Criteria crit) {
        setChild(TeiidSqlLexicon.NotCriteria.CRITERIA_REF_NAME, crit);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCriteria() == null) ? 0 : this.getCriteria().hashCode());
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
        NotCriteria other = (NotCriteria)obj;
        if (this.getCriteria() == null) {
            if (other.getCriteria() != null)
                return false;
        } else if (!this.getCriteria().equals(other.getCriteria()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public NotCriteria clone() {
        NotCriteria clone = new NotCriteria(this.getTeiidParser(), this.getId());

        if (getCriteria() != null)
            clone.setCriteria(getCriteria().clone());

        return clone;
    }

}
