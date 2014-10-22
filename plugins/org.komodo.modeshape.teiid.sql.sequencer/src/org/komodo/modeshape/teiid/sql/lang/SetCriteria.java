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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ISetCriteria;

/**
 *
 */
public class SetCriteria extends AbstractSetCriteria implements ISetCriteria<Expression, LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public SetCriteria(ITeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public Collection<Expression> getValues() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.SetCriteria.VALUES_REF_NAME, Expression.class);
    }

    @Override
    public void setValues(Collection<Expression> values) {
        setChildren(TeiidSqlLexicon.SetCriteria.VALUES_REF_NAME, values);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getValues() == null) ? 0 : this.getValues().hashCode());
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
        SetCriteria other = (SetCriteria)obj;
        if (this.getValues() == null) {
            if (other.getValues() != null)
                return false;
        } else if (this.getValues().size() != other.getValues().size() || (!this.getValues().containsAll(other.getValues())))
            return false;

        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public SetCriteria clone() {
        SetCriteria clone = new SetCriteria(this.getTeiidParser(), this.getId());

        if (getValues() != null)
            clone.setValues(cloneCollection(getValues()));
        if (getExpression() != null)
            clone.setExpression(getExpression().clone());
        clone.setNegated(isNegated());

        return clone;
    }

}
