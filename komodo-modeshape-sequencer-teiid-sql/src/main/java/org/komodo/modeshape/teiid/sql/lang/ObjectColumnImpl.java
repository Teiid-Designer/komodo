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

import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.lang.ObjectColumn;

/**
 *
 */
public class ObjectColumnImpl extends ProjectedColumnImpl implements ObjectColumn<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public ObjectColumnImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    public String getPath() {
        Object property = getProperty(TeiidSqlLexicon.ObjectColumn.PATH_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setPath(String path) {
        setProperty(TeiidSqlLexicon.ObjectColumn.PATH_PROP_NAME, path);
    }

    public BaseExpression getDefaultExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.ObjectColumn.DEFAULT_EXPRESSION_REF_NAME, BaseExpression.class);
    }

    public void setDefaultExpression(BaseExpression defaultExpr) {
        setChild(TeiidSqlLexicon.ObjectColumn.DEFAULT_EXPRESSION_REF_NAME, defaultExpr);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getDefaultExpression() == null) ? 0 : this.getDefaultExpression().hashCode());
        result = prime * result + ((this.getPath() == null) ? 0 : this.getPath().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        ObjectColumnImpl other = (ObjectColumnImpl)obj;
        if (this.getDefaultExpression() == null) {
            if (other.getDefaultExpression() != null) return false;
        } else if (!this.getDefaultExpression().equals(other.getDefaultExpression())) return false;
        if (this.getPath() == null) {
            if (other.getPath() != null) return false;
        } else if (!this.getPath().equals(other.getPath())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public ObjectColumnImpl clone() {
        ObjectColumnImpl clone = new ObjectColumnImpl(this.getTeiidParser(), this.getId());

        if(getPath() != null)
            clone.setPath(getPath());
        if(getDefaultExpression() != null)
            clone.setDefaultExpression(getDefaultExpression().clone());
        if(getType() != null)
            clone.setType(getType());
        if(getName() != null)
            clone.setName(getName());

        return clone;
    }

}
