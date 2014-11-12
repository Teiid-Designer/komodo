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
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbolImpl;
import org.komodo.spi.query.sql.lang.ProjectedColumn;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;

/**
 *
 */
public class ProjectedColumnImpl extends ASTNode implements ProjectedColumn<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public ProjectedColumnImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    public String getName() {
        Object property = getProperty(TeiidSqlLexicon.ProjectedColumn.NAME_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setName(String name) {
        setProperty(TeiidSqlLexicon.ProjectedColumn.NAME_PROP_NAME, name);
    }

    public ElementSymbolImpl getSymbol() {
        ElementSymbolImpl symbol = getTeiidParser().createASTNode(ASTNodes.ELEMENT_SYMBOL);
        symbol.setName(getName());
        Class<?> typeClass = getTeiidParser().getDataTypeService().getDataTypeClass(getType());
        symbol.setType(typeClass);

        return symbol;
    }

    public String getType() {
        Object property = getProperty(TeiidSqlLexicon.ProjectedColumn.TYPE_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setType(String type) {
        setProperty(TeiidSqlLexicon.ProjectedColumn.TYPE_PROP_NAME, type);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getName() == null) ? 0 : this.getName().hashCode());
        result = prime * result + ((this.getType() == null) ? 0 : this.getType().hashCode());
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
        ProjectedColumnImpl other = (ProjectedColumnImpl)obj;
        if (this.getName() == null) {
            if (other.getName() != null)
                return false;
        } else if (!this.getName().equals(other.getName()))
            return false;
        if (this.getType() == null) {
            if (other.getType() != null)
                return false;
        } else if (!this.getType().equals(other.getType()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public ProjectedColumnImpl clone() {
        ProjectedColumnImpl clone = new ProjectedColumnImpl(this.getTeiidParser(), this.getId());

        if (getName() != null)
            clone.setName(getName());
        if (getType() != null)
            clone.setType(getType());

        return clone;
    }

}
