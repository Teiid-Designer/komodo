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

package org.komodo.modeshape.teiid.sql.symbol;

import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.symbol.IXMLForest;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
public class XMLForest extends ASTNode implements Expression, IXMLForest<LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public XMLForest(TeiidSeqParser p, int id) {
        super(p, id);
        setProperty(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME, DataTypeName.XML.name());
    }

    @Override
    public Class<?> getType() {
        return convertTypeClassPropertyToClass(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME);
    }

    public List<DerivedColumn> getArguments() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.XMLForest.ARGUMENTS_REF_NAME, DerivedColumn.class);
    }

    public void setArguments(List<DerivedColumn> args) {
        setChildren(TeiidSqlLexicon.XMLForest.ARGUMENTS_REF_NAME, args);
    }

    public XMLNamespaces getNamespaces() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.XMLForest.NAMESPACES_REF_NAME, XMLNamespaces.class);
    }

    public void setNamespaces(XMLNamespaces xmlNamespaces) {
        setChild(TeiidSqlLexicon.XMLForest.NAMESPACES_REF_NAME, xmlNamespaces);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getArguments() == null) ? 0 : this.getArguments().hashCode());
        result = prime * result + ((this.getNamespaces() == null) ? 0 : this.getNamespaces().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        XMLForest other = (XMLForest)obj;
        if (this.getArguments() == null) {
            if (other.getArguments() != null) return false;
        } else if (!this.getArguments().equals(other.getArguments())) return false;
        if (this.getNamespaces() == null) {
            if (other.getNamespaces() != null) return false;
        } else if (!this.getNamespaces().equals(other.getNamespaces())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public XMLForest clone() {
        XMLForest clone = new XMLForest(this.getTeiidParser(), this.getId());

        if(getNamespaces() != null)
            clone.setNamespaces(getNamespaces().clone());
        if(getArguments() != null)
            clone.setArguments(cloneList(getArguments()));

        return clone;
    }

}
