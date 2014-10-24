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
import org.komodo.modeshape.teiid.sql.symbol.DerivedColumn;
import org.komodo.modeshape.teiid.sql.symbol.XMLNamespaces;
import org.komodo.spi.query.sql.lang.IXMLTable;

/**
 *
 */
public class XMLTable extends TableFunctionReference implements IXMLTable<LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public XMLTable(ITeiidParser p, int id) {
        super(p, id);
    }

    public String getXquery() {
        Object property = getProperty(TeiidSqlLexicon.XMLTable.XQUERY_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setXquery(String xquery) {
        setProperty(TeiidSqlLexicon.XMLTable.XQUERY_PROP_NAME, xquery);
    }

    public XMLNamespaces getNamespaces() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.XMLTable.NAMESPACES_REF_NAME, XMLNamespaces.class);
    }

    public void setNamespaces(XMLNamespaces xmlNamespaces) {
        setChild(TeiidSqlLexicon.XMLTable.NAMESPACES_REF_NAME, xmlNamespaces);
    }

    public List<DerivedColumn> getPassing() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.XMLTable.PASSING_REF_NAME, DerivedColumn.class);
    }

    public void setPassing(List<DerivedColumn> passingValues) {
        setChildren(TeiidSqlLexicon.XMLTable.PASSING_REF_NAME, passingValues);
    }

    @Override
    public List<XMLColumn> getColumns() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.XMLTable.COLUMNS_REF_NAME, XMLColumn.class);
    }

    public void setColumns(List<XMLColumn> columns) {
        setChildren(TeiidSqlLexicon.XMLTable.COLUMNS_REF_NAME, columns);
    }

    public boolean isUsingDefaultColumn() {
        Object property = getProperty(TeiidSqlLexicon.XMLTable.USING_DEFAULT_COLUMN_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setUsingDefaultColumn(boolean usingDefaultColumn) {
        setProperty(TeiidSqlLexicon.XMLTable.USING_DEFAULT_COLUMN_PROP_NAME, usingDefaultColumn);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getColumns() == null) ? 0 : this.getColumns().hashCode());
        result = prime * result + ((this.getNamespaces() == null) ? 0 : this.getNamespaces().hashCode());
        result = prime * result + ((this.getPassing() == null) ? 0 : this.getPassing().hashCode());
        result = prime * result + (this.isUsingDefaultColumn() ? 1231 : 1237);
        result = prime * result + ((this.getXquery() == null) ? 0 : this.getXquery().hashCode());
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
        XMLTable other = (XMLTable)obj;
        if (this.getColumns() == null) {
            if (other.getColumns() != null)
                return false;
        } else if (!this.getColumns().equals(other.getColumns()))
            return false;
        if (this.getNamespaces() == null) {
            if (other.getNamespaces() != null)
                return false;
        } else if (!this.getNamespaces().equals(other.getNamespaces()))
            return false;
        if (this.getPassing() == null) {
            if (other.getPassing() != null)
                return false;
        } else if (!this.getPassing().equals(other.getPassing()))
            return false;
        if (this.isUsingDefaultColumn() != other.isUsingDefaultColumn())
            return false;
        if (this.getXquery() == null) {
            if (other.getXquery() != null)
                return false;
        } else if (!this.getXquery().equals(other.getXquery()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public XMLTable clone() {
        XMLTable clone = new XMLTable(this.getTeiidParser(), this.getId());

        if (getColumns() != null)
            clone.setColumns(cloneList(getColumns()));
        if (getPassing() != null)
            clone.setPassing(cloneList(getPassing()));
        if (getNamespaces() != null)
            clone.setNamespaces(getNamespaces().clone());
        if (getXquery() != null)
            clone.setXquery(getXquery());
        clone.setUsingDefaultColumn(isUsingDefaultColumn());
        if (getName() != null)
            clone.setName(getName());
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
