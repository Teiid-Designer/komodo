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

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.spi.query.sql.symbol.IElementSymbol;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
public class ElementSymbol extends Symbol implements Expression, IElementSymbol<GroupSymbol, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public ElementSymbol(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public Class<?> getType() {
        return convertTypeClassPropertyToClass(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME);
    }

    @Override
    public void setType(Class<?> type) {
        DataTypeName dataTypeName = getDataTypeService().retrieveDataTypeName(type);
        setProperty(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME, dataTypeName.name());
    }

    @Override
    public GroupSymbol getGroupSymbol() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.ElementSymbol.GROUP_SYMBOL_REF_NAME, GroupSymbol.class);
    }

    @Override
    public void setGroupSymbol(GroupSymbol groupSymbol) {
        setChild(TeiidSqlLexicon.ElementSymbol.GROUP_SYMBOL_REF_NAME, groupSymbol);
    }

    @Override
    public boolean isExternalReference() {
        Object property = getProperty(TeiidSqlLexicon.ElementSymbol.EXTERNAL_REFERENCE_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setExternalReference(boolean externalReference) {
        setProperty(TeiidSqlLexicon.ElementSymbol.EXTERNAL_REFERENCE_PROP_NAME, externalReference);
    }

    @Override
    public ElementSymbol.DisplayMode getDisplayMode() {
        Object property = getProperty(TeiidSqlLexicon.ElementSymbol.DISPLAY_MODE_PROP_NAME);
        return property == null ? DisplayMode.OUTPUT_NAME : DisplayMode.findDisplayMode(property.toString());
    }

    @Override
    public void setDisplayMode(DisplayMode displayMode) {
        setProperty(TeiidSqlLexicon.ElementSymbol.DISPLAY_MODE_PROP_NAME, displayMode.name());
        setProperty(
                    TeiidSqlLexicon.ElementSymbol.DISPLAY_FULLY_QUALIFIED_PROP_NAME,
                    DisplayMode.FULLY_QUALIFIED.equals(displayMode));
    }

    public boolean isDisplayFullyQualified() {
        Object property = getProperty(TeiidSqlLexicon.ElementSymbol.DISPLAY_FULLY_QUALIFIED_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    @Override
    public void setDisplayFullyQualified(boolean displayFullyQualified) {
        setDisplayMode(displayFullyQualified ? DisplayMode.FULLY_QUALIFIED : DisplayMode.SHORT_OUTPUT_NAME);
    }

    @Override
    public Object getMetadataID() {
        Object property = getProperty(TeiidSqlLexicon.ElementSymbol.METADATAID_PROP_NAME);
        return property == null ? null : property;
    }

    @Override
    public void setMetadataID(Object metadataID) {
        setProperty(TeiidSqlLexicon.ElementSymbol.METADATAID_PROP_NAME, metadataID);
    }

    @Override
    public int hashCode() {
        super.hashCode();
    
        if (this.getGroupSymbol() != null) {
            final int prime = 31;
            int result = 1;
    
            result = prime * result + this.getGroupSymbol().hashCode();
            result = prime * result + (this.getShortName() == null ? 0 : this.getShortName().hashCode());
    
            return result;
        }
    
        return super.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        ElementSymbol other = (ElementSymbol)obj;
    
        if (this.getGroupSymbol() == null) {
            return super.equals(obj);
        }
    
        if (this.getType() == null) {
            if (other.getType() != null)
                return false;
        } else if (!this.getType().equals(other.getType()))
            return false;
    
        return this.getGroupSymbol().equals(other.getGroupSymbol()) && this.getGroupSymbol().equals(other.getGroupSymbol());
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public ElementSymbol clone() {
        ElementSymbol clone = new ElementSymbol(this.getTeiidParser(), this.getId());

        if (getGroupSymbol() != null)
            clone.setGroupSymbol(getGroupSymbol().clone());
        if (getType() != null)
            clone.setType(getType());
        if (getMetadataID() != null)
            clone.setMetadataID(getMetadataID());
        if (getDisplayMode() != null)
            clone.setDisplayMode(getDisplayMode());
        if (getShortName() != null)
            clone.setShortName(getShortName());
        if (getName() != null)
            clone.setName(getName());
        if (getOutputName() != null)
            clone.setOutputName(getOutputName());

        return clone;
    }

}
