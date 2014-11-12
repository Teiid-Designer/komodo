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
import org.komodo.spi.query.sql.symbol.GroupSymbol;

/**
 *
 */
public class GroupSymbolImpl extends SymbolImpl implements GroupSymbol<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public GroupSymbolImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public String getDefinition() {
        Object property = getProperty(TeiidSqlLexicon.GroupSymbol.DEFINITION_PROP_NAME);
        return property == null ? null : property.toString();
    }

    @Override
    public void setDefinition(String definition) {
        setProperty(TeiidSqlLexicon.GroupSymbol.DEFINITION_PROP_NAME, definition);
    }

    @Override
    public boolean isProcedure() {
        Object property = getProperty(TeiidSqlLexicon.GroupSymbol.PROCEDURE_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setProcedure(boolean procedure) {
        setProperty(TeiidSqlLexicon.GroupSymbol.PROCEDURE_PROP_NAME, procedure);
    }

    @Override
    public Object getMetadataID() {
        Object property = getProperty(TeiidSqlLexicon.GroupSymbol.METADATAID_PROP_NAME);
        return property == null ? null : property.toString();
    }

    @Override
    public void setMetadataID(Object metadataID) {
        setProperty(TeiidSqlLexicon.GroupSymbol.METADATAID_PROP_NAME, metadataID);
    }

    @Override
    public int hashCode() {
//        final int prime = 31;
//        int result = 1;
//        if (this.getSchema() != null) {
//            result = prime * result + this.getSchema().hashCode();
//            result = prime * result + ((this.getShortCanonicalName() == null) ? 0 : this.getShortCanonicalName().hashCode());
//    
//            return result;
//        }
    
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
        GroupSymbolImpl other = (GroupSymbolImpl)obj;
//        if (this.getSchema() == null || other.getSchema() == null) {
//                return this.getName().equals(other.getName());
//        }
//        
//        if (this.getSchema() == null) {
//            if (other.getSchema() != null)
//                return false;
//        } else if (!this.getSchema().equals(other.getSchema()))
//            return false;
    
        return this.getShortName().equals(other.getShortName());
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public GroupSymbolImpl clone() {
        GroupSymbolImpl clone = new GroupSymbolImpl(this.getTeiidParser(), this.getId());

        if (getDefinition() != null)
            clone.setDefinition(getDefinition());
//        if (getOutputDefinition() != null)
//            clone.setOutputDefinition(getOutputDefinition());
        if (getOutputName() != null)
            clone.setOutputName(getOutputName());
        if (getShortName() != null)
            clone.setShortName(getShortName());
        if (getName() != null)
            clone.setName(getName());
        if (getMetadataID() != null)
            clone.setMetadataID(getMetadataID());

//        clone.setIsTempTable(isTempTable);
//        clone.setProcedure(isProcedure);

        return clone;
    }

}
