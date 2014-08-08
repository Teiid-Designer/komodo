/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.komodo.modeshape.teiid.sql.lang;

import java.util.Collection;
import java.util.List;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.lang.ITableFunctionReference;

/**
 *
 */
public abstract class TableFunctionReference extends FromClause implements ITableFunctionReference<LanguageVisitor> {

    /**
     * @param p
     * @param id
     */
    public TableFunctionReference(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * @param copy
     */
    public void copy(TableFunctionReference copy) {

    }

    @Override
    public void collectGroups(Collection<GroupSymbol> groups) {

    }

    /**
     * Get name of this clause.
     * @return Name of clause
     */
    @Override
    public String getName() {
        return null;

    }

    /** 
     * Reset the alias for this subquery from clause and it's pseudo-GroupSymbol.  
     * WARNING: this will modify the hashCode and equals semantics and will cause this object
     * to be lost if currently in a HashMap or HashSet.
     * @param name New name
     *
     */
    @Override
    public void setName(String name) {
    }

    /**
     * @return output name
     */
    public String getOutputName() {
        return null;
    }

    /**
     * Get GroupSymbol representing the named subquery 
     * @return GroupSymbol representing the subquery
     */
    public GroupSymbol getGroupSymbol() {
        return null;
    }

    /**
     * @return projected columns
     */
    public abstract List<? extends ProjectedColumn> getColumns();

    /**
     * @return calculated projected symbols
     */
    public List<ElementSymbol> getProjectedSymbols() {
        return null;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
//        result = prime * result + ((this.getCorrelatedReferences() == null) ? 0 : this.getCorrelatedReferences().hashCode());
        result = prime * result + ((this.getGroupSymbol() == null) ? 0 : this.getGroupSymbol().hashCode());
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
        TableFunctionReference other = (TableFunctionReference)obj;
//        if (this.getCorrelatedReferences() == null) {
//            if (other.getCorrelatedReferences() != null)
//                return false;
//        } else if (!this.getCorrelatedReferences().equals(other.getCorrelatedReferences()))
//            return false;
        if (this.getGroupSymbol() == null) {
            if (other.getGroupSymbol() != null)
                return false;
        } else if (!this.getGroupSymbol().equals(other.getGroupSymbol()))
            return false;
        return true;
    }
}
