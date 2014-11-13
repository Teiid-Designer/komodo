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
package org.teiid.query.sql.lang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.komodo.spi.query.sql.lang.TableFunctionReference;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.parser.TeiidClientParser;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.util.SymbolMap;

/**
 *
 */
public abstract class TableFunctionReferenceImpl extends FromClauseImpl
    implements TableFunctionReference<TCLanguageVisitorImpl> {

    private GroupSymbolImpl symbol;

    private SymbolMap correlatedReferences;

    /**
     * @param p
     * @param id
     */
    public TableFunctionReferenceImpl(TeiidClientParser p, int id) {
        super(p, id);
    }

    /**
     * @return correlated references
     */
    public SymbolMap getCorrelatedReferences() {
        return correlatedReferences;
    }
    
    /**
     * @param correlatedReferences
     */
    public void setCorrelatedReferences(SymbolMap correlatedReferences) {
        this.correlatedReferences = correlatedReferences;
    }
    
    /**
     * @param copy
     */
    public void copy(TableFunctionReferenceImpl copy) {
        copy.symbol = this.symbol.clone();
        if (correlatedReferences != null) {
            copy.correlatedReferences = correlatedReferences.clone();
        }
    }

    @Override
    public void collectGroups(Collection<GroupSymbolImpl> groups) {
        groups.add(getGroupSymbol());
    }

    /**
     * Get name of this clause.
     * @return Name of clause
     */
    public String getName() {
        return this.symbol.getName();   
    }
    
    /**
     * @return output name
     */
    public String getOutputName() {
        return this.symbol.getOutputName();
    }

    /**
     * Get GroupSymbol representing the named subquery 
     * @return GroupSymbol representing the subquery
     */
    public GroupSymbolImpl getGroupSymbol() {
        return this.symbol;    
    }
    
    /** 
     * Reset the alias for this subquery from clause and it's pseudo-GroupSymbol.  
     * WARNING: this will modify the hashCode and equals semantics and will cause this object
     * to be lost if currently in a HashMap or HashSet.
     * @param name New name
     *
     */
    public void setName(String name) {
        this.symbol = this.parser.createASTNode(ASTNodes.GROUP_SYMBOL);
        this.symbol.setName(name);
    }

    /**
     * @return projected columns
     */
    public abstract List<? extends ProjectedColumnImpl> getColumns();

    /**
     * @return calculated projected symbols
     */
    public List<ElementSymbolImpl> getProjectedSymbols() {
        ArrayList<ElementSymbolImpl> symbols = new ArrayList<ElementSymbolImpl>(getColumns().size());
        for (ProjectedColumnImpl col : getColumns()) {
            symbols.add(col.getSymbol());
        }
        return symbols;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.correlatedReferences == null) ? 0 : this.correlatedReferences.hashCode());
        result = prime * result + ((this.symbol == null) ? 0 : this.symbol.hashCode());
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
        TableFunctionReferenceImpl other = (TableFunctionReferenceImpl)obj;
        if (this.correlatedReferences == null) {
            if (other.correlatedReferences != null)
                return false;
        } else if (!this.correlatedReferences.equals(other.correlatedReferences))
            return false;
        if (this.symbol == null) {
            if (other.symbol != null)
                return false;
        } else if (!this.symbol.equals(other.symbol))
            return false;
        return true;
    }
}
