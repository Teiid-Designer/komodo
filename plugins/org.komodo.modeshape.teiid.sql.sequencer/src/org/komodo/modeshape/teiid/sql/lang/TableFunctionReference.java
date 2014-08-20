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
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
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

    public String getName() {
        Object property = getProperty(TeiidSqlLexicon.TableFunctionReference.NAME_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setName(String name) {
        setProperty(TeiidSqlLexicon.TableFunctionReference.NAME_PROP_NAME, name);
    }

    @Override
    public void collectGroups(Collection<GroupSymbol> groups) {
        GroupSymbol symbol = getTeiidParser().createASTNode(ASTNodes.GROUP_SYMBOL);
        symbol.setName(getName());
        groups.add(symbol);
    }

    /**
     * @return projected columns
     */
    public abstract List<? extends ProjectedColumn> getColumns();

    /**
     * @return calculated projected symbols
     */
    public List<ElementSymbol> getProjectedSymbols() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
//        result = prime * result + ((this.getCorrelatedReferences() == null) ? 0 : this.getCorrelatedReferences().hashCode());
        result = prime * result + ((this.getName() == null) ? 0 : this.getName().hashCode());
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
        if (this.getName() == null) {
            if (other.getName() != null)
                return false;
        } else if (!this.getName().equals(other.getName()))
            return false;
        return true;
    }
}
