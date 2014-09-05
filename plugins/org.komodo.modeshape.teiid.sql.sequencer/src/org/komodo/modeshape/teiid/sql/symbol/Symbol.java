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
package org.komodo.modeshape.teiid.sql.symbol;

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.query.sql.symbol.ISymbol;


/**
 *
 */
public abstract class Symbol extends ASTNode implements ISymbol<LanguageVisitor> {

    /**
     * @param p
     * @param i
     */
    public Symbol(TeiidParser p, int i) {
        super(p, i);
    }

    private void assignName(String name) {
        // 2 properties used interchangeably
        setProperty(TeiidSqlLexicon.Symbol.NAME_PROP_NAME, name);
        setProperty(TeiidSqlLexicon.Symbol.SHORT_NAME_PROP_NAME, name);        
    }

    @Override
    public String getName() {
        Object property = getProperty(TeiidSqlLexicon.Symbol.NAME_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setName(String name) {
        assignName(name);
    }

    @Override
    public String getShortName() { 
        Object property = getProperty(TeiidSqlLexicon.Symbol.SHORT_NAME_PROP_NAME);
        return property == null ? null : property.toString();
    }

    @Override
    public void setShortName(String name) {
        assignName(name);
        setOutputName(null);
    }

    @Override
    public String getOutputName() {
        Object property = getProperty(TeiidSqlLexicon.Symbol.OUTPUT_NAME_PROP_NAME);
        return property == null ? null : property.toString();
    }

    @Override
    public void setOutputName(String outputName) {
        setProperty(TeiidSqlLexicon.Symbol.OUTPUT_NAME_PROP_NAME, outputName);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getShortName() == null) ? 0 : this.getShortName().hashCode());

        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        Symbol other = (Symbol)obj;

        if (this.getName() == null) {
            if (other.getName() != null) return false;
        } else if (!this.getName().equalsIgnoreCase(other.getName())) return false;
        return true;
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

}
