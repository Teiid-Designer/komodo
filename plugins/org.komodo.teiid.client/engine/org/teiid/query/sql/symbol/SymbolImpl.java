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
package org.teiid.query.sql.symbol;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.query.sql.symbol.Symbol;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.parser.TeiidClientParser;
import org.teiid.query.sql.lang.SimpleNode;
import org.teiid.runtime.client.Messages;


/**
 *
 */
public class SymbolImpl extends SimpleNode implements Symbol<TCLanguageVisitorImpl> {

    /**
     * Character used to delimit name components in a symbol
     */
    public static final String SEPARATOR = "."; //$NON-NLS-1$

    private String shortName;

    /**
     * Prior to resolving null, after resolving it is the exact string
     * entered in the query.
     *
     * The AliasGenerator can also set this value as necessary for the data tier.
     */
    protected String outputName;

    /** 
     * upper case of name
     */
    @Removed(Version.TEIID_8_0)
    private String canonicalShortName;

    /**
     * @param ex
     * @return short name of given expression if a symbol
     */
    public static String getShortName(BaseExpression ex) {
        if (ex instanceof SymbolImpl) {
            return ((SymbolImpl)ex).getShortName();
        }
        return "expr"; //$NON-NLS-1$
    }

    /**
     * @param name
     * @return shortname of string using {@link SymbolImpl#SEPARATOR} as delimiter
     */
    public static String getShortName(String name) {
        int index = name.lastIndexOf(SymbolImpl.SEPARATOR);
        if(index >= 0) {
            return name.substring(index+1);
        }
        return name;
    }

    /**
     * @param p
     * @param i
     */
    public SymbolImpl(TeiidClientParser p, int i) {
        super(p, i);
    }

    /**
     * @param name
     */
    public void setName(String name) {
        setShortName(name);
    }

    /**
     * Get the short name of the element
     *
     * @return Short name of the symbol (un-dotted)
     */
    @Override
    public final String getShortName() { 
        return shortName;
    }

    /**
     * Change the symbol's name.  This will change the symbol's hash code
     * and canonical name!!!!!!!!!!!!!!!!!  If this symbol is in a hashed
     * collection, it will be lost!
     *
     * @param name New name
     */
    @Override
    public void setShortName(String name) {
        if(name == null) {
            throw new IllegalArgumentException(Messages.getString(Messages.ERR.ERR_015_010_0017));
        }
        this.shortName = name;
        this.outputName = null;
    }

    /**
     * Get the name of the symbol
     * @return Name of the symbol, never null
     */
    @Override
    public String getName() {
        return getShortName();
    }

    /**
     * @return the canonicalShortName
     */
    @Removed(Version.TEIID_8_0)
    public String getShortCanonicalName() {
        if (canonicalShortName == null && shortName != null) {
            canonicalShortName = shortName.toUpperCase();
        }
        return this.canonicalShortName;
    }

    /**
     * @param canonicalShortName the canonicalShortName to set
     */
    @Removed(Version.TEIID_8_0)
    public void setShortCanonicalName(String canonicalShortName) {
        this.canonicalShortName = canonicalShortName;
    }

    @Override
    public String getOutputName() {
        return this.outputName == null ? getName() : this.outputName;
    }

    @Override
    public void setOutputName(String outputName) {
        this.outputName = outputName;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        if (getTeiidVersion().isLessThan(Version.TEIID_8_0.get()))
            result = prime * result + ((this.getShortCanonicalName() == null) ? 0 : this.getShortCanonicalName().hashCode());
        else
            result = prime * result + ((this.getShortName() == null) ? 0 : this.getShortName().hashCode());

        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        SymbolImpl other = (SymbolImpl)obj;
        if (this.getShortCanonicalName() == null) {
            if (other.getShortCanonicalName() != null) return false;
        } else if (!this.getShortCanonicalName().equalsIgnoreCase(other.getShortCanonicalName())) return false;
        if (this.getName() == null) {
            if (other.getName() != null) return false;
        } else if (!this.getName().equalsIgnoreCase(other.getName())) return false;
        return true;
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(TCLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public SymbolImpl clone() {
        SymbolImpl clone = new SymbolImpl(this.parser, this.id);

        if(getShortCanonicalName() != null)
            clone.setShortCanonicalName(getShortCanonicalName());
        if(outputName != null)
            clone.outputName = outputName;
        if(getShortName() != null)
            clone.setShortName(getShortName());
        if(getName() != null)
            clone.setName(getName());

        return clone;
    }

}
