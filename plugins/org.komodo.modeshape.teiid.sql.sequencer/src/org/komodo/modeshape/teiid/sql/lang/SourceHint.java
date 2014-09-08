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

import java.util.Collection;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.spi.query.sql.lang.ISourceHint;

/**
 *
 */
public class SourceHint extends ASTNode implements ISourceHint {

    /**
     * @param p
     * @param id
     */
    public SourceHint(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * @return general hint
     */
    public String getGeneralHint() {
        Object property = getProperty(TeiidSqlLexicon.SourceHint.GENERAL_HINT_PROP_NAME);
        return property == null ? null : property.toString();
    }

    /**
     * @param generalHint
     */
    public void setGeneralHint(String generalHint) {
        setProperty(TeiidSqlLexicon.SourceHint.GENERAL_HINT_PROP_NAME, generalHint);
    }

    public Collection<SpecificHint> getSourceHints() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.SourceHint.SOURCE_HINTS_REF_NAME, SpecificHint.class);
    }

    public void addSourceHint(SpecificHint specificHint) {
        addLastChild(TeiidSqlLexicon.SourceHint.SOURCE_HINTS_REF_NAME, specificHint);
    }

    public void setSourceHints(Collection<SpecificHint> specificHint) {
        setChildren(TeiidSqlLexicon.SourceHint.SOURCE_HINTS_REF_NAME, specificHint);
    }

    /**
     * @return use aliases flag
     */
    public boolean isUseAliases() {
        Object property = getProperty(TeiidSqlLexicon.SourceHint.USE_ALIASES_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    /**
     * @param useAliases
     */
    public void setUseAliases(boolean useAliases) {
        setProperty(TeiidSqlLexicon.SourceHint.USE_ALIASES_PROP_NAME, useAliases);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.getGeneralHint() == null) ? 0 : this.getGeneralHint().hashCode());
        result = prime * result + ((this.getSourceHints() == null) ? 0 : this.getSourceHints().hashCode());
        result = prime * result + (this.isUseAliases() ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SourceHint other = (SourceHint)obj;
        if (this.getGeneralHint() == null) {
            if (other.getGeneralHint() != null)
                return false;
        } else if (!this.getGeneralHint().equals(other.getGeneralHint()))
            return false;
        if (this.getSourceHints() == null) {
            if (other.getSourceHints() != null)
                return false;
        } else if (!this.getSourceHints().equals(other.getSourceHints()))
            return false;
        if (this.isUseAliases() != other.isUseAliases())
            return false;
        return true;
    }

    @Override
    public SourceHint clone() {
        SourceHint clone = new SourceHint(this.getTeiidParser(), this.getId());

        if(getGeneralHint() != null)
            clone.setGeneralHint(getGeneralHint());

        if (getSourceHints() != null) {
            clone.setSourceHints(cloneCollection(getSourceHints()));
        }

        clone.setUseAliases(isUseAliases());

        return clone;
    }
}
