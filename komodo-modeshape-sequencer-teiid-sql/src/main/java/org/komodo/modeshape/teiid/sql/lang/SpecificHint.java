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

import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.spi.lexicon.TeiidSqlLexicon;

/**
 *
 */
public class SpecificHint extends ASTNode {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public SpecificHint(TeiidSeqParser p, int id) {
        super(p, id);
    }

    public String getHint() {
        Object property = getProperty(TeiidSqlLexicon.SpecificHint.HINT_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setHint(String hint) {
        setProperty(TeiidSqlLexicon.SpecificHint.HINT_PROP_NAME, hint);
    }

    public boolean isUseAliases() {
        Object property = getProperty(TeiidSqlLexicon.SpecificHint.USE_ALIASES_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setUseAliases(boolean useAliases) {
        setProperty(TeiidSqlLexicon.SpecificHint.USE_ALIASES_PROP_NAME, useAliases);
    }

    public String getTranslatorName() {
        Object property = getProperty(TeiidSqlLexicon.SpecificHint.TRANSLATOR_NAME_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setTranslatorName(String translatorName) {
        setProperty(TeiidSqlLexicon.SpecificHint.TRANSLATOR_NAME_PROP_NAME, translatorName);
    }

    @Override
    public SpecificHint clone() {
        SpecificHint clone = new SpecificHint(this.getTeiidParser(), this.getId());
        clone.setHint(getHint());
        clone.setTranslatorName(getTranslatorName());
        clone.setUseAliases(isUseAliases());
        return clone;
    }
}