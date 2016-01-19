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

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;

/**
 *
 */
public class MakeDep extends ASTNode {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public MakeDep(TeiidSeqParser p, int id) {
        super(p, id);
    }

    public Integer getMax() {
        Object property = getProperty(TeiidSqlLexicon.MakeDep.MAX_PROP_NAME);
        return property == null ? -1 : Integer.parseInt(property.toString());
    }

    public void setMax(Integer max) {
        setProperty(TeiidSqlLexicon.MakeDep.MAX_PROP_NAME, max);
    }
    
    public boolean isJoin() {
        Object property = getProperty(TeiidSqlLexicon.MakeDep.JOIN_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }
    
    public void setJoin(boolean join) {
        setProperty(TeiidSqlLexicon.MakeDep.JOIN_PROP_NAME, join);
    }
    
    public boolean isSimple() {
        return getMax() == -1 && !isJoin();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + (this.isJoin() ? 1231 : 1237);
        result = prime * result + ((this.getMax() == -1) ? 0 : this.getMax().hashCode());
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
        MakeDep other = (MakeDep)obj;
        if (this.isJoin() != other.isJoin())
            return false;
        if (this.getMax() == null) {
            if (other.getMax() != null)
                return false;
        } else if (!this.getMax().equals(other.getMax()))
            return false;
        return true;
    }

    @Override
    public MakeDep clone() {
        MakeDep clone = new MakeDep(this.getTeiidParser(), this.getId());
        clone.setMax(getMax());
        clone.setJoin(isJoin());

        return clone;
    }
}
