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

import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.lang.QueryCommand;
import org.komodo.modeshape.teiid.sql.lang.SubqueryContainer;
import org.komodo.spi.query.sql.symbol.IScalarSubquery;

public class ScalarSubquery extends ASTNode implements Expression, SubqueryContainer<QueryCommand>, IScalarSubquery<LanguageVisitor, QueryCommand> {

    public ScalarSubquery(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public <T> Class<T> getType() {
        return null;
    }

    /**
     * @param type
     */
    public void setType(Class<Object> type) {
    }

    @Override
    public QueryCommand getCommand() {
        return null;
    }

    @Override
    public void setCommand(QueryCommand command) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCommand() == null) ? 0 : this.getCommand().hashCode());
        result = prime * result + ((this.getType() == null) ? 0 : this.getType().hashCode());
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
        ScalarSubquery other = (ScalarSubquery)obj;
        if (this.getCommand() == null) {
            if (other.getCommand() != null)
                return false;
        } else if (!this.getCommand().equals(other.getCommand()))
            return false;
        if (this.getType() == null) {
            if (other.getType() != null)
                return false;
        } else if (!this.getType().equals(other.getType()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public ScalarSubquery clone() {
        ScalarSubquery clone = new ScalarSubquery(this.getTeiidParser(), this.getId());

        if (getType() != null)
            clone.setType(getType());
        if (getCommand() != null)
            clone.setCommand(getCommand().clone());

        return clone;
    }

}
