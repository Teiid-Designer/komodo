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

package org.komodo.modeshape.teiid.sql.proc;

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.query.sql.proc.DeclareStatement;

/**
 *
 */
public class DeclareStatementImpl extends AssignmentStatementImpl implements DeclareStatement<BaseExpression, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public DeclareStatementImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public String getVariableType() {
        Object property = getProperty(TeiidSqlLexicon.DeclareStatement.VARIABLE_TYPE_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setVariableType(String type) {
        setProperty(TeiidSqlLexicon.DeclareStatement.VARIABLE_TYPE_PROP_NAME, type);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getVariableType() == null) ? 0 : this.getVariableType().hashCode());
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
        DeclareStatementImpl other = (DeclareStatementImpl)obj;
        if (this.getVariableType() == null) {
            if (other.getVariableType() != null)
                return false;
        } else if (!this.getVariableType().equals(other.getVariableType()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public DeclareStatementImpl clone() {
        DeclareStatementImpl clone = new DeclareStatementImpl(this.getTeiidParser(), this.getId());

        if (getVariableType() != null)
            clone.setVariableType(getVariableType());
        if (getExpression() != null)
            clone.setExpression(getExpression().clone());
        if (getCommand() != null)
            clone.setCommand(getCommand().clone());
        if (getVariable() != null)
            clone.setVariable(getVariable().clone());
        if (getValue() != null)
            clone.setValue(getValue().clone());

        return clone;
    }

}
