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

import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.Command;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.proc.IAssignmentStatement;

public class AssignmentStatement extends Statement implements ExpressionStatement, IAssignmentStatement<Expression, LanguageVisitor> {

    public AssignmentStatement(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * Return the type for this statement, this is one of the types
     * defined on the statement object.
     * @return The type of this statement
     */
    @Override
    public StatementType getType() {
        return StatementType.TYPE_ASSIGNMENT;
    }

    @Override
    public ElementSymbol getVariable() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param elementSymbol
     */
    public void setVariable(ElementSymbol elementSymbol) {
    }

    @Override
    public Expression getValue() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setValue(Expression value) {
    }

    @Override
    public Class<?> getExpectedType() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return
     */
    public Command getCommand() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param value
     */
    public void setCommand(Command value) {
    }

    @Override
    public Expression getExpression() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setExpression(Expression expr) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCommand() == null) ? 0 : this.getCommand().hashCode());
        result = prime * result + ((this.getValue() == null) ? 0 : this.getValue().hashCode());
        result = prime * result + ((this.getVariable() == null) ? 0 : this.getVariable().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        AssignmentStatement other = (AssignmentStatement)obj;
        if (this.getCommand() == null) {
            if (other.getCommand() != null) return false;
        } else if (!this.getCommand().equals(other.getCommand())) return false;
        if (this.getValue() == null) {
            if (other.getValue() != null) return false;
        } else if (!this.getValue().equals(other.getValue())) return false;
        if (this.getVariable() == null) {
            if (other.getVariable() != null) return false;
        } else if (!this.getVariable().equals(other.getVariable())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public AssignmentStatement clone() {
        AssignmentStatement clone = new AssignmentStatement(this.getTeiidParser(), this.getId());
    
        if(getExpression() != null)
            clone.setExpression(getExpression().clone());
        if(getCommand() != null)
            clone.setCommand(getCommand().clone());
        if(getVariable() != null)
            clone.setVariable(getVariable().clone());
        if(getValue() != null)
            clone.setValue(getValue().clone());
    
        return clone;
    }

}
