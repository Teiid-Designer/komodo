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
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.Command;
import org.komodo.modeshape.teiid.sql.lang.SubqueryContainer;
import org.komodo.spi.query.sql.proc.ICommandStatement;

public class CommandStatement extends Statement implements SubqueryContainer<Command>, ICommandStatement<LanguageVisitor, Command> {

    public CommandStatement(TeiidParser p, int id) {
        super(p, id);
        setType(StatementType.TYPE_COMMAND);
        setReturnable(true);
    }

    public boolean isReturnable() {
        Object property = getProperty(TeiidSqlLexicon.CommandStatement.RETURNABLE_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setReturnable(boolean returnable) {
        setProperty(TeiidSqlLexicon.CommandStatement.RETURNABLE_PROP_NAME, returnable);
    }

    @Override
    public Command getCommand() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.SubqueryContainer.COMMAND_REF_NAME, Command.class);
    }

    @Override
    public void setCommand(Command command) {
        setChild(TeiidSqlLexicon.SubqueryContainer.COMMAND_REF_NAME, command);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCommand() == null) ? 0 : this.getCommand().hashCode());
        result = prime * result + (this.isReturnable() ? 1231 : 1237);
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
        CommandStatement other = (CommandStatement)obj;
        if (this.getCommand() == null) {
            if (other.getCommand() != null)
                return false;
        } else if (!this.getCommand().equals(other.getCommand()))
            return false;
        if (this.isReturnable() != other.isReturnable())
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public CommandStatement clone() {
        CommandStatement clone = new CommandStatement(this.getTeiidParser(), this.getId());

        clone.setReturnable(isReturnable());
        if (getCommand() != null)
            clone.setCommand(getCommand().clone());

        return clone;
    }

}
