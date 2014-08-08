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
import org.komodo.modeshape.teiid.sql.lang.Labeled;
import org.komodo.modeshape.teiid.sql.lang.SubqueryContainer;
import org.komodo.spi.query.sql.proc.ILoopStatement;

public class LoopStatement extends Statement implements Labeled, SubqueryContainer<Command>, ILoopStatement<LanguageVisitor, Command> {

    public LoopStatement(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * Return the type for this statement, this is one of the types
     * defined on the statement object.
     * @return The statement type
     */
    @Override
    public StatementType getType() {
        return StatementType.TYPE_LOOP;
    }

    @Override
    public Command getCommand() {
        return null;
    }

    @Override
    public void setCommand(Command command) {
    }

    @Override
    public String getLabel() {
        return null;
    }

    @Override
    public void setLabel(String label) {
    }

    /**
     * @return
     */
    private Block getBlock() {
        return null;
    }

    /**
     * @param asBlock
     */
    public void setBlock(Block asBlock) {
    }

    /**
     * @return
     */
    private String getCursorName() {
        return null;
    }

    /**
     * @param cursor
     */
    public void setCursorName(String cursor) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCursorName() == null) ? 0 : this.getCursorName().hashCode());
        result = prime * result + ((this.getLabel() == null) ? 0 : this.getLabel().hashCode());
        result = prime * result + ((this.getBlock() == null) ? 0 : this.getBlock().hashCode());
        result = prime * result + ((this.getCommand() == null) ? 0 : this.getCommand().hashCode());
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
        LoopStatement other = (LoopStatement)obj;
        if (this.getCursorName() == null) {
            if (other.getCursorName() != null)
                return false;
        } else if (!this.getCursorName().equals(other.getCursorName()))
            return false;
        if (this.getLabel() == null) {
            if (other.getLabel() != null)
                return false;
        } else if (!this.getLabel().equals(other.getLabel()))
            return false;
        if (this.getBlock() == null) {
            if (other.getBlock() != null)
                return false;
        } else if (!this.getBlock().equals(other.getBlock()))
            return false;
        if (this.getCommand() == null) {
            if (other.getCommand() != null)
                return false;
        } else if (!this.getCommand().equals(other.getCommand()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public LoopStatement clone() {
        LoopStatement clone = new LoopStatement(this.getTeiidParser(), this.getId());

        if (getLabel() != null)
            clone.setLabel(getLabel());
        if (getCommand() != null)
            clone.setCommand(getCommand().clone());
        if (getBlock() != null)
            clone.setBlock(getBlock().clone());
        if (getCursorName() != null)
            clone.setCursorName(getCursorName());

        return clone;
    }

}
