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
import org.komodo.spi.query.sql.proc.IBranchingStatement;

public class BranchingStatement extends Statement implements IBranchingStatement<LanguageVisitor> {

    /**
     * Modes of branching
     */
    public enum BranchingMode {
        /**
         * Teiid specific - only allowed to target loops
         */
        BREAK,
        /**
         * Teiid specific - only allowed to target loops
         */
        CONTINUE,
        /**
         * ANSI - allowed to leave any block 
         */
        LEAVE
    }

    public BranchingStatement(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public StatementType getType() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return
     */
    public String getLabel() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param label
     */
    public void setLabel(String label) {
    }

    /**
     * @return
     */
    public BranchingMode getMode() {
        throw new UnsupportedOperationException();
    }

    /**
     * @param mode
     */
    public void setMode(BranchingMode mode) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getLabel() == null) ? 0 : this.getLabel().hashCode());
        result = prime * result + ((this.getMode() == null) ? 0 : this.getMode().hashCode());
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
        BranchingStatement other = (BranchingStatement)obj;
        if (this.getLabel() == null) {
            if (other.getLabel() != null)
                return false;
        } else if (!this.getLabel().equals(other.getLabel()))
            return false;
        if (this.getMode() != other.getMode())
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public BranchingStatement clone() {
        BranchingStatement clone = new BranchingStatement(this.getTeiidParser(), this.getId());

        if (getLabel() != null)
            clone.setLabel(getLabel());
        if (getMode() != null)
            clone.setMode(getMode());

        return clone;
    }

}
