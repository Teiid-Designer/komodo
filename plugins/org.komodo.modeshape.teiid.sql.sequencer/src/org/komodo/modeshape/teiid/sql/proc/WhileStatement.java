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
import org.komodo.modeshape.teiid.sql.lang.Criteria;
import org.komodo.modeshape.teiid.sql.lang.Labeled;
import org.komodo.spi.query.sql.proc.IWhileStatement;

public class WhileStatement extends Statement implements Labeled, IWhileStatement<LanguageVisitor> {

    public WhileStatement(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * Return the type for this statement, this is one of the types
     * defined on the statement object.
     * @return The statement type
     */
    @Override
    public StatementType getType() {
        return StatementType.TYPE_WHILE;
    }

    /**
     * @return
     */
    public Criteria getCondition() {
        return null;
    }

    /**
     * @param criteria
     */
    public void setCondition(Criteria criteria) {
    }

    /**
     * @return
     */
    public Block getBlock() {
        return null;
    }

    /**
     * @param asBlock
     */
    public void setBlock(Block asBlock) {
    }

    @Override
    public String getLabel() {
        return null;
    }

    @Override
    public void setLabel(String label) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCondition() == null) ? 0 : this.getCondition().hashCode());
        result = prime * result + ((this.getLabel() == null) ? 0 : this.getLabel().hashCode());
        result = prime * result + ((this.getBlock() == null) ? 0 : this.getBlock().hashCode());
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
        WhileStatement other = (WhileStatement)obj;
        if (this.getCondition() == null) {
            if (other.getCondition() != null)
                return false;
        } else if (!this.getCondition().equals(other.getCondition()))
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
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public WhileStatement clone() {
        WhileStatement clone = new WhileStatement(this.getTeiidParser(), this.getId());

        if (getCondition() != null)
            clone.setCondition(getCondition().clone());
        if (getLabel() != null)
            clone.setLabel(getLabel());
        if (getBlock() != null)
            clone.setBlock(getBlock().clone());

        return clone;
    }

}
