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

import java.util.List;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.Labeled;
import org.komodo.spi.query.sql.proc.IBlock;

public class Block extends Statement implements Labeled, IBlock<Statement, LanguageVisitor> {

    public Block(TeiidParser p, int id) {
        super(p, id);
    }

    /**
     * Return the type for this statement, this is one of the types
     * defined on the statement object.
     * @return The statement type
     */
    @Override
    public StatementType getType() {
        return StatementType.TYPE_COMPOUND;
    }

    /**
     * @return
     */
    public boolean isAtomic() {
        return false;
    }

    /**
     * @param b
     */
    public void setAtomic(boolean b) {
    }

    /**
     * @return
     */
    public String getExceptionGroup() {
        return null;
    }

    /**
     * @param eId
     */
    public void setExceptionGroup(String eId) {
    }
    @Override
    public List<Statement> getStatements() {
        return null;
    }

    /**
     * @param stmt
     * @param b
     */
    public void addStatement(Statement stmt, boolean b) {
    }

    @Override
    public void addStatement(Statement statement) {
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
    public List<Statement> getExceptionStatements() {
        return null;
    }

    /**
     * @param cloneList
     */
    public void setExceptionStatements(List<Statement> cloneList) {
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isAtomic() ? 1231 : 1237);
        result = prime * result + ((this.getExceptionGroup() == null) ? 0 : this.getExceptionGroup().hashCode());
        result = prime * result + ((this.getExceptionStatements() == null) ? 0 : this.getExceptionStatements().hashCode());
        result = prime * result + ((this.getLabel() == null) ? 0 : this.getLabel().hashCode());
        result = prime * result + ((this.getStatements() == null) ? 0 : this.getStatements().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        Block other = (Block)obj;
        if (this.isAtomic() != other.isAtomic()) return false;
        if (this.getExceptionGroup() == null) {
            if (other.getExceptionGroup() != null) return false;
        } else if (!this.getExceptionGroup().equals(other.getExceptionGroup())) return false;
        if (this.getExceptionStatements() == null) {
            if (other.getExceptionStatements() != null) return false;
        } else if (!this.getExceptionStatements().equals(other.getExceptionStatements())) return false;
        if (this.getLabel() == null) {
            if (other.getLabel() != null) return false;
        } else if (!this.getLabel().equals(other.getLabel())) return false;
        if (this.getStatements() == null) {
            if (other.getStatements() != null) return false;
        } else if (!this.getStatements().equals(other.getStatements())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public Block clone() {
        Block clone = new Block(this.getTeiidParser(), this.getId());
    
        clone.setAtomic(isAtomic());
        if(getLabel() != null)
            clone.setLabel(getLabel());
        if(getExceptionGroup() != null)
            clone.setExceptionGroup(getExceptionGroup());
        if(getExceptionStatements() != null)
            clone.setExceptionStatements(cloneList(getExceptionStatements()));
        if(getStatements() != null) {
            for (Statement statement : getStatements())
                clone.addStatement(statement);
        }
    
        return clone;
    }

}
