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
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.spi.query.sql.proc.BranchingStatement;

/**
 *
 */
public class BranchingStatementImpl extends StatementImpl implements BranchingStatement<SQLanguageVisitorImpl> {

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
        LEAVE;

        /**
         * @param name
         * @return BranchingMode for given name
         */
        public static BranchingMode findBranchingMode(String name) {
            if (name == null)
                return null;

            name = name.toUpperCase();
            for (BranchingMode mode : values()) {
                if (mode.name().equals(name))
                    return mode;
            }

            return null;
        }
    }

    public BranchingStatementImpl(TeiidSeqParser p, int id) {
        super(p, id);
        setType(StatementType.TYPE_BREAK);
    }

    public String getLabel() {
        Object property = getProperty(TeiidSqlLexicon.BranchingStatement.LABEL_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setLabel(String label) {
        setProperty(TeiidSqlLexicon.BranchingStatement.LABEL_PROP_NAME, label);
    }

    public BranchingMode getMode() {
        Object property = getProperty(TeiidSqlLexicon.BranchingStatement.MODE_PROP_NAME);
        return property == null ? BranchingMode.BREAK : BranchingMode.findBranchingMode(property.toString());
    }

    public void setMode(BranchingMode mode) {
        setProperty(TeiidSqlLexicon.BranchingStatement.MODE_PROP_NAME, mode);
        switch (mode) {
            case BREAK:
                setType(StatementType.TYPE_BREAK);
                return;
            case CONTINUE:
                setType(StatementType.TYPE_CONTINUE);
                return;
            case LEAVE:
                setType(StatementType.TYPE_LEAVE);
        }
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
        BranchingStatementImpl other = (BranchingStatementImpl)obj;
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
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public BranchingStatementImpl clone() {
        BranchingStatementImpl clone = new BranchingStatementImpl(this.getTeiidParser(), this.getId());

        if (getLabel() != null)
            clone.setLabel(getLabel());
        if (getMode() != null)
            clone.setMode(getMode());

        return clone;
    }

}
