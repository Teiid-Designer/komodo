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

import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.CriteriaImpl;
import org.komodo.modeshape.teiid.sql.lang.Labeled;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.proc.WhileStatement;

/**
 *
 */
public class WhileStatementImpl extends StatementImpl implements Labeled, WhileStatement<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public WhileStatementImpl(TeiidSeqParser p, int id) {
        super(p, id);
        setType(StatementType.TYPE_WHILE);
    }

    public CriteriaImpl getCondition() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.WhileStatement.CONDITION_REF_NAME, CriteriaImpl.class);
    }

    public void setCondition(CriteriaImpl criteria) {
        setChild(TeiidSqlLexicon.WhileStatement.CONDITION_REF_NAME, criteria);
    }

    public BlockImpl getBlock() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.WhileStatement.BLOCK_REF_NAME, BlockImpl.class);
    }

    public void setBlock(BlockImpl block) {
        setChild(TeiidSqlLexicon.WhileStatement.BLOCK_REF_NAME, block);
    }

    @Override
    public String getLabel() {
        Object property = getProperty(TeiidSqlLexicon.Labeled.LABEL_PROP_NAME);
        return property == null ? null : property.toString();
    }

    @Override
    public void setLabel(String label) {
        setProperty(TeiidSqlLexicon.Labeled.LABEL_PROP_NAME, label);
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
        WhileStatementImpl other = (WhileStatementImpl)obj;
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
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public WhileStatementImpl clone() {
        WhileStatementImpl clone = new WhileStatementImpl(this.getTeiidParser(), this.getId());

        if (getCondition() != null)
            clone.setCondition(getCondition().clone());
        if (getLabel() != null)
            clone.setLabel(getLabel());
        if (getBlock() != null)
            clone.setBlock(getBlock().clone());

        return clone;
    }

}
