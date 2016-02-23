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
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.proc.IfStatement;

/**
 *
 */
public class IfStatementImpl extends StatementImpl implements IfStatement<SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public IfStatementImpl(TeiidSeqParser p, int id) {
        super(p, id);
        setType(StatementType.TYPE_IF);
    }

    public CriteriaImpl getCondition() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.IfStatement.CONDITION_REF_NAME, CriteriaImpl.class);
    }

    public void setCondition(CriteriaImpl criteria) {
        setChild(TeiidSqlLexicon.IfStatement.CONDITION_REF_NAME, criteria);
    }

    public BlockImpl getIfBlock() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.IfStatement.IF_BLOCK_REF_NAME, BlockImpl.class);
    }

    public void setIfBlock(BlockImpl asBlock) {
        setChild(TeiidSqlLexicon.IfStatement.IF_BLOCK_REF_NAME, asBlock);
    }

    public BlockImpl getElseBlock() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.IfStatement.ELSE_BLOCK_REF_NAME, BlockImpl.class);
    }

    public void setElseBlock(BlockImpl asBlock) {
        setChild(TeiidSqlLexicon.IfStatement.ELSE_BLOCK_REF_NAME, asBlock);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getCondition() == null) ? 0 : this.getCondition().hashCode());
        result = prime * result + ((this.getElseBlock() == null) ? 0 : this.getElseBlock().hashCode());
        result = prime * result + ((this.getIfBlock() == null) ? 0 : this.getIfBlock().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!super.equals(obj)) return false;
        if (getClass() != obj.getClass()) return false;
        IfStatementImpl other = (IfStatementImpl)obj;
        if (this.getCondition() == null) {
            if (other.getCondition() != null) return false;
        } else if (!this.getCondition().equals(other.getCondition())) return false;
        if (this.getElseBlock() == null) {
            if (other.getElseBlock() != null) return false;
        } else if (!this.getElseBlock().equals(other.getElseBlock())) return false;
        if (this.getIfBlock() == null) {
            if (other.getIfBlock() != null) return false;
        } else if (!this.getIfBlock().equals(other.getIfBlock())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public IfStatementImpl clone() {
        IfStatementImpl clone = new IfStatementImpl(this.getTeiidParser(), this.getId());

        if(getCondition() != null)
            clone.setCondition(getCondition().clone());
        if(getIfBlock() != null)
            clone.setIfBlock(getIfBlock().clone());
        if(getElseBlock() != null)
            clone.setElseBlock(getElseBlock().clone());

        return clone;
    }

}
