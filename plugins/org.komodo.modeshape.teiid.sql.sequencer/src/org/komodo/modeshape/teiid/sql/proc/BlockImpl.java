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
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.SQLanguageVisitorImpl;
import org.komodo.modeshape.teiid.sql.lang.CommandImpl;
import org.komodo.modeshape.teiid.sql.lang.Labeled;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbolImpl;
import org.komodo.spi.query.sql.SQLConstants;
import org.komodo.spi.query.sql.proc.Block;
import org.komodo.spi.query.sql.symbol.Symbol;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;

/**
 *
 */
public class BlockImpl extends StatementImpl implements Labeled, Block<StatementImpl, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public BlockImpl(TeiidSeqParser p, int id) {
        super(p, id);
        setType(StatementType.TYPE_COMPOUND);
    }

    public boolean isAtomic() {
        Object property = getProperty(TeiidSqlLexicon.Block.ATOMIC_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setAtomic(boolean atomic) {
        setProperty(TeiidSqlLexicon.Block.ATOMIC_PROP_NAME, atomic);
    }

    public String getExceptionGroup() {
        Object property = getProperty(TeiidSqlLexicon.Block.EXCEPTION_GROUP_PROP_NAME);
        return property == null ? null : property.toString();
    }

    public void setExceptionGroup(String eId) {
        setProperty(TeiidSqlLexicon.Block.EXCEPTION_GROUP_PROP_NAME, eId);
    }

    @Override
    public List<StatementImpl> getStatements() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.Block.STATEMENTS_REF_NAME, StatementImpl.class);
    }

    private void internalAddStatement(StatementImpl statement, boolean exception) {
        if (exception) {
            addLastChild(TeiidSqlLexicon.Block.EXCEPTION_STATEMENTS_REF_NAME, statement);
        } else {
            addLastChild(TeiidSqlLexicon.Block.STATEMENTS_REF_NAME, statement);
        }
    }

    public void addStatement(StatementImpl statement, boolean exception) {
        if (statement instanceof AssignmentStatementImpl) {
            AssignmentStatementImpl stmt = (AssignmentStatementImpl)statement;
            CommandImpl cmd = stmt.getCommand();
            if (cmd != null) {
                CommandStatementImpl cs = getTeiidParser().createASTNode(ASTNodes.COMMAND_STATEMENT);
                cs.setCommand(cmd);
                internalAddStatement(cs, exception);
                stmt.setCommand(null);
                stmt.setExpression(null);
                ElementSymbolImpl variable = stmt.getVariable();
                if (variable != null && variable.getShortName().equalsIgnoreCase(SQLConstants.ROWCOUNT) 
                        && variable.getGroupSymbol() != null && variable.getGroupSymbol().getName().equalsIgnoreCase(SQLConstants.VARIABLES)) {
                    return;
                }
                String fullName = SQLConstants.VARIABLES + Symbol.SEPARATOR + SQLConstants.ROWCOUNT;
                ElementSymbolImpl es = getTeiidParser().createASTNode(ASTNodes.ELEMENT_SYMBOL);
                es.setName(fullName);
                stmt.setExpression(es);
            }
        }
        internalAddStatement(statement, exception);
    }

    @Override
    public void addStatement(StatementImpl statement) {
        addStatement(statement, false);
    }

    public void setStatements(List<StatementImpl> statements) {
        setChildren(TeiidSqlLexicon.Block.STATEMENTS_REF_NAME, statements);
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

    public List<StatementImpl> getExceptionStatements() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.Block.EXCEPTION_STATEMENTS_REF_NAME, StatementImpl.class);
    }

    public void setExceptionStatements(List<StatementImpl> exceptionStatements) {
        setChildren(TeiidSqlLexicon.Block.EXCEPTION_STATEMENTS_REF_NAME, exceptionStatements);
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
        BlockImpl other = (BlockImpl)obj;
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
        } else if (!this.getStatements().equals(other.getStatements()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public BlockImpl clone() {
        BlockImpl clone = new BlockImpl(this.getTeiidParser(), this.getId());
    
        clone.setAtomic(isAtomic());
        if(getLabel() != null)
            clone.setLabel(getLabel());
        if(getExceptionGroup() != null)
            clone.setExceptionGroup(getExceptionGroup());
        if(getExceptionStatements() != null)
            clone.setExceptionStatements(cloneList(getExceptionStatements()));
        if(getStatements() != null) {
            for (StatementImpl statement : getStatements())
                clone.addStatement(statement.clone());
        }
    
        return clone;
    }

}
