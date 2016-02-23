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
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.lang.CommandImpl;
import org.komodo.modeshape.teiid.sql.lang.QueryCommandImpl;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbolImpl;
import org.komodo.modeshape.teiid.sql.symbol.ScalarSubqueryImpl;
import org.komodo.spi.lexicon.TeiidSqlLexicon;
import org.komodo.spi.query.sql.proc.AssignmentStatement;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
public class AssignmentStatementImpl extends StatementImpl implements BaseExpressionStatement, AssignmentStatement<BaseExpression, SQLanguageVisitorImpl> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public AssignmentStatementImpl(TeiidSeqParser p, int id) {
        super(p, id);
        setType(StatementType.TYPE_ASSIGNMENT);
    }

    @Override
    public ElementSymbolImpl getVariable() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.AssignmentStatement.VARIABLE_REF_NAME, ElementSymbolImpl.class);
    }

    public void setVariable(ElementSymbolImpl elementSymbol) {
        setChild(TeiidSqlLexicon.AssignmentStatement.VARIABLE_REF_NAME, elementSymbol);
        
        Class<?> type = elementSymbol.getType();
        DataTypeName dataType = getDataTypeService().retrieveDataTypeName(type);
        setProperty(TeiidSqlLexicon.ExpressionStatement.EXPECTED_TYPE_CLASS_PROP_NAME, dataType.name());
    }

    @Override
    public BaseExpression getValue() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.AssignmentStatement.VALUE_REF_NAME, BaseExpression.class);
    }

    /**
     * Both setters need to exist due to implementing different interfaces
     * To avoid any pain, simply store the same Expression in both.
     *
     * @param value
     */
    private void assignValue(BaseExpression value) {
        setChild(TeiidSqlLexicon.ExpressionStatement.EXPRESSION_REF_NAME, value);
        // Cannot store the same reference under 2 references as adding the value will 'move' the child to the new parent
        setChild(TeiidSqlLexicon.AssignmentStatement.VALUE_REF_NAME, value.clone());
    }

    @Override
    public void setValue(BaseExpression value) {
        assignValue(value);
    }

    public CommandImpl getCommand() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.AssignmentStatement.COMMAND_REF_NAME, CommandImpl.class);
    }

    public void setCommand(CommandImpl command) {
        if (command instanceof QueryCommandImpl) {
            ScalarSubqueryImpl ssq = getTeiidParser().createASTNode(ASTNodes.SCALAR_SUBQUERY);
            ssq.setCommand((QueryCommandImpl) command);
            setValue(ssq);
        } else
            setChild(TeiidSqlLexicon.AssignmentStatement.COMMAND_REF_NAME, command);
    }

    @Override
    public BaseExpression getExpression() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.ExpressionStatement.EXPRESSION_REF_NAME, BaseExpression.class);
    }

    @Override
    public void setExpression(BaseExpression expr) {
        assignValue(expr);
    }

    @Override
    public Class<?> getExpectedType() {
        return convertTypeClassPropertyToClass(TeiidSqlLexicon.ExpressionStatement.EXPECTED_TYPE_CLASS_PROP_NAME);
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
        AssignmentStatementImpl other = (AssignmentStatementImpl)obj;
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
    public void acceptVisitor(SQLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public AssignmentStatementImpl clone() {
        AssignmentStatementImpl clone = new AssignmentStatementImpl(this.getTeiidParser(), this.getId());
    
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
