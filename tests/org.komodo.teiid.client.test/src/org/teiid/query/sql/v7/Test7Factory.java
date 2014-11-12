/*
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
*/
package org.teiid.query.sql.v7;

import java.util.List;
import org.teiid.query.parser.TCQueryParser;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.AbstractTestFactory;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.CriteriaSelectorImpl;
import org.teiid.query.sql.lang.HasCriteriaImpl;
import org.teiid.query.sql.lang.TranslateCriteriaImpl;
import org.teiid.query.sql.proc.BlockImpl;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.proc.IfStatementImpl;
import org.teiid.query.sql.proc.RaiseErrorStatementImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.BaseWindowFunction;
import org.teiid.query.sql.symbol.ExpressionSymbolImpl;

/**
 *
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class Test7Factory extends AbstractTestFactory {

    /**
     * @param parser
     */
    public Test7Factory(TCQueryParser parser) {
        super(parser);
    }

    @Override
    public BaseExpression wrapExpression(BaseExpression expr, String... exprName) {
        String name = "expr";
        if (exprName != null && exprName.length > 0)
            name = exprName[0];

        ExpressionSymbolImpl exprSymbol = newNode(ASTNodes.EXPRESSION_SYMBOL);
        exprSymbol.setName(name);
        exprSymbol.setExpression(expr);
        return exprSymbol;
    }

    @Override
    public BaseAggregateSymbol newAggregateSymbol(String name, boolean isDistinct, BaseExpression expression) {
        BaseAggregateSymbol as = newNode(ASTNodes.AGGREGATE_SYMBOL);
        as.setName(name);
        as.setAggregateFunction(name);
        as.setDistinct(isDistinct);
        as.setExpression(expression);
        return as;
    }

    public BaseAggregateSymbol newAggregateSymbol(String name, String functionName, boolean isDistinct, BaseExpression expression) {
        BaseAggregateSymbol as = newNode(ASTNodes.AGGREGATE_SYMBOL);
        as.setName(name);
        as.setAggregateFunction(functionName);
        as.setDistinct(isDistinct);
        as.setExpression(expression);
        return as;
    }

    @Override
    public BaseWindowFunction newWindowFunction(String name) {
        BaseWindowFunction windowFunction = newNode(ASTNodes.WINDOW_FUNCTION);
        windowFunction.setName(name);
        return windowFunction;
    }

    public CriteriaSelectorImpl newCriteriaSelector() {
        CriteriaSelectorImpl cs = newNode(ASTNodes.CRITERIA_SELECTOR);
        return cs;
    }

    public CreateUpdateProcedureCommandImpl newCreateUpdateProcedureCommand() {
        CreateUpdateProcedureCommandImpl cupc = newNode(ASTNodes.CREATE_UPDATE_PROCEDURE_COMMAND);
        return cupc;
    }

    public CreateUpdateProcedureCommandImpl newCreateUpdateProcedureCommand(BlockImpl b) {
        CreateUpdateProcedureCommandImpl command = newCreateUpdateProcedureCommand();
        command.setBlock(b);
        return command;
    }

    public HasCriteriaImpl newHasCriteria(CriteriaSelectorImpl critSelector) {
        HasCriteriaImpl hasSelector = newNode(ASTNodes.HAS_CRITERIA);
        hasSelector.setSelector(critSelector);
        return hasSelector;
    }

    public TranslateCriteriaImpl newTranslateCriteria() {
        TranslateCriteriaImpl tc = newNode(ASTNodes.TRANSLATE_CRITERIA);
        return tc;
    }

    public TranslateCriteriaImpl newTranslateCriteria(CriteriaSelectorImpl critSelector, List<CompareCriteriaImpl> critList) {
        TranslateCriteriaImpl tc = newTranslateCriteria();
        tc.setSelector(critSelector);
        tc.setTranslations(critList);
        return tc;
    }

    public IfStatementImpl newIfStatement(BlockImpl ifBlock, BlockImpl elseBlock, HasCriteriaImpl hasSelector) {
        IfStatementImpl ifStmt = newNode(ASTNodes.IF_STATEMENT);
        ifStmt.setIfBlock(ifBlock);
        ifStmt.setElseBlock(elseBlock);
        ifStmt.setCondition(hasSelector);
        return ifStmt;
    }

    @Override
    public RaiseErrorStatementImpl newRaiseStatement(BaseExpression expr) {
        RaiseErrorStatementImpl raiseStatement = newNode(ASTNodes.RAISE_ERROR_STATEMENT);
        raiseStatement.setExpression(expr);
        return raiseStatement;
    }
}
