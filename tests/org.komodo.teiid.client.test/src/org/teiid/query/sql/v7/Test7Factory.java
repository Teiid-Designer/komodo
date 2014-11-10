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
import org.teiid.query.sql.lang.CompareCriteria;
import org.teiid.query.sql.lang.CriteriaSelector;
import org.teiid.query.sql.lang.HasCriteria;
import org.teiid.query.sql.lang.TranslateCriteria;
import org.teiid.query.sql.proc.Block;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommand;
import org.teiid.query.sql.proc.IfStatement;
import org.teiid.query.sql.proc.RaiseErrorStatement;
import org.teiid.query.sql.symbol.AggregateSymbol;
import org.teiid.query.sql.symbol.Expression;
import org.teiid.query.sql.symbol.ExpressionSymbol;
import org.teiid.query.sql.symbol.WindowFunction;

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
    public Expression wrapExpression(Expression expr, String... exprName) {
        String name = "expr";
        if (exprName != null && exprName.length > 0)
            name = exprName[0];

        ExpressionSymbol exprSymbol = newNode(ASTNodes.EXPRESSION_SYMBOL);
        exprSymbol.setName(name);
        exprSymbol.setExpression(expr);
        return exprSymbol;
    }

    @Override
    public AggregateSymbol newAggregateSymbol(String name, boolean isDistinct, Expression expression) {
        AggregateSymbol as = newNode(ASTNodes.AGGREGATE_SYMBOL);
        as.setName(name);
        as.setAggregateFunction(name);
        as.setDistinct(isDistinct);
        as.setExpression(expression);
        return as;
    }

    public AggregateSymbol newAggregateSymbol(String name, String functionName, boolean isDistinct, Expression expression) {
        AggregateSymbol as = newNode(ASTNodes.AGGREGATE_SYMBOL);
        as.setName(name);
        as.setAggregateFunction(functionName);
        as.setDistinct(isDistinct);
        as.setExpression(expression);
        return as;
    }

    @Override
    public WindowFunction newWindowFunction(String name) {
        WindowFunction windowFunction = newNode(ASTNodes.WINDOW_FUNCTION);
        windowFunction.setName(name);
        return windowFunction;
    }

    public CriteriaSelector newCriteriaSelector() {
        CriteriaSelector cs = newNode(ASTNodes.CRITERIA_SELECTOR);
        return cs;
    }

    public CreateUpdateProcedureCommand newCreateUpdateProcedureCommand() {
        CreateUpdateProcedureCommand cupc = newNode(ASTNodes.CREATE_UPDATE_PROCEDURE_COMMAND);
        return cupc;
    }

    public CreateUpdateProcedureCommand newCreateUpdateProcedureCommand(Block b) {
        CreateUpdateProcedureCommand command = newCreateUpdateProcedureCommand();
        command.setBlock(b);
        return command;
    }

    public HasCriteria newHasCriteria(CriteriaSelector critSelector) {
        HasCriteria hasSelector = newNode(ASTNodes.HAS_CRITERIA);
        hasSelector.setSelector(critSelector);
        return hasSelector;
    }

    public TranslateCriteria newTranslateCriteria() {
        TranslateCriteria tc = newNode(ASTNodes.TRANSLATE_CRITERIA);
        return tc;
    }

    public TranslateCriteria newTranslateCriteria(CriteriaSelector critSelector, List<CompareCriteria> critList) {
        TranslateCriteria tc = newTranslateCriteria();
        tc.setSelector(critSelector);
        tc.setTranslations(critList);
        return tc;
    }

    public IfStatement newIfStatement(Block ifBlock, Block elseBlock, HasCriteria hasSelector) {
        IfStatement ifStmt = newNode(ASTNodes.IF_STATEMENT);
        ifStmt.setIfBlock(ifBlock);
        ifStmt.setElseBlock(elseBlock);
        ifStmt.setCondition(hasSelector);
        return ifStmt;
    }

    @Override
    public RaiseErrorStatement newRaiseStatement(Expression expr) {
        RaiseErrorStatement raiseStatement = newNode(ASTNodes.RAISE_ERROR_STATEMENT);
        raiseStatement.setExpression(expr);
        return raiseStatement;
    }
}
