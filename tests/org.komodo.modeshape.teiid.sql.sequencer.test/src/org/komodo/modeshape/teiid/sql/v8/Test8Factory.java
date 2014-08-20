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
package org.komodo.modeshape.teiid.sql.v8;

import org.komodo.modeshape.teiid.parser.QueryParser;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.sql.AbstractTestFactory;
import org.komodo.modeshape.teiid.sql.proc.RaiseStatement;
import org.komodo.modeshape.teiid.sql.symbol.AggregateSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.modeshape.teiid.sql.symbol.WindowFunction;

/**
 *
 */
public class Test8Factory extends AbstractTestFactory {

    /**
     * @param parser
     */
    public Test8Factory(QueryParser parser) {
        super(parser);
    }

    @Override
    public Expression wrapExpression(Expression expr, String... exprName) {
        // Expression are no longer wrapped in ExpressionSymbols. Purely a version 7 concept
        return expr;
    }

    @Override
    public AggregateSymbol newAggregateSymbol(String name, boolean isDistinct, Expression expression) {
        AggregateSymbol as = newNode(ASTNodes.AGGREGATE_SYMBOL);
        as.setName(name);
        as.setDistinct(isDistinct);
        if (expression == null)
            as.setArgs(null);
        else
            as.setArgs(new Expression[] {expression});
        return as;
    }

    @Override
    public WindowFunction newWindowFunction(String name) {
        WindowFunction windowFunction = newNode(ASTNodes.WINDOW_FUNCTION);
        // window function no longer uses name
        return windowFunction;
    }

    @Override
    public RaiseStatement newRaiseStatement(Expression expr) {
        RaiseStatement raiseStatement = newNode(ASTNodes.RAISE_STATEMENT);
        raiseStatement.setExpression(expr);
        return raiseStatement;
    }

}
