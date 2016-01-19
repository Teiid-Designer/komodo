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
package org.teiid.query.sql.v8;

import org.teiid.query.parser.TCQueryParser;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.AbstractTestFactory;
import org.teiid.query.sql.proc.RaiseStatementImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.BaseWindowFunction;

@SuppressWarnings( {"javadoc"} )
public class Test8Factory extends AbstractTestFactory {

    /**
     * @param parser
     */
    public Test8Factory(TCQueryParser parser) {
        super(parser);
    }

    @Override
    public BaseExpression wrapExpression(BaseExpression expr, String... exprName) {
        // Expression are no longer wrapped in ExpressionSymbols. Purely a version 7 concept
        return expr;
    }

    @Override
    public BaseAggregateSymbol newAggregateSymbol(String name, boolean isDistinct, BaseExpression expression) {
        BaseAggregateSymbol as = newNode(ASTNodes.AGGREGATE_SYMBOL);
        as.setName(name);
        as.setDistinct(isDistinct);
        if (expression == null)
            as.setArgs(null);
        else
            as.setArgs(new BaseExpression[] {expression});
        return as;
    }

    @Override
    public BaseWindowFunction newWindowFunction(String name) {
        BaseWindowFunction windowFunction = newNode(ASTNodes.WINDOW_FUNCTION);
        // window function no longer uses name
        return windowFunction;
    }

    @Override
    public RaiseStatementImpl newRaiseStatement(BaseExpression expr) {
        RaiseStatementImpl raiseStatement = newNode(ASTNodes.RAISE_STATEMENT);
        raiseStatement.setExpression(expr);
        return raiseStatement;
    }

}
