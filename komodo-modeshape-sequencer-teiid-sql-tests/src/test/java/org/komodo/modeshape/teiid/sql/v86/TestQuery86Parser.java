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
package org.komodo.modeshape.teiid.sql.v86;

import static org.junit.Assert.fail;
import java.util.Arrays;
import org.junit.Test;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.sql.lang.QueryImpl;
import org.komodo.modeshape.teiid.sql.lang.SelectImpl;
import org.komodo.modeshape.teiid.sql.symbol.AggregateSymbolImpl;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbolImpl;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.modeshape.teiid.sql.symbol.ExpressionSymbolImpl;
import org.komodo.modeshape.teiid.sql.symbol.WindowFunctionImpl;
import org.komodo.modeshape.teiid.sql.symbol.WindowSpecificationImpl;
import org.komodo.modeshape.teiid.sql.v85.TestQuery85Parser;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;

/**
 *
 */
@SuppressWarnings( {"nls", "javadoc"} )
public class TestQuery86Parser extends TestQuery85Parser {

    protected TestQuery86Parser(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public TestQuery86Parser() {
        this(Version.TEIID_8_6.get());
    }

    @Override
    @Test
    public void testWindowedExpression() {
        String sql = "SELECT foo(x, y) over ()";
        String expectedSql = "SELECT foo(ALL x, y) OVER ()";

        try {
            ElementSymbolImpl x = getFactory().newElementSymbol("x");
            ElementSymbolImpl y = getFactory().newElementSymbol("y");
            AggregateSymbolImpl aggSym = getFactory().newAggregateSymbol("foo", false, null);
            aggSym.setArgs(new BaseExpression[] {x, y});
            WindowSpecificationImpl ws = getFactory().newWindowSpecification();

            WindowFunctionImpl wf = getFactory().newWindowFunction("");
            wf.setFunction(aggSym);
            wf.setWindowSpecification(ws);

            ExpressionSymbolImpl es = getFactory().newNode(ASTNodes.EXPRESSION_SYMBOL);
            es.setName("expr1");
            es.setExpression(wf);

            SelectImpl select = getFactory().newSelect(Arrays.asList(es));
            QueryImpl query = getFactory().newQuery(select, null);

            helpTest(sql, expectedSql, query);
        } catch (Exception ex) {
            fail(ex.getMessage());
        }
    }
}
