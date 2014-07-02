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
package org.teiid.query.sql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.teiid.query.parser.ParseInfo;
import org.teiid.query.sql.lang.LanguageObject;
import org.teiid.query.sql.lang.Query;
import org.teiid.query.sql.lang.Select;
import org.teiid.query.sql.proc.Statement;
import org.teiid.query.sql.symbol.Expression;

/**
 *
 */
@SuppressWarnings( {"nls"} )
public abstract class AbstractTestCloning extends AbstractTest<LanguageObject> {

    /**
     * @param teiidVersion 
     */
    public AbstractTestCloning(ITeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    protected void helpTest(String sql, LanguageObject expectedNode) {
        helpTest(sql, sql, expectedNode);
    }

    protected void helpTest(String sql, String expectedSql, LanguageObject expectedNode) {
        helpTest(sql, expectedSql, expectedNode, new ParseInfo());
    }

    protected void helpTest(String sql, String expectedSql, LanguageObject expectedNode, ParseInfo info) {
        LanguageObject clonedNode = null;
        try {
            clonedNode = expectedNode.clone();
        } catch (Throwable e) {
            fail(e.getClass().getSimpleName() + ": " + e.getMessage());
        }

        assertEquals("Cloned object does not match: ", expectedNode, clonedNode);
    }

    protected void helpTestLiteral(Boolean expected, Class<?> expectedType, String sql, String expectedSql) {
        Select select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(expected, expectedType)));

        Query query = getFactory().newQuery();
        query.setSelect(select);

        helpTest(sql, expectedSql, query);
    }

    protected void helpTestExpression(String sql, String expectedString, Expression expected) throws Exception {
        helpTest(sql, expectedString, expected);
    }

    protected void helpStmtTest(String stmt, String expectedString, Statement expectedStmt) throws Exception {
        helpTest(stmt, expectedString, expectedStmt);
    }
}
