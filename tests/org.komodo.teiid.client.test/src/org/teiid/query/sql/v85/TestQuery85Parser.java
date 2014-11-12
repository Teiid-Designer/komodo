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
package org.teiid.query.sql.v85;

import org.junit.Test;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.sql.lang.FromImpl;
import org.teiid.query.sql.lang.GroupByImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.v84.TestQuery84Parser;

/**
 *
 */
@SuppressWarnings( {"javadoc"} )
public class TestQuery85Parser extends TestQuery84Parser {

    protected TestQuery85Parser(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public TestQuery85Parser() {
        this(Version.TEIID_8_5.get());
    }

    @Test
    public void testGroupByRollup() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g"); //$NON-NLS-1$
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a")); //$NON-NLS-1$

        GroupByImpl groupBy = getFactory().newGroupBy();
        groupBy.setRollup(true);
        groupBy.addSymbol(getFactory().newElementSymbol("b")); //$NON-NLS-1$
        groupBy.addSymbol(getFactory().newElementSymbol("c")); //$NON-NLS-1$

        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);
        query.setFrom(from);
        query.setGroupBy(groupBy);
        helpTest("SELECT a FROM m.g GROUP BY rollup(b, c)", //$NON-NLS-1$
                 "SELECT a FROM m.g GROUP BY ROLLUP(b, c)", //$NON-NLS-1$
                 query);
    }
}
