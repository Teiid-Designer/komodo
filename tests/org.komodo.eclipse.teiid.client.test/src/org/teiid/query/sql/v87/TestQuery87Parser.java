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
package org.teiid.query.sql.v87;

import org.junit.Test;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.teiid.query.sql.lang.From;
import org.teiid.query.sql.lang.SPParameter;
import org.teiid.query.sql.lang.StoredProcedure;
import org.teiid.query.sql.lang.SubqueryFromClause;
import org.teiid.query.sql.v85.TestQuery85Parser;

/**
 *
 */
@SuppressWarnings( {"nls", "javadoc"} )
public class TestQuery87Parser extends TestQuery85Parser {

    protected TestQuery87Parser(ITeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public TestQuery87Parser() {
        this(Version.TEIID_8_7.get());
    }

    @Override
    @Test
    public void testStoredQuery2SanityCheck() {
        StoredProcedure storedQuery = getFactory().newStoredProcedure();
        storedQuery.setProcedureName("proc1");
        SPParameter parameter = getFactory().newSPParameter(1, getFactory().newConstant("param1"));
        storedQuery.setParameter(parameter);
        From from = getFactory().newFrom();
        SubqueryFromClause sfc = getFactory().newSubqueryFromClause("x", storedQuery);
        from.addClause(sfc);

        helpTest("exec proc1('param1')", "EXEC proc1('param1')", storedQuery);
    }
}
