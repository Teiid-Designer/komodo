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
package org.komodo.modeshape.teiid.sql.v87;

import org.junit.Test;
import org.komodo.modeshape.teiid.sql.lang.FromImpl;
import org.komodo.modeshape.teiid.sql.lang.SPParameterImpl;
import org.komodo.modeshape.teiid.sql.lang.StoredProcedureImpl;
import org.komodo.modeshape.teiid.sql.lang.SubqueryFromClauseImpl;
import org.komodo.modeshape.teiid.sql.v86.TestQuery86Parser;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;

/**
 *
 */
@SuppressWarnings( {"nls", "javadoc"} )
public class TestQuery87Parser extends TestQuery86Parser {

    protected TestQuery87Parser(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public TestQuery87Parser() {
        this(Version.TEIID_8_7.get());
    }

    @Override
    @Test
    public void testStoredQuery2SanityCheck() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setProcedureName("proc1");
        SPParameterImpl parameter = getFactory().newSPParameter(1, getFactory().newConstant("param1"));
        storedQuery.addParameter(parameter);
        FromImpl from = getFactory().newFrom();
        SubqueryFromClauseImpl sfc = getFactory().newSubqueryFromClause("x", storedQuery);
        from.addClause(sfc);

        helpTest("exec proc1('param1')", "EXEC proc1('param1')", storedQuery);
    }
}
