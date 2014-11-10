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
package org.teiid.query.resolver.v86;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.resolver.v85.Test85Resolver;
import org.teiid.query.sql.lang.Command;

@SuppressWarnings( {"javadoc"} )
public class Test86Resolver extends Test85Resolver {

    protected Test86Resolver(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public Test86Resolver() {
        this(Version.TEIID_8_6.get());
    }

    @Override
    @Test
    public void testImplicitTempInsertWithNoColumns() {
        StringBuffer proc = new StringBuffer("CREATE VIRTUAL PROCEDURE") //$NON-NLS-1$
        .append("\nBEGIN") //$NON-NLS-1$
        .append("\n  create local temporary table #matt (x integer);") //$NON-NLS-1$
        .append("\n  insert into #matt values (1);") //$NON-NLS-1$
        .append("\nEND"); //$NON-NLS-1$

        Command cmd = helpResolve(proc.toString());

        String sExpected = "BEGIN\nCREATE LOCAL TEMPORARY TABLE #matt (x integer);\nINSERT INTO #matt (x) VALUES (1);\nEND\n\tCREATE LOCAL TEMPORARY TABLE #matt (x integer)\n\tINSERT INTO #matt (x) VALUES (1)\n"; //$NON-NLS-1$
        String sActual = cmd.printCommandTree();
        assertEquals(sExpected, sActual);
    }
}
