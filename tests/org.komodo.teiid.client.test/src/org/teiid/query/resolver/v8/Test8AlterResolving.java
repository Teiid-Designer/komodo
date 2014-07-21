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
package org.teiid.query.resolver.v8;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.teiid.query.resolver.AbstractTestAlterResolving;
import org.teiid.query.sql.AbstractTestFactory;
import org.teiid.query.sql.lang.AlterProcedure;
import org.teiid.query.sql.lang.Query;
import org.teiid.query.sql.proc.CommandStatement;
import org.teiid.query.sql.proc.CreateProcedureCommand;
import org.teiid.query.sql.symbol.ElementSymbol;
import org.teiid.query.sql.v8.Test8Factory;

/**
 *
 */
@SuppressWarnings( {"nls" , "javadoc"})
public class Test8AlterResolving extends AbstractTestAlterResolving {

    private Test8Factory factory;

    protected Test8AlterResolving(ITeiidVersion teiidVersion) {
        super(teiidVersion);
    }
   
    public Test8AlterResolving() {
        super(Version.TEIID_8_0.get());
    }

    @Override
    protected AbstractTestFactory getFactory() {
        if (factory == null)
            factory = new Test8Factory(getQueryParser());

        return factory;
    }

    @Test
    public void testAlterProcedure() {
        AlterProcedure alterProc = (AlterProcedure)helpResolve("alter procedure MMSP5 as begin select param1; end",
                                                               getMetadataFactory().exampleBQTCached());
        assertNotNull(alterProc.getTarget().getMetadataID());
        assertTrue(alterProc.getDefinition() instanceof CreateProcedureCommand);

        CreateProcedureCommand command = (CreateProcedureCommand) alterProc.getDefinition();
        Query q = (Query)((CommandStatement) command.getBlock().getStatements().get(0)).getCommand();
        assertTrue(((ElementSymbol)q.getSelect().getSymbol(0)).isExternalReference());
    }
}
