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
package org.teiid.query.resolver.v7;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.resolver.AbstractTestAlterResolving;
import org.teiid.query.sql.AbstractTestFactory;
import org.teiid.query.sql.lang.AlterProcedure;
import org.teiid.query.sql.lang.Query;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommand;
import org.teiid.query.sql.symbol.ElementSymbol;
import org.teiid.query.sql.v7.Test7Factory;

/**
 *
 */
@SuppressWarnings( {"nls" , "javadoc"})
public class Test7AlterResolving extends AbstractTestAlterResolving {

    private Test7Factory factory;

    /**
     *
     */
    public Test7AlterResolving() {
        super(Version.TEIID_7_7.get());
    }

    @Override
    protected AbstractTestFactory getFactory() {
        if (factory == null)
            factory = new Test7Factory(getQueryParser());

        return factory;
    }

    @Test
    public void testAlterProcedure() {
        AlterProcedure alterProc = (AlterProcedure)helpResolve("alter procedure MMSP5 as begin select param1; end",
                                                               getMetadataFactory().exampleBQTCached());
        assertNotNull(alterProc.getTarget().getMetadataID());
        assertTrue(alterProc.getDefinition() instanceof CreateUpdateProcedureCommand);

        CreateUpdateProcedureCommand command = (CreateUpdateProcedureCommand) alterProc.getDefinition();
        Query q = (Query) command.getResultsCommand();
        assertTrue(((ElementSymbol)q.getSelect().getSymbol(0)).isExternalReference());
    }
}
