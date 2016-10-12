/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.relational.commands.datatyperesultset;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.DataTypeResultSet;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

@SuppressWarnings( { "javadoc",
                     "nls" } )
public abstract class DataTypeResultSetCommandTest extends AbstractCommandTest {

    private DataTypeResultSet resultSet;

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model myModel ",
                                    "cd myModel",
                                    "add-pushdown-function myPushdownFunction",
                                    "cd myPushdownFunction",
                                    "set-result-set DataTypeResultSet",
                                    "cd resultSet" };
        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 1 ) );

        final Model[] models = vdbs[ 0 ].getModels( getTransaction() );
        assertThat( models.length, is( 1 ) );

        final Function[] functions = models[ 0 ].getFunctions( getTransaction() );
        assertThat( functions.length, is( 1 ) );
        assertThat( functions[ 0 ], is( instanceOf( PushdownFunction.class ) ) );

        final ProcedureResultSet procedureResultSet = ( ( PushdownFunction )functions[ 0 ] ).getResultSet( getTransaction() );
        assertThat( procedureResultSet, is( instanceOf( DataTypeResultSet.class ) ) );

        this.resultSet = ( DataTypeResultSet )procedureResultSet;
    }

    protected DataTypeResultSet get() {
        return this.resultSet;
    }

}
