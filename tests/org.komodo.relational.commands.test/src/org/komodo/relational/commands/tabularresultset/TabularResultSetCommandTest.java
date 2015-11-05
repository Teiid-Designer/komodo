/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational.commands.tabularresultset;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

@SuppressWarnings( { "javadoc",
                     "nls" } )
public abstract class TabularResultSetCommandTest extends AbstractCommandTest {

    private TabularResultSet resultSet;

    @Before
    public void createContext() throws Exception {
        final String[] commands = { "workspace",
                                    "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model myModel ",
                                    "cd myModel",
                                    "add-pushdown-function myPushdownFunction",
                                    "cd myPushdownFunction",
                                    "set-result-set TabularResultSet",
                                    "cd resultSet" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance( _repo );
        final Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 1 ) );

        final Model[] models = vdbs[ 0 ].getModels( getTransaction() );
        assertThat( models.length, is( 1 ) );

        final Function[] functions = models[ 0 ].getFunctions( getTransaction() );
        assertThat( functions.length, is( 1 ) );
        assertThat( functions[ 0 ], is( instanceOf( PushdownFunction.class ) ) );

        final ProcedureResultSet procedureResultSet = ( ( PushdownFunction )functions[ 0 ] ).getResultSet( getTransaction() );
        assertThat( procedureResultSet, is( instanceOf( TabularResultSet.class ) ) );

        this.resultSet = ( TabularResultSet )procedureResultSet;
    }

    protected TabularResultSet get() {
        return this.resultSet;
    }

}
