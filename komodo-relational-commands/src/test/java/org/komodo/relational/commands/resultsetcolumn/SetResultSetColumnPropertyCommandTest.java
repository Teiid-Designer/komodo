/*
 * Copyright 2014 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.relational.commands.resultsetcolumn;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Function;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.PushdownFunction;
import org.komodo.relational.model.ResultSetColumn;
import org.komodo.relational.model.TabularResultSet;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetResultSetColumnPropertyCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class SetResultSetColumnPropertyCommandTest extends AbstractCommandTest {

    @Test
    public void testSetProperty1() throws Exception {
        final String[] commands = {
            "create-vdb myVdb vdbPath",
            "cd myVdb",
            "add-model myModel",
            "cd myModel",
            "add-pushdown-function myPushdownFunction",
            "cd myPushdownFunction",
            "set-result-set TabularResultSet",
            "cd resultSet",
            "add-column myColumn",
            "cd myColumn",
            "set-property NAMEINSOURCE myNameInSource" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);

        Model[] models = vdbs[0].getModels(getTransaction());
        assertEquals(1, models.length);
        assertEquals("myModel", models[0].getName(getTransaction())); //$NON-NLS-1$

        Function[] functions = models[0].getFunctions(getTransaction());
        assertEquals(1, functions.length);
        assertEquals(true, functions[0] instanceof PushdownFunction);
        assertEquals("myPushdownFunction", functions[0].getName(getTransaction())); //$NON-NLS-1$

        ProcedureResultSet rSet = ((PushdownFunction)functions[0]).getResultSet(getTransaction());
        TabularResultSet tabularResultSet = null;
        if(rSet instanceof TabularResultSet) {
            tabularResultSet = (TabularResultSet)rSet;
        }

        ResultSetColumn[] cols = tabularResultSet.getColumns(getTransaction());
        assertEquals(1,cols.length);
        assertEquals("myColumn", cols[0].getName(getTransaction())); //$NON-NLS-1$

        String nameInSource = cols[0].getNameInSource(getTransaction());
        assertEquals("myNameInSource", nameInSource); //$NON-NLS-1$
    }

    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	setup("commandFiles","addResSetColumns.cmd");
    	final String[] commands = { "cd myColumn1" };
    	final CommandResult result = execute( commands );
        assertCommandResultOk(result);

    	candidates.add(ResultSetColumnShellCommand.NAME_IN_SOURCE);
    	assertTabCompletion("set-property NA", candidates);
    	assertTabCompletion("set-property na", candidates);
    }
}
