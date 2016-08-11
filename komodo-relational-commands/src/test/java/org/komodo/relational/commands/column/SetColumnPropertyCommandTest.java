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
package org.komodo.relational.commands.column;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetColumnPropertyCommand}.
 */
@SuppressWarnings( {"javadoc","nls"} )
public final class SetColumnPropertyCommandTest extends AbstractCommandTest {

    @Test
    public void testSetProperty1() throws Exception {
        final String[] commands = {
            "create-vdb myVdb vdbPath",
            "cd myVdb",
            "add-model myModel",
            "cd myModel",
            "add-table myTable",
            "cd myTable",
            "add-column myColumn",
            "cd myColumn",
            "set-property NAMEINSOURCE myNameInSource" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(vdbs.length,1);

        Model[] models = vdbs[0].getModels(getTransaction());
        assertEquals(1, models.length,1);
        assertEquals("myModel", models[0].getName(getTransaction())); //$NON-NLS-1$

        Table[] tables = models[0].getTables(getTransaction());
        assertEquals(1, tables.length);
        assertEquals("myTable", tables[0].getName(getTransaction())); //$NON-NLS-1$

        Column[] columns = tables[0].getColumns(getTransaction());
        assertEquals(1, columns.length);
        assertEquals("myColumn", columns[0].getName(getTransaction())); //$NON-NLS-1$

        String nameInSource = columns[0].getNameInSource(getTransaction());
        assertEquals("myNameInSource", nameInSource); //$NON-NLS-1$
    }

    @Test
    public void testTabCompleter()throws Exception{

    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	setup("commandFiles","addColumns.cmd");
    	final String[] commands = { "cd myColumn1" };
    	final CommandResult result = execute( commands );
    	assertCommandResultOk(result);

    	candidates.add("NAMEINSOURCE");
    	candidates.add("NATIVE_TYPE");

    	assertTabCompletion("set-property na", candidates);
        assertTabCompletion("set-property NA", candidates);
    }

}
