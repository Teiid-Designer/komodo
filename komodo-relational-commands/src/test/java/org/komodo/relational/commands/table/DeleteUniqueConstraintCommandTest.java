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
package org.komodo.relational.commands.table;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link DeleteUniqueConstraintCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class DeleteUniqueConstraintCommandTest extends AbstractCommandTest {

    @Test
    public void testDelete1() throws Exception {
        final String[] commands = {
            "create-vdb myVdb vdbPath",
            "cd myVdb",
            "add-model myModel",
            "cd myModel",
            "add-table myTable",
            "cd myTable",
            "add-unique-constraint myUniqueConstraint1",
            "add-unique-constraint myUniqueConstraint2",
            "delete-unique-constraint myUniqueConstraint1" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);

        Model[] models = vdbs[0].getModels(getTransaction());
        assertEquals(1, models.length);
        assertEquals("myModel", models[0].getName(getTransaction())); //$NON-NLS-1$

        Table[] tables = models[0].getTables(getTransaction());
        assertEquals(1, tables.length);
        assertEquals("myTable", tables[0].getName(getTransaction())); //$NON-NLS-1$

        UniqueConstraint[] ucs = tables[0].getUniqueConstraints(getTransaction());
        assertEquals(1, ucs.length);
        assertEquals("myUniqueConstraint2", ucs[0].getName(getTransaction())); //$NON-NLS-1$
    }

    @Test
    public void testTabCompleter()throws Exception{

    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	candidates.add("myUniqueConstraint1");
    	candidates.add("myUniqueConstraint2");

    	setup("commandFiles","addUniqueConstraints.cmd");
    	assertTabCompletion("delete-unique-constraint myU", candidates);

    	candidates.add("MyUniqueConstraint3");
    	assertTabCompletion("delete-unique-constraint ", candidates);
    }

}
