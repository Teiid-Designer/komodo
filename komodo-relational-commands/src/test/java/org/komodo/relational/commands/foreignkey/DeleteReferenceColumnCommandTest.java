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
package org.komodo.relational.commands.foreignkey;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link DeleteReferenceColumnCommand}.
 *
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class DeleteReferenceColumnCommandTest extends AbstractCommandTest {

    @Test
    public void testDelete1() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model refModel",
                                    "cd refModel",
                                    "add-table refTable",
                                    "cd refTable",
                                    "add-column refCol1",
                                    "add-column refCol2",
                                    "cd ../..",
                                    "add-model myModel",
                                    "cd myModel",
                                    "add-table myTable",
                                    "cd myTable",
                                    "add-foreign-key myForeignKey /workspace/myVdb/refModel/refTable",
                                    "cd myForeignKey",
                                    "add-ref-column /workspace/myVdb/refModel/refTable/refCol1",
                                    "add-ref-column /workspace/myVdb/refModel/refTable/refCol2",
                                    "commit", // need to commit since delete-ref-column uses search framework
                                    "delete-ref-column /workspace/myVdb/refModel/refTable/refCol1" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);

        Model[] models = vdbs[0].getModels(getTransaction());
        assertEquals(2, models.length);

        Model modelWithFk = models[0];
        if(!modelWithFk.getName(getTransaction()).equals("myModel")) {
            modelWithFk = models[1];
        }
        assertEquals("myModel", modelWithFk.getName(getTransaction()));

        Table[] tables = modelWithFk.getTables(getTransaction());
        assertEquals(1, tables.length);
        assertEquals("myTable", tables[0].getName(getTransaction()));

        ForeignKey[] fks = tables[0].getForeignKeys(getTransaction());
        assertEquals(1, fks.length);
        assertEquals("myForeignKey", fks[0].getName(getTransaction()));

        Column[] refCols = fks[0].getReferencesColumns(getTransaction());
        assertEquals(1, refCols.length);
        assertEquals("refCol2", refCols[0].getName(getTransaction()));
    }

    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	setup("commandFiles","addRefColumns.cmd");

    	candidates.add("/workspace/" + TEST_USER + "/myVDB1/myModel1/myTable2/myColumn1");
    	candidates.add("/workspace/" + TEST_USER + "/myVDB1/myModel1/myTable2/myColumn2");
    	assertTabCompletion("delete-ref-column /workspace/" + TEST_USER + "/myVDB1/myModel1/myTable2/myC", candidates);

    	candidates.add("/workspace/" + TEST_USER + "/myVDB1/myModel1/myTable2/MyColumn3");
    	assertTabCompletion("delete-ref-column ", candidates);

    }
}
