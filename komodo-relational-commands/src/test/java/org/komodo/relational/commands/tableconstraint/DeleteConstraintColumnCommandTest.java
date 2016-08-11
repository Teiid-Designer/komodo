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
package org.komodo.relational.commands.tableconstraint;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.TableConstraint;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link DeleteConstraintColumnCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class DeleteConstraintColumnCommandTest extends AbstractCommandTest {

    @Test
    public void testDelete1() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-vdb myVdb vdbPath",
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
            "add-column myCol1",
            "add-column myCol2",
            "add-foreign-key myForeignKey /workspace/myVdb/refModel/refTable",
            "cd myForeignKey",
            "add-column /workspace/myVdb/myModel/myTable/myCol1",
            "add-column /workspace/myVdb/myModel/myTable/myCol2",
            "commit",
            "delete-column /workspace/myVdb/myModel/myTable/myCol1",
            "commit"
            };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);

        Model[] models = vdbs[0].getModels(getTransaction());
        assertEquals(2, models.length);

        Model modelWithFk = models[0];
        if(!modelWithFk.getName(getTransaction()).equals("myModel")) { //$NON-NLS-1$
            modelWithFk = models[1];
        }
        assertEquals("myModel", modelWithFk.getName(getTransaction())); //$NON-NLS-1$

        Table[] tables = modelWithFk.getTables(getTransaction());
        assertEquals(1, tables.length);
        assertEquals("myTable", tables[0].getName(getTransaction())); //$NON-NLS-1$

        ForeignKey[] fks = tables[0].getForeignKeys(getTransaction());
        assertEquals(1, fks.length);
        assertEquals("myForeignKey", fks[0].getName(getTransaction())); //$NON-NLS-1$

        TableConstraint tConstraint = fks[0];
        Column[] cols = tConstraint.getColumns(getTransaction());
        assertEquals(1, cols.length);
        assertEquals("myCol2", cols[0].getName(getTransaction())); //$NON-NLS-1$
    }

}
