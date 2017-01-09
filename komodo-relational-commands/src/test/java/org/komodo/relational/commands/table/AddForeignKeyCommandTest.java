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
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link AddForeignKeyCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class AddForeignKeyCommandTest extends AbstractCommandTest {

    @Test
    public void testAdd1() throws Exception {
        final String[] commands = {
            "create-vdb myVdb vdbPath",
            "cd myVdb",
            "add-model refModel",
            "cd refModel",
            "add-table refTable",
            "cd ..",
            "add-model myModel",
            "cd myModel",
            "add-table myTable",
            "cd myTable",
            "add-foreign-key myForeignKey /workspace/myVdb/refModel/refTable" };
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
    }

}
