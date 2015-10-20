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
import java.io.File;
import java.io.FileWriter;
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
 * Test Class to test DeleteParameterCommand
 *
 */
@SuppressWarnings("javadoc")
public class DeleteConstraintColumnCommandTest extends AbstractCommandTest {

    /**
	 * Test for DeleteParameterCommand
	 */
	public DeleteConstraintColumnCommandTest( ) {
		super();
	}

    @Test
    public void testDelete1() throws Exception {
        File cmdFile = File.createTempFile("TestCommand", ".txt");  //$NON-NLS-1$  //$NON-NLS-2$
        cmdFile.deleteOnExit();
        
        FileWriter writer = new FileWriter(cmdFile);
        writer.write("set-auto-commit false" + NEW_LINE);  //$NON-NLS-1$
        writer.write("workspace" + NEW_LINE);  //$NON-NLS-1$
        writer.write("create-vdb myVdb vdbPath" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myVdb" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-model refModel" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd refModel" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-table refTable" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd refTable" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-column refCol1" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-column refCol2" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd ../.." + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-model myModel" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myModel" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-table myTable" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myTable" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-column myCol1" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-column myCol2" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-foreign-key myForeignKey /workspace/myVdb/refModel/refTable" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myForeignKey" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-column /workspace/myVdb/myModel/myTable/myCol1" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-column /workspace/myVdb/myModel/myTable/myCol2" + NEW_LINE);  //$NON-NLS-1$
        writer.write("commit" + NEW_LINE);  //$NON-NLS-1$
        writer.write("delete-column /workspace/myVdb/myModel/myTable/myCol1" + NEW_LINE);  //$NON-NLS-1$
        writer.write("commit" + NEW_LINE);  //$NON-NLS-1$
        writer.close();

        setup(cmdFile.getAbsolutePath(), AddConstraintColumnCommand.class);

        CommandResult result = execute();
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Vdb[] vdbs = wkspMgr.findVdbs(uow);
        
        assertEquals(1, vdbs.length);
        
        Model[] models = vdbs[0].getModels(uow);
        assertEquals(2, models.length);
        
        Model modelWithFk = models[0];
        if(!modelWithFk.getName(uow).equals("myModel")) { //$NON-NLS-1$
            modelWithFk = models[1];
        }
        assertEquals("myModel", modelWithFk.getName(uow)); //$NON-NLS-1$
        
        Table[] tables = modelWithFk.getTables(uow);
        assertEquals(1, tables.length);
        assertEquals("myTable", tables[0].getName(uow)); //$NON-NLS-1$
        
        ForeignKey[] fks = tables[0].getForeignKeys(uow);
        assertEquals(1, fks.length);
        assertEquals("myForeignKey", fks[0].getName(uow)); //$NON-NLS-1$
        
        TableConstraint tConstraint = fks[0];
        Column[] cols = tConstraint.getColumns(uow);
        assertEquals(1, cols.length);
        assertEquals("myCol2", cols[0].getName(uow)); //$NON-NLS-1$
    }

}
