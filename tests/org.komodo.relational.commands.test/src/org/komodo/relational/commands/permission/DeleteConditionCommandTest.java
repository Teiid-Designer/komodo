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
package org.komodo.relational.commands.permission;

import static org.junit.Assert.assertEquals;
import java.io.File;
import java.io.FileWriter;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test DeleteConditionCommand
 *
 */
@SuppressWarnings("javadoc")
public class DeleteConditionCommandTest extends AbstractCommandTest {

    @Test
    public void testDelete1() throws Exception {
        File cmdFile = File.createTempFile("TestCommand", ".txt");  //$NON-NLS-1$  //$NON-NLS-2$
        cmdFile.deleteOnExit();

        FileWriter writer = new FileWriter(cmdFile);
        writer.write("workspace" + NEW_LINE);  //$NON-NLS-1$
        writer.write("create-vdb myVdb vdbPath" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myVdb" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-data-role myDataRole" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myDataRole" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-permission myPermission" + NEW_LINE);  //$NON-NLS-1$
        writer.write("cd myPermission" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-condition myCondition1" + NEW_LINE);  //$NON-NLS-1$
        writer.write("add-condition myCondition2" + NEW_LINE);  //$NON-NLS-1$
        writer.write("delete-condition myCondition1" + NEW_LINE);  //$NON-NLS-1$
        writer.close();

        setup( cmdFile.getAbsolutePath() );

        CommandResult result = execute();
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());
        assertEquals(1, vdbs.length);

        DataRole[] dataRoles = vdbs[0].getDataRoles(getTransaction());
        assertEquals(1, dataRoles.length);

        Permission[] permissions = dataRoles[0].getPermissions(getTransaction());
        assertEquals(1, permissions.length);
        assertEquals("myPermission", permissions[0].getName(getTransaction())); //$NON-NLS-1$

        Condition[] conditions = permissions[0].getConditions(getTransaction());
        assertEquals(1, conditions.length);
        assertEquals("myCondition2", conditions[0].getName(getTransaction())); //$NON-NLS-1$
    }

}
