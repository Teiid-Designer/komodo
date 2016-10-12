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
package org.komodo.relational.commands.condition;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.vdb.Condition;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Permission;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetConditionPropertyCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class SetConditionPropertyCommandTest extends AbstractCommandTest {

    @Test
    public void testSetProperty1() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-permission myPermission",
                                    "cd myPermission",
                                    "add-condition myCondition",
                                    "cd myCondition",
                                    "set-property constraint true" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());
        assertEquals(1, vdbs.length);

        DataRole[] dataRoles = vdbs[0].getDataRoles(getTransaction());
        assertEquals(1, dataRoles.length);

        Permission[] permissions = dataRoles[0].getPermissions(getTransaction());
        assertEquals(1, permissions.length);
        assertEquals("myPermission",permissions[0].getName(getTransaction()));

        Condition[] conditions = permissions[0].getConditions(getTransaction());
        assertEquals(1, conditions.length);
        assertEquals("myCondition", conditions[0].getName(getTransaction()));

        assertEquals(true, conditions[0].isConstraint(getTransaction()));
    }

    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	setup("commandFiles","addConditions.cmd");
    	final String[] commands = { "cd myCondition1" };
    	final CommandResult result = execute( commands );
        assertCommandResultOk(result);


    	candidates.add(ConditionShellCommand.CONSTRAINT);
    	assertTabCompletion("set-property CON", candidates);

    }
}
