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
package org.komodo.relational.commands.datarole;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.vdb.DataRole;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test DeleteMappedRoleCommand
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class DeleteMappedRoleCommandTest extends AbstractCommandTest {

    @Test
    public void testSetProperty1() throws Exception {
        final String[] commands = { "workspace",
            "create-vdb myVdb vdbPath",
            "cd myVdb",
            "add-data-role myDataRole",
            "cd myDataRole",
            "add-mapped-role myMappedRole1",
            "add-mapped-role myMappedRole2",
            "delete-mapped-role myMappedRole1"};
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());
        assertEquals(1, vdbs.length);

        DataRole[] dataRoles = vdbs[0].getDataRoles(getTransaction());
        assertEquals(1, dataRoles.length);

        String[] mappedRoles = dataRoles[0].getMappedRoles(getTransaction());
        assertEquals(1, mappedRoles.length);

        assertEquals("myMappedRole2", mappedRoles[0]); //$NON-NLS-1$
    }

    @Test
    public void testTabCompleter()throws Exception{

    	setup("commandFiles","addMappedRoles.cmd");
    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	candidates.add("myMappedRole1");
    	candidates.add("myMappedRole2");

    	assertTabCompletion("delete-mapped-role m", candidates);

    	candidates.add("MyMappedRole3");
        assertTabCompletion("delete-mapped-role ", candidates);
    }

    @Test( expected = AssertionError.class )
    public void shouldFailDeletingMappedRoleThatDoesNotExist() throws Exception {
        final String[] commands = { "workspace",
                                    "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-data-role myDataRole",
                                    "cd myDataRole",
                                    "add-mapped-role myMappedRole1",
                                    "delete-mapped-role bogusMappedRole" };
        execute( commands );
    }

}
