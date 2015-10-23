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
package org.komodo.relational.commands;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test DeleteVdbCommand
 *
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class DeleteVdbCommandTest extends AbstractCommandTest {

    @Test
    public void testDeleteVdb1() throws Exception {
        final String[] commands = { "set-auto-commit false",
                                    "workspace",
                                    "create-vdb testVdb1 vdbPath",
                                    "create-vdb testVdb2 vdbPath",
                                    "commit",
                                    "delete-vdb testVdb1",
                                    "commit" };
        setup( commands );

        CommandResult result = execute();
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);
        assertEquals("testVdb2", vdbs[0].getName(getTransaction()));
    }

}
