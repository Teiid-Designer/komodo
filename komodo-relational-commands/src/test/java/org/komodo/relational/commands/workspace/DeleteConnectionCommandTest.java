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
package org.komodo.relational.commands.workspace;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link DeleteConnectionCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class DeleteConnectionCommandTest extends AbstractCommandTest {

    @Test
    public void testDeleteTeiid1() throws Exception {
        final String[] commands = { "set-auto-commit false",
                                    "create-connection testSource1",
                                    "create-connection testSource2",
                                    "commit",
                                    "delete-connection testSource1",
                                    "commit" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Connection[] connections = wkspMgr.findConnections(getTransaction());

        assertEquals(1,connections.length);
        assertEquals("testSource2",connections[0].getName(getTransaction()));
    }

}
