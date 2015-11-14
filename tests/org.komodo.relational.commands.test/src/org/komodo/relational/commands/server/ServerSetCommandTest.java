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
package org.komodo.relational.commands.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.KomodoObject;

/**
 * Test Class to test {@link ServerSetCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class ServerSetCommandTest extends AbstractCommandTest {

    @Test
    public void testSet() throws Exception {
        final String[] commands = {
            "set-auto-commit false",
            "create-teiid myTeiid",
            "commit",
            "set-server myTeiid",
            "commit",
            "show-global" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Teiid[] teiids = wkspMgr.findTeiids(getTransaction());

        assertEquals(1, teiids.length);
        assertEquals("myTeiid", teiids[0].getName(getTransaction()));

        // Make sure the server has been set
        boolean hasServerObject = false;
        KomodoObject serverObj = wsStatus.getStateObjects().get(ServerCommandProvider.SERVER_DEFAULT_KEY);
        if(serverObj!=null && Teiid.RESOLVER.resolvable(getTransaction(), serverObj)) {
            hasServerObject = true;
        }
        assertTrue(hasServerObject);
        assertEquals("myTeiid",serverObj.getName(getTransaction()));
    }

}
