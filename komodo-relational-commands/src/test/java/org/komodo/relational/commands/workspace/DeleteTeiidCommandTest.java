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

import java.util.ArrayList;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.teiid.Teiid;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link DeleteTeiidCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class DeleteTeiidCommandTest extends AbstractCommandTest {

    @Test
    public void testDeleteTeiid1() throws Exception {
        final String[] commands = { "set-auto-commit false",
                                    "create-teiid testTeiid1",
                                    "create-teiid testTeiid2",
                                    "commit",
                                    "delete-teiid testTeiid1",
                                    "commit" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Teiid[] teiids = wkspMgr.findTeiids(getTransaction());

        assertEquals(1,teiids.length);
        assertEquals("testTeiid2",teiids[0].getName(getTransaction()));
    }

    @Test
    public void testTabCompleter()throws Exception{

    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	candidates.add("myServer1");
    	candidates.add("myServer2");

    	setup("commandFiles","addServers.cmd");
    	assertTabCompletion("delete-teiid myS", candidates);

    	candidates.add("MyServer3");
    	assertTabCompletion("delete-teiid ", candidates);
    }
}
