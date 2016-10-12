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
package org.komodo.relational.commands.vdb;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetVdbPropertyCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class SetVdbPropertyCommandTest extends AbstractCommandTest {

    @Test
    public void testSetProperty1() throws Exception {
        final String[] commands = {
            "create-vdb testVdb vdbPath",
            "cd testVdb",
            "set-property version 3" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);
        assertEquals("testVdb", vdbs[0].getName(getTransaction())); //$NON-NLS-1$

        assertEquals(3, vdbs[0].getVersion(getTransaction()));
    }

    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	setup("commandFiles","addVDBs.cmd");
    	final String[] commands = { "cd myVDB1" };
    	final CommandResult result = execute( commands );
        assertCommandResultOk(result);

    	candidates.add(VdbShellCommand.VERSION);
    	assertTabCompletion("set-property Ver", candidates);
    	assertTabCompletion("set-property ver", candidates);
    }
}
