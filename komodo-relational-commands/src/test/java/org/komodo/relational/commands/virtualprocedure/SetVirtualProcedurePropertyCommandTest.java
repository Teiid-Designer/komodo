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
package org.komodo.relational.commands.virtualprocedure;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.VirtualProcedure;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetVirtualProcedurePropertyCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class SetVirtualProcedurePropertyCommandTest extends AbstractCommandTest {

    @Test
    public void testSetProperty1() throws Exception {
        final String[] commands = {
            "create-vdb myVdb vdbPath",
            "cd myVdb",
            "add-model myModel",
            "cd myModel",
            "add-virtual-procedure myVirtualProcedure",
            "cd myVirtualProcedure",
            "set-property name-in-source myNameInSource" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(vdbs.length,1);

        Model[] models = vdbs[0].getModels(getTransaction());
        assertEquals(1, models.length);
        assertEquals("myModel", models[0].getName(getTransaction())); //$NON-NLS-1$

        Procedure[] procs = models[0].getProcedures(getTransaction());
        assertEquals(1, procs.length);
        assertEquals(true, procs[0] instanceof VirtualProcedure);
        assertEquals("myVirtualProcedure", procs[0].getName(getTransaction())); //$NON-NLS-1$

        assertEquals("myNameInSource", ((VirtualProcedure)procs[0]).getNameInSource(getTransaction())); //$NON-NLS-1$
    }

    @Test
    public void testTabCompleter()throws Exception{

    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	setup("commandFiles","addVirtualProcedures.cmd");
    	final String[] commands = { "cd myVirtualProcedure1" };
    	final CommandResult result = execute( commands );
    	assertCommandResultOk(result);

    	candidates.add(VirtualProcedureShellCommand.NAME_IN_SOURCE);

    	assertTabCompletion("set-property na", candidates);
        assertTabCompletion("set-property NA", candidates);
    }

}
