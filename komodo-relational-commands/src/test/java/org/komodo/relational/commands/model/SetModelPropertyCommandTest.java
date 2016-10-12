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
package org.komodo.relational.commands.model;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import java.util.ArrayList;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetModelPropertyCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class SetModelPropertyCommandTest extends AbstractCommandTest {

    @Test
    public void testSetProperty1() throws Exception {
        final String[] commands = {
            "create-vdb myVdb vdbPath",
            "cd myVdb",
            "add-model myModel",
            "cd myModel",
            "set-property description myDescription" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);

        Model[] models = vdbs[0].getModels(getTransaction());
        assertEquals(1, models.length);
        assertEquals("myModel", models[0].getName(getTransaction())); //$NON-NLS-1$

        assertEquals("myDescription", models[0].getDescription(getTransaction())); //$NON-NLS-1$
    }

    @Test
    public void testTabCompleter()throws Exception{
    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	setup("commandFiles","addModels.cmd");
    	final String[] commands = { "cd myModel1" };
    	final CommandResult result = execute( commands );
        assertCommandResultOk(result);

    	candidates.add(ModelShellCommand.MODEL_TYPE);
    	candidates.add(ModelShellCommand.METADATA_TYPE);
    	assertTabCompletion("set-property m", candidates);
    	assertTabCompletion("set-property M", candidates);
    }

    @Test
    public void shouldSetModelTypeToPhyscalIgnoringTextCase() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model myModel",
                                    "cd myModel",
                                    "set-property modelType pHySiCal" };

        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 1 ) );

        final Model[] models = vdbs[ 0 ].getModels( getTransaction() );
        assertThat( models.length, is( 1 ) );
        assertThat( models[ 0 ].getModelType( getTransaction() ), is( Model.Type.PHYSICAL ) );
    }

    @Test
    public void shouldSetModelTypeToVirtualIgnoringTextCase() throws Exception {
        final String[] commands = { "create-vdb myVdb vdbPath",
                                    "cd myVdb",
                                    "add-model myModel",
                                    "cd myModel",
                                    "set-property modelType ViRtUaL" };

        final CommandResult result = execute( commands );
        assertCommandResultOk( result );

        final WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        final Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );
        assertThat( vdbs.length, is( 1 ) );

        final Model[] models = vdbs[ 0 ].getModels( getTransaction() );
        assertThat( models.length, is( 1 ) );
        assertThat( models[ 0 ].getModelType( getTransaction() ), is( Model.Type.VIRTUAL ) );
    }

}
