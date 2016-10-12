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
package org.komodo.relational.commands.view;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.View;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link DeleteColumnCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public final class DeleteColumnCommandTest extends AbstractCommandTest {

    @Test
    public void testDelete1() throws Exception {
        final String[] commands = {
            "create-vdb myVdb vdbPath",
            "cd myVdb",
            "add-model myModel",
            "cd myModel",
            "add-view myView",
            "cd myView",
            "add-column myColumn1",
            "add-column myColumn2",
            "delete-column myColumn1" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);

        Model[] models = vdbs[0].getModels(getTransaction());
        assertEquals(1, models.length);
        assertEquals("myModel", models[0].getName(getTransaction())); //$NON-NLS-1$

        View[] views = models[0].getViews(getTransaction());
        assertEquals(1, views.length);
        assertEquals("myView", views[0].getName(getTransaction())); //$NON-NLS-1$

        Column[] columns = views[0].getColumns(getTransaction());
        assertEquals(1, columns.length);
        assertEquals("myColumn2", columns[0].getName(getTransaction())); //$NON-NLS-1$
    }

    @Test
    public void testTabCompleter()throws Exception{

    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	candidates.add("myColumn1");
    	candidates.add("myColumn2");

    	setup("commandFiles","addViewColumns.cmd");
    	assertTabCompletion("delete-column myCol", candidates);

    	candidates.add("MyColumn3");
    	assertTabCompletion("delete-column ", candidates);
    }
}
