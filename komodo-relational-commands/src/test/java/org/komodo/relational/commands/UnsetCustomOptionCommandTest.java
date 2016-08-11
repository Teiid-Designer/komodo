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
import static org.junit.Assert.assertNotNull;
import java.util.ArrayList;
import org.junit.Test;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Table;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link SetCustomPropertyCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class UnsetCustomOptionCommandTest extends CustomOptionCommandTest {

    @Test
    public void testCustomOptionTable() throws Exception {
        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        setup("commandFiles","addCustomOptions.cmd");
        final String[] commands = {
            "unset-custom-option myCustomOption1",
            "unset-custom-option My\"Custom\"Option3"};
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        String[][] expectedOptions={
        {"myCustomOption2", "myValue2"},
        {"MyCustomOption3", "myValue3"},
        {"my.custom.option", "myValue1"},};

        Vdb vdb=getVDB("myVDB1",wkspMgr);
        assertNotNull(vdb);
        Model[] models=vdb.getModels(getTransaction(), "myModel1");
        assertEquals(1, models.length);
        Table[] tables=models[0].getTables(getTransaction(), "myTable1");
        assertEquals(1, tables.length);
        assertCustomOptions(tables[0],expectedOptions);
    }

    @Test
    public void testTabCompleter()throws Exception{

        ArrayList<CharSequence> candidates=new ArrayList<>();
        candidates.add("myCustomOption1");
        candidates.add("myCustomOption2");
        candidates.add("my.custom.option");

        setup("commandFiles","addCustomOptions.cmd");
        assertTabCompletion("unset-custom-option my", candidates);

        candidates.add("MyCustomOption3");
        candidates.add("My\"Custom\"Option3");
        assertTabCompletion("unset-custom-option ", candidates);
    }

}
