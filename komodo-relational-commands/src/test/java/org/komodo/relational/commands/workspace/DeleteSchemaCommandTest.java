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
import org.komodo.relational.model.Schema;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link DeleteSchemaCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class DeleteSchemaCommandTest extends AbstractCommandTest {

    @Test
    public void testDeleteSchema1() throws Exception {
        final String[] commands = { "set-auto-commit false",
                                    "create-schema testSchema1",
                                    "create-schema testSchema2",
                                    "commit",
                                    "delete-schema testSchema1",
                                    "commit" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Schema[] schemas = wkspMgr.findSchemas(getTransaction());

        assertEquals(1, schemas.length);
        assertEquals("testSchema2", schemas[0].getName(getTransaction()));
    }

    @Test
    public void testTabCompleter()throws Exception{

    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	candidates.add("mySchema1");
    	candidates.add("mySchema2");

    	setup("commandFiles","addSchemas.cmd");
    	assertTabCompletion("delete-schema myS", candidates);

    	candidates.add("MySchema3");
    	assertTabCompletion("delete-schema ", candidates);
    }

}
