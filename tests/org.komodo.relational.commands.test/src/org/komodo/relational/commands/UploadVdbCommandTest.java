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
 * Test Class to test UploadVdbCommand
 *
 */
@SuppressWarnings( { "javadoc", "nls" } )
public class UploadVdbCommandTest extends AbstractCommandTest {

    private static final String UPLOAD_VDB = "./resources/AzureService-vdb.xml";

    @Test
    public void shouldUploadVdb1() throws Exception {
        final String[] commands = { "workspace",
                                    "upload-vdb myVdb " + UPLOAD_VDB };
        setup( commands );

        CommandResult result = execute();
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Vdb[] vdbs = wkspMgr.findVdbs(getTransaction());

        assertEquals(1, vdbs.length);
        assertEquals("myVdb", vdbs[0].getName(getTransaction()));
    }

}
