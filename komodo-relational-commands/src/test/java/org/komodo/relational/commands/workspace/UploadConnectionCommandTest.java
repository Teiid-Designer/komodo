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
import java.io.File;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link UploadConnectionCommand}.
 */
@SuppressWarnings( { "javadoc", "nls" } )
public final class UploadConnectionCommandTest extends AbstractCommandTest {

    private static final File UPLOAD_SOURCE = getResourceFile(UploadConnectionCommandTest.class, "dashboardDS.xml");

    @Test
    public void shouldUploadConnection() throws Exception {
        final String[] commands = { "upload-connection " + UPLOAD_SOURCE.getAbsolutePath() };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Connection[] sources = wkspMgr.findConnections(getTransaction());

        assertEquals(1, sources.length);
        assertEquals("DashboardDS", sources[0].getName(getTransaction()));
    }
    
    @Test( expected = AssertionError.class )
    public void shouldNotUploadConnectionIfExists() throws Exception {
        final String[] commands = { "create-connection DashboardDS ",
                                    "upload-connection " + UPLOAD_SOURCE.getAbsolutePath() };
        execute( commands );
    }
    
    @Test
    public void shouldUploadConnectionIfExistsWithOverwrite() throws Exception {
        final String[] commands = { "create-connection DashboardDS ",
                                    "upload-connection " + UPLOAD_SOURCE.getAbsolutePath() + " -o" };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Connection[] sources = wkspMgr.findConnections(getTransaction());

        assertEquals(1, sources.length);
        assertEquals("DashboardDS", sources[0].getName(getTransaction()));
    }

}
