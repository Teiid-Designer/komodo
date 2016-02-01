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
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.commands.workspace.UploadVdbCommandTest;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;

/**
 * Test Class to test {@link UploadModelCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class UploadModelCommandTest extends AbstractCommandTest {

    private static final String UPLOAD_MODEL = UploadVdbCommandTest.class.getClassLoader().getResource("PartsOracle.ddl").getFile();

    @Test
    public void shouldUploadPartsOraclePhysical() throws Exception {
        final String[] commands = { "workspace",
                                    "create-vdb testVdb vdbPath",
                                    "cd testVdb",
                                    "upload-model myModel PHYSICAL " + UPLOAD_MODEL };
        final CommandResult result = execute( commands );
        assertCommandResultOk(result);

        WorkspaceManager wkspMgr = WorkspaceManager.getInstance(_repo);
        Vdb[] vdbs = wkspMgr.findVdbs( getTransaction() );

        assertEquals(1, vdbs.length);
        assertEquals("testVdb", vdbs[0].getName( getTransaction() ));

        // Make sure model was created
        assertEquals(1, vdbs[0].getModels( getTransaction() ).length);
        assertEquals("myModel", vdbs[0].getModels( getTransaction() )[0].getName( getTransaction() ));

        // Should be 5 tables in the model
        assertEquals(5, vdbs[0].getModels(getTransaction())[0].getTables(getTransaction()).length);
    }

}
