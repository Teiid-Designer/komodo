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
package org.komodo.relational.commands.schema;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.FileWriter;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.relational.model.Schema;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.test.utils.TestUtilities;

/**
 * Test Class to test Schema ExportCommand
 *
 */
@SuppressWarnings("javadoc")
public class ExportCommandTest extends AbstractCommandTest {

    private final static String TWITTER_VIEW_MODEL_DDL = EMPTY_STRING + 
                                                         "CREATE VIRTUAL PROCEDURE getTweets(IN query varchar) RETURNS TABLE " +  //$NON-NLS-1$ 
                                                         "(created_on varchar(25), from_user varchar(25), to_user varchar(25), profile_image_url " +  //$NON-NLS-1$ 
                                                         "varchar(25), source varchar(25), text varchar(140))" + NEW_LINE +  //$NON-NLS-1$ 
                                                         "AS" + NEW_LINE +  //$NON-NLS-1$ 
                                                         "SELECT tweet.* FROM (EXEC twitter.invokeHTTP(action => 'GET', endpoint => QUERYSTRING('', query AS q))) AS w, XMLTABLE('results' PASSING JSONTOXML('myxml', w.result) COLUMNS created_on string PATH 'created_at',  from_user string PATH 'from_user',  to_user string PATH 'to_user',  profile_image_url string PATH 'profile_image_url',  source string PATH 'source',  text string PATH 'text') AS tweet;" + NEW_LINE + //$NON-NLS-1$  
                                                         NEW_LINE + "CREATE VIEW Tweet" + NEW_LINE + "AS" + NEW_LINE + "SELECT * FROM twitterview.getTweets;"; //$NON-NLS-1$  //$NON-NLS-2$ //$NON-NLS-3$ 

    /**
	 * Test for ExportCommand
	 */
	public ExportCommandTest( ) {
		super();
	}

    private KomodoObject addSchemaExample() throws Exception {
        Schema schema = null;

        try {
            createInitialTransaction();
            KomodoObject kWorkspace = _repo.komodoWorkspace(uow);
            WorkspaceManager manager = WorkspaceManager.getInstance(_repo);
            schema = manager.createSchema(uow, kWorkspace, "TestTweetSchema");  //$NON-NLS-1$
            schema.setRendition(uow, TWITTER_VIEW_MODEL_DDL);
        } finally {
            commit();
        }

        assertNotNull(schema);

        try {
            createInitialTransaction();
            traverse(uow, schema.getAbsolutePath());
        } finally {
            commit();
        }

        return schema;
    }
    
    /**
     * Test export of Virtual model -> ddl
     *
     * @throws Exception
     */
    @Test
    public void testExportCommandSchemaRendition() throws Exception {
        FileWriter writer = null;

        //
        // Create the vdb in the repository
        //
        addSchemaExample();

        try {
            //
            // Create the export command instructions file
            //
            File exportCmdFile = File.createTempFile("TestExportCommand", ".txt");  //$NON-NLS-1$ //$NON-NLS-2$
            exportCmdFile.deleteOnExit();

            //
            // Create the export destination file, ensuring it does not already exist
            //
            File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt");  //$NON-NLS-1$ //$NON-NLS-2$
            exportDest.deleteOnExit();
            if (exportDest.exists())
                exportDest.delete();

            //
            // Write the instructions to the file
            //
            createInitialTransaction();
            writer = new FileWriter(exportCmdFile);
            writer.write("workspace" + NEW_LINE);  //$NON-NLS-1$
            writer.write("cd TestTweetSchema" + NEW_LINE);  //$NON-NLS-1$
            writer.write("export-ddl " + exportDest.getAbsolutePath() + NEW_LINE);  //$NON-NLS-1$
            writer.close();

            //
            // Setup the export instructions
            //
            setup(exportCmdFile.getAbsolutePath(), ExportCommand.class);

            //
            // Execute the commands
            //
            CommandResult result = execute();
            assertCommandResultOk(result);

            assertTrue(exportDest.exists());

            String exportContents = TestUtilities.fileToString(exportDest);
            assertEquals(TWITTER_VIEW_MODEL_DDL + NEW_LINE, exportContents);

        } finally {
            commit();

            if (writer != null)
                writer.close();
        }
    }
    
}
