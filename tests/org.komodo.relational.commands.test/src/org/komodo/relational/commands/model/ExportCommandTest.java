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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.FileWriter;
import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.test.utils.TestUtilities;

/**
 * Test Class to test ExportCommand
 *
 */
@SuppressWarnings("javadoc")
public class ExportCommandTest extends AbstractCommandTest {

    private final static String ALL_ELEMENTS_MODEL_TWO_DDL = EMPTY_STRING + "CREATE VIEW Test" + NEW_LINE + //$NON-NLS-1$ 
                                                             "AS" + NEW_LINE + "SELECT * FROM Test.getTest;";  //$NON-NLS-1$ //$NON-NLS-2$

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

    private KomodoObject addTweetVdbExample() throws KException, Exception {
        KomodoObject tweet = null;

        try {
            createInitialTransaction();
            KomodoObject kWorkspace = _repo.komodoWorkspace(uow);
            tweet = TestUtilities.createTweetExampleNode(uow, kWorkspace);
        } finally {
            commit();
        }

        assertNotNull(tweet);

        try {
            createInitialTransaction();
            traverse(uow, tweet.getAbsolutePath());
        } finally {
            commit();
        }

        return tweet;
    }

    private KomodoObject addAllElementsVdbExample() throws Exception {
        KomodoObject tweet = null;

        try {
            createInitialTransaction();
            KomodoObject kWorkspace = _repo.komodoWorkspace(uow);
            tweet = TestUtilities.createAllElementsExampleNode(uow, kWorkspace);
        } finally {
            commit();
        }

        assertNotNull(tweet);

        try {
            createInitialTransaction();
            traverse(uow, tweet.getAbsolutePath());
        } finally {
            commit();
        }

        return tweet;
    }

    /**
     * Test export of Virtual model -> ddl
     *
     * @throws Exception
     */
    @Test
    public void testExportCommandTweetViewModel() throws Exception {
        FileWriter writer = null;

        //
        // Create the vdb in the repository
        //
        KomodoObject tweetVdb = addTweetVdbExample();

        try {
            //
            // Create the export command instructions file
            //
            File exportCmdFile = File.createTempFile("TestExportCommand", ".txt");  //$NON-NLS-1$  //$NON-NLS-2$
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
            writer.write("cd " + tweetVdb.getName(uow) + NEW_LINE);  //$NON-NLS-1$
            writer.write("cd twitterview" + NEW_LINE);  //$NON-NLS-1$
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
            assertTrue(exportDest.exists());
            assertTrue(result.isOk());

            String exportContents = TestUtilities.fileToString(exportDest);
            assertEquals(TWITTER_VIEW_MODEL_DDL + NEW_LINE, exportContents);

        } finally {
            commit();

            if (writer != null)
                writer.close();
        }
    }
    
    /**
     * Test export of Physical model -> xml
     *
     * @throws Exception
     */
    @Test
    public void testExportCommandAllElementsVdbModelOne() throws Exception {
        FileWriter writer = null;

        //
        // Create the vdb in the repository
        //
        KomodoObject allElementsVdb = addAllElementsVdbExample();

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
            writer.write("cd " + allElementsVdb.getName(uow) + NEW_LINE);  //$NON-NLS-1$
            writer.write("cd model-one" + NEW_LINE);  //$NON-NLS-1$
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
            assertTrue(exportDest.exists());
            assertTrue(!result.isOk());
            
            String message = result.getMessage();
            assertEquals("There is no DDL content to output.",message); //$NON-NLS-1$
        } finally {
            commit();

            if (writer != null)
                writer.close();
        }
    }

    /**
     * Test export of Virtual model -> ddl
     *
     * @throws Exception
     */
    @Test
    public void testExportCommandAllElementsVdbModelTwo() throws Exception {
        FileWriter writer = null;

        //
        // Create the vdb in the repository
        //
        KomodoObject allElementsVdb = addAllElementsVdbExample();

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
            writer.write("cd " + allElementsVdb.getName(uow) + NEW_LINE);  //$NON-NLS-1$
            writer.write("cd model-two" + NEW_LINE);  //$NON-NLS-1$
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
            assertEquals(ALL_ELEMENTS_MODEL_TWO_DDL + NEW_LINE, exportContents);

        } finally {
            commit();

            if (writer != null)
                writer.close();
        }
    }

}
