/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.shell;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStream;
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.relational.model.Schema;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.RepositoryImpl;
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.commands.core.ExportCommand;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.test.utils.TestUtilities;
import org.w3c.dom.Document;

/**
 * Test Class to test ExportCommand
 *
 */
@SuppressWarnings( {"nls", "javadoc"} )
public class ExportCommandTest extends AbstractCommandTest {

    private final static String ALL_ELEMENTS_MODEL_TWO_DDL = EMPTY_STRING + "CREATE VIEW Test" + NEW_LINE + "AS" + NEW_LINE + "SELECT * FROM Test.getTest;";

    private final static String TWITTER_VIEW_MODEL_DDL = EMPTY_STRING + "CREATE VIRTUAL PROCEDURE getTweets(IN query varchar) RETURNS TABLE " + "(created_on varchar(25), from_user varchar(25), to_user varchar(25), profile_image_url " + "varchar(25), source varchar(25), text varchar(140))" + NEW_LINE + "AS" + NEW_LINE + "SELECT tweet.* FROM (EXEC twitter.invokeHTTP(action => 'GET', endpoint => QUERYSTRING('', query AS q))) AS w, XMLTABLE('results' PASSING JSONTOXML('myxml', w.result) COLUMNS created_on string PATH 'created_at',  from_user string PATH 'from_user',  to_user string PATH 'to_user',  profile_image_url string PATH 'profile_image_url',  source string PATH 'source',  text string PATH 'text') AS tweet;" + NEW_LINE + NEW_LINE + "CREATE VIEW Tweet" + NEW_LINE + "AS" + NEW_LINE + "SELECT * FROM twitterview.getTweets;";

    private String convertToContextPath(KomodoObject kObject) {
        return kObject.getAbsolutePath().replaceAll(RepositoryImpl.WORKSPACE_ROOT,
                                                                                  FORWARD_SLASH +
                                                                                  WorkspaceContext.WORKSPACE_ROOT_DISPLAY_NAME);
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

    private KomodoObject addSchemaExample() throws Exception {
        Schema schema = null;

        try {
            createInitialTransaction();
            KomodoObject kWorkspace = _repo.komodoWorkspace(uow);
            WorkspaceManager manager = WorkspaceManager.getInstance(_repo);
            schema = manager.createSchema(uow, kWorkspace, "TestTweetSchema");
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

    @Ignore
    @Test
    public void testExportCommandTweetVdb() throws Exception {
        FileWriter writer = null;

        //
        // Create the vdb in the repository
        //
        KomodoObject tweetVdb = addTweetVdbExample();

        try {
            //
            // Create the export command instructions file
            //
            File exportCmdFile = File.createTempFile("TestExportCommand", ".txt");
            exportCmdFile.deleteOnExit();

            //
            // Create the export destination file, ensuring it does not already exist
            //
            File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt");
            exportDest.deleteOnExit();
            if (exportDest.exists())
                exportDest.delete();

            //
            // Write the instructions to the file
            //
            createInitialTransaction();
            writer = new FileWriter(exportCmdFile);
            String path = convertToContextPath(tweetVdb);
            writer.write("export " + path + SPACE + exportDest.getAbsolutePath() + NEW_LINE);
            writer.close();

            //
            // Setup the export instructions
            //
            setup(exportCmdFile.getAbsolutePath(), ExportCommand.class);

            //
            // Execute the commands
            //
            execute();

            assertTrue(exportDest.exists());

            Document vdbDocument = TestUtilities.createDocument(new FileInputStream(exportDest));
            assertNotNull(vdbDocument);

            InputStream tweetExample = TestUtilities.tweetExample();
            Document tweetDocument = TestUtilities.createDocument(tweetExample);

            TestUtilities.compareDocuments(tweetDocument, vdbDocument);

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
            File exportCmdFile = File.createTempFile("TestExportCommand", ".txt");
            exportCmdFile.deleteOnExit();

            //
            // Create the export destination file, ensuring it does not already exist
            //
            File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt");
            exportDest.deleteOnExit();
            if (exportDest.exists())
                exportDest.delete();

            //
            // Write the instructions to the file
            //
            createInitialTransaction();
            writer = new FileWriter(exportCmdFile);
            writer.write("cd /workspace" + NEW_LINE);
            writer.write("cd " + tweetVdb.getName(uow) + NEW_LINE);
            writer.write("export twitterview " + exportDest.getAbsolutePath() + NEW_LINE);
            writer.close();

            //
            // Setup the export instructions
            //
            setup(exportCmdFile.getAbsolutePath(), ExportCommand.class);

            //
            // Execute the commands
            //
            execute();

            assertTrue(exportDest.exists());

            String exportContents = TestUtilities.fileToString(exportDest);
            assertEquals(TWITTER_VIEW_MODEL_DDL + NEW_LINE, exportContents);

        } finally {
            commit();

            if (writer != null)
                writer.close();
        }
    }

    @Ignore
    @Test
    public void testExportCommandAllElementsVdb() throws Exception {
        FileWriter writer = null;

        //
        // Create the vdb in the repository
        //
        KomodoObject allElementsVdb = addAllElementsVdbExample();

        try {
            //
            // Create the export command instructions file
            //
            File exportCmdFile = File.createTempFile("TestExportCommand", ".txt");
            exportCmdFile.deleteOnExit();

            //
            // Create the export destination file, ensuring it does not already exist
            //
            File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt");
            exportDest.deleteOnExit();
            if (exportDest.exists())
                exportDest.delete();

            //
            // Write the instructions to the file
            //
            createInitialTransaction();
            writer = new FileWriter(exportCmdFile);
            String path = convertToContextPath(allElementsVdb);
            writer.write("export " + path + SPACE + exportDest.getAbsolutePath() + NEW_LINE);
            writer.close();

            //
            // Setup the export instructions
            //
            setup(exportCmdFile.getAbsolutePath(), ExportCommand.class);

            //
            // Execute the commands
            //
            execute();

            assertTrue(exportDest.exists());

            Document vdbDocument = TestUtilities.createDocument(new FileInputStream(exportDest));
            assertNotNull(vdbDocument);

            InputStream allElementsExample = TestUtilities.allElementsExample();
            Document allElementsDocument = TestUtilities.createDocument(allElementsExample);

            TestUtilities.compareDocuments(allElementsDocument, vdbDocument);

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
            File exportCmdFile = File.createTempFile("TestExportCommand", ".txt");
            exportCmdFile.deleteOnExit();

            //
            // Create the export destination file, ensuring it does not already exist
            //
            File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt");
            exportDest.deleteOnExit();
            if (exportDest.exists())
                exportDest.delete();

            //
            // Write the instructions to the file
            //
            createInitialTransaction();
            writer = new FileWriter(exportCmdFile);
            writer.write("cd /workspace" + NEW_LINE);
            writer.write("cd " + allElementsVdb.getName(uow) + NEW_LINE);
            writer.write("export model-one " + exportDest.getAbsolutePath() + NEW_LINE);
            writer.close();

            //
            // Setup the export instructions
            //
            setup(exportCmdFile.getAbsolutePath(), ExportCommand.class);

            //
            // Execute the commands
            //
            execute();

            String writerOutput = getCommandOutput();
            assertEquals(Messages.getString(Messages.ExportCommand.NoContentExported, "model-one"), writerOutput.trim());

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
            File exportCmdFile = File.createTempFile("TestExportCommand", ".txt");
            exportCmdFile.deleteOnExit();

            //
            // Create the export destination file, ensuring it does not already exist
            //
            File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt");
            exportDest.deleteOnExit();
            if (exportDest.exists())
                exportDest.delete();

            //
            // Write the instructions to the file
            //
            createInitialTransaction();
            writer = new FileWriter(exportCmdFile);
            writer.write("cd /workspace" + NEW_LINE);
            writer.write("cd " + allElementsVdb.getName(uow) + NEW_LINE);
            writer.write("export model-two " + exportDest.getAbsolutePath() + NEW_LINE);
            writer.close();

            //
            // Setup the export instructions
            //
            setup(exportCmdFile.getAbsolutePath(), ExportCommand.class);

            //
            // Execute the commands
            //
            execute();

            assertTrue(exportDest.exists());

            String exportContents = TestUtilities.fileToString(exportDest);
            assertEquals(ALL_ELEMENTS_MODEL_TWO_DDL + NEW_LINE, exportContents);

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
    public void testExportCommandSchemaRendition() throws Exception {
        FileWriter writer = null;

        //
        // Create the vdb in the repository
        //
        KomodoObject exampleSchema = addSchemaExample();

        try {
            //
            // Create the export command instructions file
            //
            File exportCmdFile = File.createTempFile("TestExportCommand", ".txt");
            exportCmdFile.deleteOnExit();

            //
            // Create the export destination file, ensuring it does not already exist
            //
            File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt");
            exportDest.deleteOnExit();
            if (exportDest.exists())
                exportDest.delete();

            //
            // Write the instructions to the file
            //
            createInitialTransaction();
            writer = new FileWriter(exportCmdFile);
            String path = convertToContextPath(exampleSchema);
            writer.write("export " + path + SPACE + exportDest.getAbsolutePath() + NEW_LINE);
            writer.close();

            //
            // Setup the export instructions
            //
            setup(exportCmdFile.getAbsolutePath(), ExportCommand.class);

            //
            // Execute the commands
            //
            execute();

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
