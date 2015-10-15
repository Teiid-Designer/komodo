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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStream;
import org.junit.Test;
import org.komodo.relational.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.test.utils.TestUtilities;
import org.w3c.dom.Document;

/**
 * Test Class to test VDB ExportCommand
 *
 */
@SuppressWarnings("javadoc")
public class ExportCommandTest extends AbstractCommandTest {

    private static final String TWEET_VDB = "./resources/tweet-example-vdb.xml";  //$NON-NLS-1$
    private static final String ALL_ELEMENTS_VDB = "./resources/teiid-vdb-all-elements.xml";  //$NON-NLS-1$
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
            File exportCmdFile = File.createTempFile("TestExportCommand", ".txt"); //$NON-NLS-1$  //$NON-NLS-2$
            exportCmdFile.deleteOnExit();

            //
            // Create the export destination file, ensuring it does not already exist
            //
            File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt"); //$NON-NLS-1$  //$NON-NLS-2$
            exportDest.deleteOnExit();
            if (exportDest.exists())
                exportDest.delete();

            //
            // Write the instructions to the file
            //
            createInitialTransaction();
            writer = new FileWriter(exportCmdFile);
            writer.write("workspace " + NEW_LINE); //$NON-NLS-1$
            writer.write("cd " + tweetVdb.getName(uow) + NEW_LINE);  //$NON-NLS-1$
            writer.write("export-vdb " + exportDest.getAbsolutePath() + NEW_LINE); //$NON-NLS-1$
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

            InputStream tweetExample = new FileInputStream(new File(TWEET_VDB));
            Document tweetDocument = TestUtilities.createDocument(tweetExample);

            TestUtilities.compareDocuments(tweetDocument, vdbDocument);

        } finally {
            commit();

            if (writer != null)
                writer.close();
        }
    }
    
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
            File exportCmdFile = File.createTempFile("TestExportCommand", ".txt"); //$NON-NLS-1$  //$NON-NLS-2$
            exportCmdFile.deleteOnExit();

            //
            // Create the export destination file, ensuring it does not already exist
            //
            File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt"); //$NON-NLS-1$  //$NON-NLS-2$
            exportDest.deleteOnExit();
            if (exportDest.exists())
                exportDest.delete();

            //
            // Write the instructions to the file
            //
            createInitialTransaction();
            writer = new FileWriter(exportCmdFile);
            writer.write("workspace " + NEW_LINE); //$NON-NLS-1$
            writer.write("cd " + allElementsVdb.getName(uow) + NEW_LINE);  //$NON-NLS-1$
            writer.write("export-vdb " + exportDest.getAbsolutePath() + NEW_LINE); //$NON-NLS-1$ 
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

            Document vdbDocument = TestUtilities.createDocument(new FileInputStream(exportDest));
            assertNotNull(vdbDocument);
            
            InputStream allElementsExample = new FileInputStream(new File(ALL_ELEMENTS_VDB));
            Document allElementsDocument = TestUtilities.createDocument(allElementsExample);

            TestUtilities.compareDocuments(allElementsDocument, vdbDocument);

        } finally {
            commit();

            if (writer != null)
                writer.close();
        }
    }

}
