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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;

import org.junit.Test;
import org.komodo.relational.commands.AbstractCommandTest;
import org.komodo.shell.api.CommandResult;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.test.utils.TestUtilities;
import org.w3c.dom.Document;

/**
 * Test Class to test VDB {@link ExportVdbCommand}.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class ExportVdbCommandTest extends AbstractCommandTest {

    private KomodoObject addTweetVdbExample() throws KException, Exception {
        KomodoObject kWorkspace = _repo.komodoWorkspace(getTransaction());
        KomodoObject tweet = TestUtilities.createTweetExampleNode(getTransaction(), kWorkspace);

        assertNotNull(tweet);
        return tweet;
    }

    private KomodoObject addAllElementsVdbExample() throws Exception {
        KomodoObject kWorkspace = _repo.komodoWorkspace(getTransaction());
        KomodoObject tweet = TestUtilities.createAllElementsExampleNode(getTransaction(), kWorkspace);

        assertNotNull(tweet);
        return tweet;
    }

    @Test
    public void shouldExportTweetVdbFromVdbContext() throws Exception {
        testExportTweetVdb( false );
    }

    @Test
    public void shouldExportTweetVdbFromWorkspaceContext() throws Exception {
        testExportTweetVdb( true );
    }

    private void testExportTweetVdb( final boolean exportFromWorkspaceContext ) throws Exception {
        //
        // Create the vdb in the repository
        //
        KomodoObject tweetVdb = addTweetVdbExample();

        //
        // Create the export command instructions file
        //
        File exportCmdFile = File.createTempFile("TestExportVdbCommand", ".txt"); //$NON-NLS-1$  //$NON-NLS-2$
        exportCmdFile.deleteOnExit();

        //
        // Create the export destination file, ensuring it does not already exist
        //
        File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt"); //$NON-NLS-1$  //$NON-NLS-2$
        exportDest.deleteOnExit();
        if (exportDest.exists())
            exportDest.delete();

        // The test commands
        final String[] commands = new String[ exportFromWorkspaceContext ? 2 : 3 ];

        if ( exportFromWorkspaceContext ) {
            commands[ 0 ] = "commit";
            commands[ 1 ] = "export-vdb " + tweetVdb.getName( getTransaction() ) + " " + exportDest.getAbsolutePath();
        } else {
            commands[ 0 ] = "commit";
            commands[ 1 ] = "cd " + tweetVdb.getName( getTransaction() );
            commands[ 2 ] = "export-vdb " + exportDest.getAbsolutePath();
        }

        final CommandResult result = execute( commands );
        assertCommandResultOk(result);
        assertTrue(exportDest.exists());

        Document vdbDocument = TestUtilities.createDocument(new FileInputStream(exportDest));
        assertNotNull(vdbDocument);

        InputStream tweetExample = TestUtilities.tweetExample();
        Document tweetDocument = TestUtilities.createDocument(tweetExample);

        TestUtilities.compareDocuments(tweetDocument, vdbDocument);
    }

    @Test
    public void shouldExportAllElementsVdbFromVdbContext() throws Exception {
        testExportAllElementsVdb( false );
    }

    @Test
    public void shouldExportAllElementsVdbFromWorkspaceContext() throws Exception {
        testExportAllElementsVdb( true );
    }

    private void testExportAllElementsVdb( final boolean exportFromWorkspaceContext ) throws Exception {
        //
        // Create the vdb in the repository
        //
        KomodoObject allElementsVdb = addAllElementsVdbExample();

        //
        // Create the export command instructions file
        //
        File exportCmdFile = File.createTempFile("TestExportVdbCommand", ".txt"); //$NON-NLS-1$  //$NON-NLS-2$
        exportCmdFile.deleteOnExit();

        //
        // Create the export destination file, ensuring it does not already exist
        //
        File exportDest = new File(System.getProperty("java.io.tmpdir") + File.separator + "TestExportDestination.txt"); //$NON-NLS-1$  //$NON-NLS-2$
        exportDest.deleteOnExit();
        if (exportDest.exists())
            exportDest.delete();

        // The test commands
        final String[] commands = new String[ exportFromWorkspaceContext ? 2 : 3 ];

        if ( exportFromWorkspaceContext ) {
            commands[ 0 ] = "commit";
            commands[ 1 ] = "export-vdb " + allElementsVdb.getName(getTransaction()) + " " + exportDest.getAbsolutePath();
        } else {
            commands[ 0 ] = "commit";
            commands[ 1 ] = "cd " + allElementsVdb.getName(getTransaction() );
            commands[ 2 ] = "export-vdb " + exportDest.getAbsolutePath();
        }

        final CommandResult result = execute( commands );
        assertCommandResultOk(result);
        assertTrue(exportDest.exists());

        Document vdbDocument = TestUtilities.createDocument(new FileInputStream(exportDest));
        assertNotNull(vdbDocument);

        InputStream allElementsExample = TestUtilities.allElementsExample();
        Document allElementsDocument = TestUtilities.createDocument(allElementsExample);

        TestUtilities.compareDocuments(allElementsDocument, vdbDocument);
    }

    @Test
    public void testTabCompleter()throws Exception{

    	ArrayList<CharSequence> candidates=new ArrayList<>();
    	candidates.add("myVDB1");
    	candidates.add("myVDB2");

    	setup("commandFiles","addVDBs.cmd");
    	assertTabCompletion("export-vdb myV", candidates);

    	candidates.add("MyVDB3");
    	assertTabCompletion("export-vdb ", candidates);
    }

}
