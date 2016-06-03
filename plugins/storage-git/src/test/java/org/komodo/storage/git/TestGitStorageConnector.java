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
package org.komodo.storage.git;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.lib.ObjectLoader;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevTree;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.treewalk.TreeWalk;
import org.eclipse.jgit.treewalk.filter.PathFilter;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Exportable;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageTree;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;;

public class TestGitStorageConnector implements StringConstants {

    private static final String TEST_VDB_XML = "test-vdb.xml";

    private static final String TEST_VDB_2_XML = "test-vdb2.xml";

    private static final String SUB_DIR = "subDir";

    private static final String TEST_VDB_3_XML = "test-vdb3.xml";

    private static final String TREE = "" +
        FORWARD_SLASH + NEW_LINE +
        FORWARD_SLASH + SUB_DIR + NEW_LINE +
        FORWARD_SLASH + SUB_DIR + FORWARD_SLASH + TEST_VDB_3_XML + NEW_LINE +
        FORWARD_SLASH + TEST_VDB_XML + NEW_LINE;

    private File tmpDir;

    private File myGitDir;

    private Git myGit;

    private long timestamp;

    private File localTmpDir;

    private GitStorageConnector connector;

    private File createTempFile(String prefix, String suffix) throws IOException {
        File tempFile = File.createTempFile(prefix, suffix);
        tempFile.deleteOnExit();
        return tempFile;
    }

    private void compareFileContents(File original, File fileToCompare) throws IOException {
        assertTrue(org.apache.commons.io.FileUtils.contentEquals(original, fileToCompare));
    }

    @Before
    public void setup() throws Exception {
        String tmpDirPath = System.getProperty("java.io.tmpdir");
        tmpDir = new File(tmpDirPath);

        timestamp = System.currentTimeMillis();
        myGitDir = new File(tmpDir, "mygit-" + timestamp);
        assertTrue(myGitDir.mkdir());

        myGit = Git.init()
                            .setDirectory(myGitDir)
                            .setBare(true)
                            .call();
        assertNotNull(myGit);

        //
        // Create a clone to seed the repository with a file
        //
        File seedDir = new File(tmpDir, "seedDir-" + timestamp);
        Git seedGit = Git.cloneRepository()
                                    .setURI(myGitDir.getAbsolutePath())
                                    .setDirectory(seedDir)
                                    .call();
        
        // Adds a file into the seed repository
        File vdbFile = new File(seedDir, TEST_VDB_XML);
        assertTrue(vdbFile.createNewFile());
        FileUtils.write(TestUtilities.tweetExample(), vdbFile);
        assertTrue(vdbFile.length() > 0);

        File subDir = new File(seedDir, SUB_DIR);
        assertTrue(subDir.mkdir());

        File subDirVdbFile = new File(subDir, TEST_VDB_3_XML);
        assertTrue(subDirVdbFile.createNewFile());
        FileUtils.write(TestUtilities.tweetExample(), subDirVdbFile);
        assertTrue(subDirVdbFile.length() > 0);

        seedGit.add()
                    .addFilepattern(DOT)
                    .call();
        seedGit.commit().setMessage("Adds Test Files").call();
        seedGit.push().call();

        FileUtils.removeDirectoryAndChildren(seedDir);
    }

    @After
    public void tearDown() throws Exception {
        if (localTmpDir != null)
            FileUtils.removeDirectoryAndChildren(localTmpDir);

        if (myGit != null)
            myGit.close();

        if (myGitDir != null)
            FileUtils.removeDirectoryAndChildren(myGitDir);

        if (connector != null)
            connector.dispose();
    }

    @Test
    public void testRequiredParameters() {
        try {
            new GitStorageConnector(null);
            fail("Should not allow null parameters");
        } catch (Exception ex) {
            // Expect to fail
        }
    }

    @Test
    public void testRequiredPathParameter() {
        try {
            Properties parameters = new Properties();
            parameters.setProperty(GitStorageConnector.REPO_DEST_PROPERTY, localTmpDir.getAbsolutePath());
            
            new GitStorageConnector(parameters);
            fail("Should not allow null path parameter");
        } catch (Exception ex) {
            // Expect to fail
        }
    }

    @Test
    public void testLocalRepositoryReadFile() throws Exception {
        localTmpDir = new File(tmpDir, "localTmpDir-" + timestamp);
        Properties parameters = new Properties();
        parameters.setProperty(GitStorageConnector.REPO_DEST_PROPERTY, localTmpDir.getAbsolutePath());
        parameters.setProperty(GitStorageConnector.REPO_PATH_PROPERTY, myGitDir.getAbsolutePath());

        connector = new GitStorageConnector(parameters);
        connector.refresh();

        parameters.setProperty(StorageConnector.FILE_PATH_PROPERTY, TEST_VDB_XML);
        InputStream is = connector.read(parameters);
        assertNotNull(is);

        File original = createTempFile("tweet-vdb", XML_SUFFIX);
        FileUtils.write(TestUtilities.tweetExample(), original);

        File fileToCompare = createTempFile("test-vdb1", XML_SUFFIX);
        FileUtils.write(is, fileToCompare);

        compareFileContents(original, fileToCompare);
    }

    @Test
    public void testRequiredFileDestParameter() {
        try {
            Properties parameters = new Properties();
            parameters.setProperty(GitStorageConnector.REPO_DEST_PROPERTY, localTmpDir.getAbsolutePath());
            parameters.setProperty(GitStorageConnector.REPO_PATH_PROPERTY, myGitDir.getAbsolutePath());
            
            connector = new GitStorageConnector(parameters);

            UnitOfWork transaction = mock(UnitOfWork.class);
            when(transaction.getState()).thenReturn(State.NOT_STARTED);

            parameters = new Properties();

            Exportable artifact = mock(Exportable.class);

            connector.write(artifact, transaction, parameters);

            fail("Should not allow null dest parameter");
        } catch (Exception ex) {
            // Expect to fail
        }
    }

    @Test
    public void testWriteToRepository() throws Exception {
        localTmpDir = new File(tmpDir, "localTmpDir-" + timestamp);
        Properties parameters = new Properties();
        parameters.setProperty(GitStorageConnector.REPO_DEST_PROPERTY, localTmpDir.getAbsolutePath());
        parameters.setProperty(GitStorageConnector.REPO_PATH_PROPERTY, myGitDir.getAbsolutePath());

        connector = new GitStorageConnector(parameters);
        connector.refresh();

        UnitOfWork transaction = mock(UnitOfWork.class);
        when(transaction.getState()).thenReturn(State.NOT_STARTED);

        parameters = new Properties();
        parameters.setProperty(GitStorageConnector.FILE_PATH_PROPERTY, TEST_VDB_2_XML);

        Exportable artifact = mock(Exportable.class);
        String sampleExample = TestUtilities.streamToString(TestUtilities.sampleExample());
        when(artifact.export(transaction, parameters)).thenReturn(sampleExample);
        when(artifact.getName(transaction)).thenReturn(TestUtilities.SAMPLE_VDB_FILE);

        connector.write(artifact, transaction, parameters);

        //
        // Test the artifact was pushed by walking the origin repository
        //
        Repository repository = myGit.getRepository();
        ObjectId commitId = repository.resolve(Constants.HEAD);
        try (RevWalk revWalk = new RevWalk(repository)) {
            RevCommit commit = revWalk.parseCommit(commitId);
            RevTree tree = commit.getTree();
            try (TreeWalk treeWalk = new TreeWalk(repository)) {
                treeWalk.addTree(tree);
                treeWalk.setRecursive(true);
                treeWalk.setFilter(PathFilter.create(TEST_VDB_2_XML));
                assertTrue(treeWalk.next());

                //
                // Found the file has been successfully pushed now
                // conclude it contains the same contents
                //
                ObjectId objectId = treeWalk.getObjectId(0);
                ObjectLoader loader = repository.open(objectId);

                File tmpFile1 = createTempFile("myGitTestFile", XML_SUFFIX);
                FileUtils.write(loader.getBytes(), tmpFile1);

                File tmpFile2 = createTempFile("sampleExampleFile", XML_SUFFIX);
                FileUtils.write(TestUtilities.sampleExample(), tmpFile2);
                compareFileContents(tmpFile1, tmpFile2);
            }
        }
    }

    @Test
    public void testBrowse() throws Exception {
        localTmpDir = new File(tmpDir, "localTmpDir-" + timestamp);
        Properties parameters = new Properties();
        parameters.setProperty(GitStorageConnector.REPO_DEST_PROPERTY, localTmpDir.getAbsolutePath());
        parameters.setProperty(GitStorageConnector.REPO_PATH_PROPERTY, myGitDir.getAbsolutePath());

        connector = new GitStorageConnector(parameters);
        StorageTree<String> structure = connector.browse();
        String tree = structure.printTree();
        assertEquals(TREE, tree);
    }
}
