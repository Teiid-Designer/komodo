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
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
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
import org.komodo.spi.repository.DocumentType;
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
        FORWARD_SLASH + TEST_VDB_XML + NEW_LINE +
        FORWARD_SLASH + "usstates" + NEW_LINE +
        FORWARD_SLASH + "usstates" + FORWARD_SLASH + "META-INF" + NEW_LINE +
        FORWARD_SLASH + "usstates" + FORWARD_SLASH + "META-INF" + FORWARD_SLASH + "dataservice.xml" + NEW_LINE +
        FORWARD_SLASH + "usstates" + FORWARD_SLASH + "connections" + NEW_LINE +
        FORWARD_SLASH + "usstates" + FORWARD_SLASH + "connections" + FORWARD_SLASH + "MySqlPool-connection.xml" + NEW_LINE +
        FORWARD_SLASH + "usstates" + FORWARD_SLASH + "drivers" + NEW_LINE +
        FORWARD_SLASH + "usstates" + FORWARD_SLASH + "drivers" + FORWARD_SLASH + "mysql-connector-java-5.1.39-bin.jar" + NEW_LINE +
        FORWARD_SLASH + "usstates" + FORWARD_SLASH + "usstates-vdb.xml" + NEW_LINE;

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

    private void compareDirContents(File original, File dir2Cmp) throws Exception {
        assertNotNull(original);
        assertTrue(original.isDirectory());
        assertNotNull(dir2Cmp);
        assertTrue(dir2Cmp.isDirectory());

        for (File orig : original.listFiles()) {
            File cmp = new File(orig.getAbsolutePath().replaceAll(original.getName(), dir2Cmp.getName()));
            assertTrue(cmp.exists());
            assertTrue(orig.isDirectory() == cmp.isDirectory());

            if (orig.isDirectory()) {
                compareDirContents(orig, cmp);
                continue;
            }

            compareFileContents(orig, cmp);
        }
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

        File usStatesZipFile = new File(tmpDir, TestUtilities.US_STATES_VDB_NAME + ZIP_SUFFIX);
        FileUtils.write(TestUtilities.usStatesDataserviceExample(), usStatesZipFile);
        try (FileInputStream fis = new FileInputStream(usStatesZipFile)) {
            File usStatesDir = new File(seedDir, TestUtilities.US_STATES_VDB_NAME);
            usStatesDir.mkdir();
            FileUtils.zipExtract(fis, usStatesDir);
            usStatesZipFile.delete();
        }

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
    public void testLocalRepositoryReadDirectory() throws Exception {
        localTmpDir = new File(tmpDir, "localTmpDir-" + timestamp);
        Properties parameters = new Properties();
        parameters.setProperty(GitStorageConnector.REPO_DEST_PROPERTY, localTmpDir.getAbsolutePath());
        parameters.setProperty(GitStorageConnector.REPO_PATH_PROPERTY, myGitDir.getAbsolutePath());

        connector = new GitStorageConnector(parameters);
        connector.refresh();

        parameters.setProperty(StorageConnector.FILE_PATH_PROPERTY, TestUtilities.US_STATES_VDB_NAME);
        InputStream is = connector.read(parameters);
        assertNotNull(is);

        File original = createTempFile(TestUtilities.US_STATES_VDB_NAME, ZIP_SUFFIX);
        FileUtils.write(TestUtilities.usStatesDataserviceExample(), original);
        File origDir = new File(tmpDir, "original");
        try (FileInputStream origFis = new FileInputStream(original)) {
            origDir.mkdir();
            FileUtils.zipExtract(origFis, origDir);
        }

        File fileToCompare = createTempFile(TestUtilities.US_STATES_VDB_NAME, ZIP_SUFFIX);
        FileUtils.write(is, fileToCompare);
        File file2CmpDir = new File(tmpDir, "file2cmp");
        try (FileInputStream file2CmpFis = new FileInputStream(fileToCompare)) {
            file2CmpDir.mkdir();
            FileUtils.zipExtract(file2CmpFis, file2CmpDir);
        }

        try {
            compareDirContents(origDir, file2CmpDir);
        } finally {
            FileUtils.removeDirectoryAndChildren(origDir);
            FileUtils.removeDirectoryAndChildren(file2CmpDir);
        }
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
        when(artifact.export(transaction, parameters)).thenReturn(sampleExample.getBytes());
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
    public void testWriteZipToRepositoryAsDirectory() throws Exception {
        localTmpDir = new File(tmpDir, "localTmpDir-" + timestamp);
        Properties parameters = new Properties();
        parameters.setProperty(GitStorageConnector.REPO_DEST_PROPERTY, localTmpDir.getAbsolutePath());
        parameters.setProperty(GitStorageConnector.REPO_PATH_PROPERTY, myGitDir.getAbsolutePath());

        connector = new GitStorageConnector(parameters);
        connector.refresh();

        String dsName = TestUtilities.US_STATES_VDB_NAME;

        UnitOfWork transaction = mock(UnitOfWork.class);
        when(transaction.getState()).thenReturn(State.NOT_STARTED);

        Exportable artifact = mock(Exportable.class);
        InputStream usStatesExample = TestUtilities.usStatesDataserviceExample();
        byte[] usStatesArr = FileUtils.streamToByteArray(usStatesExample);

        parameters = new Properties();
        parameters.setProperty(GitStorageConnector.FILE_PATH_PROPERTY, DocumentType.ZIP.fileName(dsName));

        when(artifact.export(transaction, parameters)).thenReturn(usStatesArr);
        when(artifact.getName(transaction)).thenReturn(dsName);
        when(artifact.getDocumentType(transaction)).thenReturn(DocumentType.ZIP);

        connector.write(artifact, transaction, parameters);

        //
        // Test the artifact was pushed by walking the origin repository
        //
        usStatesExample = TestUtilities.usStatesDataserviceExample();
        List<String> zipEntries = TestUtilities.zipEntries(dsName, usStatesExample);

        Repository repository = myGit.getRepository();
        ObjectId commitId = repository.resolve(Constants.HEAD);
        try (RevWalk revWalk = new RevWalk(repository)) {
            RevCommit commit = revWalk.parseCommit(commitId);
            RevTree tree = commit.getTree();

            try (TreeWalk treeWalk = new TreeWalk(repository)) {
                treeWalk.addTree(tree);
                treeWalk.setRecursive(false);
                while (treeWalk.next()) {
                    zipEntries.remove(treeWalk.getPathString());

                    if (treeWalk.isSubtree())
                        treeWalk.enterSubtree();
                }
            }

            //
            // All entries in the original zip have been extracted
            // and pushed to the git repository
            //
            assertTrue("Remaining entries: " + Arrays.toString(zipEntries.toArray(new String[0])), zipEntries.isEmpty());
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

    /**
     * SECURE.CREDENTIALS="ssh private key, ssh known hosts"
     * JVM.PROPERTIES="{private.key.file, known.hosts.file"
     * 
     * Test meant to be executed explicitly rather than over automated build system. This is due to
     * ${SECURE.CREDENTIALS} being required for authentication. Such credentials should not be
     * stored in the source code hence the test is configured to fetch them from a file on the local
     * filesystem specifed via the JVM properties ${JVM.PROPERTIES}. The credential file needs to be in
     * a specific format:
     *
     * Should be a fully formatted ssh private key file and its associated public key should have been
     * uploaded to the github account prior to test execution.
     *
     * Also requires a known hosts file containing the github host & encrypted key. This can be derived by
     * manually connecting to github and copying the results from ~/.ssh/known_hosts.
     *
     * @throws Exception
     */
    @Test
    public void testRemoteRepositorySSH() throws Exception {
        localTmpDir = new File(tmpDir, "localTmpDir-" + timestamp);
        Properties parameters = new Properties();
        parameters.setProperty(GitStorageConnector.REPO_DEST_PROPERTY, localTmpDir.getAbsolutePath());
        parameters.setProperty(GitStorageConnector.REPO_PATH_PROPERTY, "git@github.com:Teiid-Designer/komodo-unit-testing.git");
        parameters.setProperty(GitStorageConnector.AUTHOR_NAME_PROPERTY, "bob");
        parameters.setProperty(GitStorageConnector.AUTHOR_EMAIL_PROPERTY, "bob@komodo.org");

        //
        // The file location of the private key to use should be attached to
        // the execution arguments of this test using the private.key.file variable
        //
        String privKeyFileLoc = System.getProperty("private.key.file");
        if (privKeyFileLoc == null) {
            System.out.println("TEST SKIPPED: PRIVATE KEY FILE NOT SPECIFIED");
            return; // Dont fail this since the test should be executed explicitly rather than as part of a suite
        }

        File privKeyFile = new File(privKeyFileLoc);
        assertTrue(privKeyFile.exists()); // This will fail since the location has been specified but doesn't exist

        String privKey = FileUtils.readSafe(privKeyFile);
        parameters.setProperty(GitStorageConnector.REPO_PRIVATE_KEY, privKey);

        //
        // The file location of the known hosts to use should be attached to
        // the execution arguments of this test using the known.hosts.file variable
        //
        String knownHostsFileLoc = System.getProperty("known.hosts.file");
        if (knownHostsFileLoc == null)
            fail("Both private.key.file and known.hosts.file are required for this test to be executed successfully");

        File knownHostsFile = new File(knownHostsFileLoc);
        assertTrue(knownHostsFile.exists());

        String knownHosts = FileUtils.readSafe(knownHostsFile);
        parameters.setProperty(GitStorageConnector.REPO_KNOWN_HOSTS_ID, knownHosts);

        connector = new GitStorageConnector(parameters);
        connector.refresh();

        parameters.setProperty(StorageConnector.FILE_PATH_PROPERTY, "README.md");
        InputStream is = connector.read(parameters);
        assertNotNull(is);

        UnitOfWork transaction = mock(UnitOfWork.class);
        when(transaction.getState()).thenReturn(State.NOT_STARTED);

        parameters.setProperty(GitStorageConnector.FILE_PATH_PROPERTY, TEST_VDB_2_XML);

        Exportable artifact = mock(Exportable.class);
        String sampleExample = TestUtilities.streamToString(TestUtilities.sampleExample());
        when(artifact.export(transaction, parameters)).thenReturn(sampleExample.getBytes());
        when(artifact.getName(transaction)).thenReturn(TestUtilities.SAMPLE_VDB_FILE);

        connector.write(artifact, transaction, parameters);

        //
        // NOW CHECK THAT A COMMIT HAS APPEARED ON GITHUB
        //
    }

    /**
     * SECURE.CREDENTIALS="username/password"
     * JVM.PROPERTY="credentials.file"
     * 
     * Test meant to be executed explicitly rather than over automated build system. This is due to
     * ${SECURE.CREDENTIALS} being required for authentication. Such credentials should not be
     * stored in the source code hence the test is configured to fetch them from a file on the local
     * filesystem specifed via the JVM property ${JVM.PROPERTY}. The credential file needs to be in
     * a specific format:
     *
     * username=blah
     * password=blah
     *
     * @throws Exception
     */
    @Test
  public void testRemoteRepositoryHttp() throws Exception {
      localTmpDir = new File(tmpDir, "localTmpDir-" + timestamp);
      Properties parameters = new Properties();
      parameters.setProperty(GitStorageConnector.REPO_DEST_PROPERTY, localTmpDir.getAbsolutePath());
      parameters.setProperty(GitStorageConnector.REPO_PATH_PROPERTY, "https://github.com/Teiid-Designer/komodo-unit-testing.git");
      parameters.setProperty(GitStorageConnector.AUTHOR_NAME_PROPERTY, "bob");
      parameters.setProperty(GitStorageConnector.AUTHOR_EMAIL_PROPERTY, "bob@komodo.org");

      //
      // The file location of the username/password to use should be attached to
      // the execution arguments of this test using the credentials.file variable
      // The file should be in the format
      // username=blah
      // password=blah
      //
      String credFileLoc = System.getProperty("credentials.file");
      if (credFileLoc == null) {
          System.out.println("TEST SKIPPED: CREDENTIALS FILE NOT SPECIFIED");
          return; // Dont fail this since the test should be executed explicitly rather than as part of a suite
      }

      File credFile = new File(credFileLoc);
      assertTrue(credFile.exists()); // This will fail since the location has been specified but doesn't exist
      
      FileReader reader = null;
      BufferedReader buf = null;
      try {
          reader = new FileReader(credFile);
          buf = new BufferedReader(reader);
          while (buf.ready()) {
              String line = buf.readLine();
              String[] segments = line.split(EQUALS);
              if (segments == null || segments.length != 2)
                  continue;

              if (segments[0].equals("username"))
                  parameters.setProperty(GitStorageConnector.REPO_USERNAME, segments[1]);
              else if (segments[0].equals("password"))
                  parameters.setProperty(GitStorageConnector.REPO_PASSWORD, segments[1]);
          }
      } finally {
          if (buf != null) {
              try {
                  buf.close();
              } catch (Exception e) {}
          }
      }

      connector = new GitStorageConnector(parameters);
      connector.refresh();

      parameters.setProperty(StorageConnector.FILE_PATH_PROPERTY, "README.md");
      InputStream is = connector.read(parameters);
      assertNotNull(is);

      UnitOfWork transaction = mock(UnitOfWork.class);
      when(transaction.getState()).thenReturn(State.NOT_STARTED);

      parameters.setProperty(GitStorageConnector.FILE_PATH_PROPERTY, TEST_VDB_2_XML);

      Exportable artifact = mock(Exportable.class);
      String sampleExample = TestUtilities.streamToString(TestUtilities.sampleExample());
      when(artifact.export(transaction, parameters)).thenReturn(sampleExample.getBytes());
      when(artifact.getName(transaction)).thenReturn(TestUtilities.SAMPLE_VDB_FILE);

      connector.write(artifact, transaction, parameters);

      //
      // NOW CHECK THAT A COMMIT HAS APPEARED ON GITHUB
      //
  }
}
