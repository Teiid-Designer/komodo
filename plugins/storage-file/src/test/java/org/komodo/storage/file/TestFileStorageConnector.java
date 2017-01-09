package org.komodo.storage.file;
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


import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.io.File;
import java.io.InputStream;
import java.util.Properties;
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

public class TestFileStorageConnector implements StringConstants {

    private static final String TEST_VDB_XML = "test-vdb.xml";

    private static final String TEST_VDB_2_XML = "test-vdb2.xml";

    private static final String SUB_DIR = "subDir";

    private static final String TEST_VDB_3_XML = "test-vdb3.xml";

    private File tmpDir;

    private File myFileDir;

    private long timestamp;

    private FileStorageConnector connector;

    @Before
    public void setup() throws Exception {
        String tmpDirPath = System.getProperty("java.io.tmpdir");
        tmpDir = new File(tmpDirPath);

        timestamp = System.currentTimeMillis();
        myFileDir = new File(tmpDir, "myfile-" + timestamp);
        assertTrue(myFileDir.mkdir());
        
        // Adds a file into the tmp directory
        File vdbFile = new File(myFileDir, TEST_VDB_XML);
        assertTrue(vdbFile.createNewFile());
        FileUtils.write(TestUtilities.tweetExample(), vdbFile);
        assertTrue(vdbFile.length() > 0);

        File subDir = new File(myFileDir, SUB_DIR);
        assertTrue(subDir.mkdir());

        File subDirVdbFile = new File(subDir, TEST_VDB_3_XML);
        assertTrue(subDirVdbFile.createNewFile());
        FileUtils.write(TestUtilities.tweetExample(), subDirVdbFile);
        assertTrue(subDirVdbFile.length() > 0);
    }

    @After
    public void tearDown() throws Exception {
        if (myFileDir != null)
            FileUtils.removeDirectoryAndChildren(myFileDir);

        if (connector != null)
            connector.dispose();
    }

    @Test
    public void testRequiredParameters() {
        try {
            new FileStorageConnector(null);
            fail("Should not allow null parameters");
        } catch (Exception ex) {
            // Expect to fail
        }
    }

    @Test
    public void testRequiredPathParameterRead() {
        try {
            Properties parameters = new Properties();

            FileStorageConnector connector = new FileStorageConnector(parameters);
            connector.read(parameters);
            fail("Should not allow null path parameter");
        } catch (Exception ex) {
            // Expect to fail
        }
    }

    @Test
    public void testRequiredPathParameterWrite() {
        try {
            Properties parameters = new Properties();

            FileStorageConnector connector = new FileStorageConnector(parameters);
            connector.write(null, null, parameters);
            fail("Should not allow null path parameter");
        } catch (Exception ex) {
            // Expect to fail
        }
    }

    @Test
    public void testReadFile() throws Exception {
        Properties parameters = new Properties();
        parameters.setProperty(StorageConnector.FILES_HOME_PATH_PROPERTY, myFileDir.getAbsolutePath());

        connector = new FileStorageConnector(parameters);

        parameters.setProperty(StorageConnector.FILE_PATH_PROPERTY, TEST_VDB_XML);
        InputStream is = connector.read(parameters);
        assertNotNull(is);

        File original = TestUtilities.createTempFile("tweet-vdb", XML_SUFFIX);
        FileUtils.write(TestUtilities.tweetExample(), original);

        File fileToCompare = TestUtilities.createTempFile("test-vdb1", XML_SUFFIX);
        FileUtils.write(is, fileToCompare);

        TestUtilities.compareFileContents(original, fileToCompare);
    }

    @Test
    public void testRequiredFileDestParameter() {
        try {
            Properties parameters = new Properties();
            parameters.setProperty(StorageConnector.FILES_HOME_PATH_PROPERTY, myFileDir.getAbsolutePath());

            connector = new FileStorageConnector(parameters);

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
    public void testWriteToFile() throws Exception {
        Properties parameters = new Properties();
        parameters.setProperty(StorageConnector.FILES_HOME_PATH_PROPERTY, myFileDir.getAbsolutePath());

        connector = new FileStorageConnector(parameters);
        connector.refresh();

        UnitOfWork transaction = mock(UnitOfWork.class);
        when(transaction.getState()).thenReturn(State.NOT_STARTED);

        parameters = new Properties();
        parameters.setProperty(FileStorageConnector.FILE_PATH_PROPERTY, TEST_VDB_2_XML);

        Exportable artifact = mock(Exportable.class);
        String sampleExample = TestUtilities.streamToString(TestUtilities.sampleExample());
        when(artifact.export(transaction, parameters)).thenReturn(sampleExample.getBytes());
        when(artifact.getName(transaction)).thenReturn(TestUtilities.SAMPLE_VDB_FILE);

        connector.write(artifact, transaction, parameters);

        //
        // Check the file was added to the tmp directory
        //
        File writtenFile = new File(myFileDir, TEST_VDB_2_XML);
        assertTrue(writtenFile.exists());

        File cmpFile = TestUtilities.createTempFile("sampleExampleFile", XML_SUFFIX);
        FileUtils.write(TestUtilities.sampleExample(), cmpFile);
        TestUtilities.compareFileContents(cmpFile, writtenFile);
    }

    @Test
    public void testBrowse() throws Exception {
        Properties parameters = new Properties();
        parameters.setProperty(StorageConnector.FILES_HOME_PATH_PROPERTY, myFileDir.getAbsolutePath());

        connector = new FileStorageConnector(parameters);
        StorageTree<String> structure = connector.browse();
        String tree = structure.printTree();

        // Allows the order to be changed but not the content
        assertTrue(tree.contains(NEW_LINE + FORWARD_SLASH + myFileDir.getName() + NEW_LINE));
        assertTrue(tree.contains(NEW_LINE + FORWARD_SLASH + myFileDir.getName() + FORWARD_SLASH + SUB_DIR + NEW_LINE));
        assertTrue(tree.contains(NEW_LINE + FORWARD_SLASH + myFileDir.getName() + FORWARD_SLASH + SUB_DIR + FORWARD_SLASH + TEST_VDB_3_XML + NEW_LINE));
        assertTrue(tree.contains(NEW_LINE + FORWARD_SLASH + myFileDir.getName() + FORWARD_SLASH + TEST_VDB_XML + NEW_LINE));
    }
}
