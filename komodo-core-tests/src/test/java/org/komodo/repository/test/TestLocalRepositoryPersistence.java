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
package org.komodo.repository.test;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.repository.LocalRepository;
import org.komodo.repository.LocalRepository.LocalRepositoryId;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.SynchronousCallback;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.test.utils.AbstractLoggingTest;
import org.komodo.test.utils.LocalRepositoryObserver;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;
import org.modeshape.jcr.JcrLexicon;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.jcr.api.observation.Event.Sequencing;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class TestLocalRepositoryPersistence extends AbstractLoggingTest implements Sequencing {

    protected static final long TIME_TO_WAIT = 3; // in minutes

    private static final String TEST_FILE_DATABASE = System.getProperty("java.io.tmpdir") +
                                                                                    File.separator
                                                                                    + "TestLocalFileRepoPersistence";

    private static final String TEST_LEVELDB_DATABASE = System.getProperty("java.io.tmpdir") +
                                                                                    File.separator
                                                                                    + "TestLocalLevelDBRepoPersistence";

    private static final String TEST_FILE_REPOSITORY_CONFIG = "test-local-repository-on-file-config.json";

    private static final String TEST_LEVELDB_REPOSITORY_CONFIG = "test-local-repository-on-leveldb-config.json";

    private static final String PRODUCTION_REPOSITORY_CONFIG = "local-repository-config.json";

    protected LocalRepository _repo = null;

    protected LocalRepositoryObserver _repoObserver = null;

    private File testDb(String dbPath) {
        return new File(dbPath);
    }

    private boolean dbExists(String dbPath) {
        File testDb = testDb(dbPath);
        return testDb.exists();
    }

    private void deleteDbDir(String dbPath) {
        File testDb = testDb(dbPath);
        if (testDb.exists())
            FileUtils.removeDirectoryAndChildren(testDb);
    }

    private void checkRepoObserverErrors() throws Exception {
        Throwable startupError = _repoObserver.getError();
        if (startupError != null) {
            startupError.printStackTrace();
            fail("Repository error occurred on startup: " + startupError.getMessage());
        }
    }

    private void initLocalRepository(Class<?> loaderClass, String configFile) throws Exception {
        URL configUrl = loaderClass.getResource(configFile);
        assertNotNull(configUrl);

        LocalRepositoryId id = new LocalRepositoryId(configUrl, DEFAULT_LOCAL_WORKSPACE_NAME);
        _repo = new LocalRepository(id);
        assertThat(_repo.getState(), is(State.NOT_REACHABLE));
        assertThat(_repo.ping(), is(false));

        _repoObserver = new LocalRepositoryObserver();
        assertNotNull(_repoObserver);
        _repo.addObserver(_repoObserver);

        // Start the repository
        final RepositoryClient client = mock(RepositoryClient.class);
        final RepositoryClientEvent event = RepositoryClientEvent.createStartedEvent(client);
        _repo.notify(event);

        // Wait for the starting of the repository or timeout of 1 minute
        if (!_repoObserver.getLatch().await(1, TimeUnit.MINUTES)) {
            checkRepoObserverErrors();
            fail("Test timed-out waiting for local repository to start");
        }

        checkRepoObserverErrors();
    }

    private void initLocalRepository(String configFile) throws Exception {
        initLocalRepository(TestLocalRepositoryPersistence.class, configFile);
    }

    /**
     * Shutdown and destroy repo
     *
     * @throws Exception
     */
    private void destroyLocalRepository() throws Exception {
        assertNotNull(_repo);
        assertNotNull(_repoObserver);

        _repoObserver.resetLatch();

        RepositoryClient client = mock(RepositoryClient.class);
        RepositoryClientEvent event = RepositoryClientEvent.createShuttingDownEvent(client);
        _repo.notify(event);

        try {
            if (! _repoObserver.getLatch().await(1, TimeUnit.MINUTES))
                fail("Local repository was not stopped");
        } finally {
            _repo.removeObserver(_repoObserver);
            _repoObserver = null;
            _repo = null;
        }
    }

    @Before
    public void setup() {
        deleteDbDir(TEST_FILE_DATABASE);
        assertFalse(dbExists(TEST_FILE_DATABASE));

        deleteDbDir(TEST_LEVELDB_DATABASE);
        assertFalse(dbExists(TEST_LEVELDB_DATABASE));
    }

    @After
    public void cleanup() throws Exception {
        destroyLocalRepository();

        deleteDbDir(TEST_FILE_DATABASE);
        deleteDbDir(TEST_LEVELDB_DATABASE);
    }

    private void helpTestPersistenceWorkspace(String dbPath, String config) throws Exception, KException {
        // Ensure the file store does not already exist
        assertFalse(dbExists(dbPath));
        assertNull(_repo);

        // Initialise the repository
        initLocalRepository(config);
        assertNotNull(_repo);

        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = _repo.createTransaction(TEST_USER, "test-persistence-workspace", false, callback);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(uow);
        assertNotNull(workspace);

        //
        // Commit the transaction and await the response of the callback
        //
        uow.commit();

        //
        // Wait for the latch to countdown
        //
        assertTrue(callback.await(TIME_TO_WAIT, TimeUnit.MINUTES));
        assertFalse(callback.hasError());

        // Find the workspace to confirm what we expect to happen
        uow = _repo.createTransaction(RepositoryImpl.SYSTEM_USER, "test-search-type", true, null);
        List<KomodoObject> results = _repo.searchByType(uow, KomodoLexicon.Workspace.NODE_TYPE);
        assertEquals(1, results.size());
        assertEquals(RepositoryImpl.komodoWorkspacePath(uow), results.iterator().next().getAbsolutePath());
        uow.commit();

        // Shutdown the repository
        destroyLocalRepository();

        // Restart the repository
        initLocalRepository(config);
        assertNotNull(_repo);

        // Find the root and workspace to confirm repo was persisted
        uow = _repo.createTransaction(RepositoryImpl.SYSTEM_USER, "test-search-type", true, null);
        results = _repo.searchByType(uow, KomodoLexicon.Komodo.NODE_TYPE);
        assertEquals(1, results.size());

        results = _repo.searchByType(uow, KomodoLexicon.Workspace.NODE_TYPE);
        assertEquals(1, results.size());
        assertEquals(RepositoryImpl.komodoWorkspacePath(uow), results.iterator().next().getAbsolutePath());
        uow.commit();
    }

    @Test
    public void testFilePersistenceWorkspace() throws Exception {
        helpTestPersistenceWorkspace(TEST_FILE_DATABASE, TEST_FILE_REPOSITORY_CONFIG);
    }

    @Test
    public void testLevelDBPersistenceWorkspace() throws Exception {
        helpTestPersistenceWorkspace(TEST_LEVELDB_DATABASE, TEST_LEVELDB_REPOSITORY_CONFIG);
    }

    private void helpTestPersistenceObjects(String dbPath, String config) throws Exception, KException {
        int testObjectCount = 5;

        // Ensure the file store does not already exist
        assertFalse(dbExists(dbPath));
        assertNull(_repo);

        // Initialise the repository
        initLocalRepository(config);
        assertNotNull(_repo);

        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = _repo.createTransaction(TEST_USER, "test-persistence-objects", false, callback);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(uow);
        assertNotNull(workspace);

        for (int i = 1; i <= testObjectCount; ++i) {
            KomodoObject child = workspace.addChild(uow, "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(uow, KomodoLexicon.VdbModel.METADATA_TYPE, "DDL");
        }

        //
        // Commit the transaction and await the response of the callback
        //
        uow.commit();

        //
        // Wait for the latch to countdown
        //
        assertTrue(callback.await(TIME_TO_WAIT, TimeUnit.MINUTES));
        assertFalse(callback.hasError());

        // Find the objects to confirm what we expect to happen
        uow = _repo.createTransaction(TEST_USER, "test-search-type", true, null);
        List<KomodoObject> results = _repo.searchByType(uow, KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(testObjectCount, results.size());
        uow.commit();

        // Shutdown the repository
        destroyLocalRepository();

        // Restart the repository
        initLocalRepository(config);
        assertNotNull(_repo);

        // Find the test nodes to confirm repo was persisted
        uow = _repo.createTransaction(TEST_USER, "test-search-type", true, null);
        results = _repo.searchByType(uow, KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(testObjectCount, results.size());

        for (KomodoObject result : results) {
            String name = result.getName(uow);
            assertTrue(name.startsWith("test"));

            Property property = result.getProperty(uow, KomodoLexicon.VdbModel.METADATA_TYPE);
            assertEquals("DDL", property.getStringValue(uow));
        }

        uow.commit();
    }

    @Test
    public void testFilePersistenceObjects() throws Exception {
        helpTestPersistenceObjects(TEST_FILE_DATABASE, TEST_FILE_REPOSITORY_CONFIG);
    }

    @Test
    public void testLevelDBPersistenceObjects() throws Exception {
        helpTestPersistenceObjects(TEST_LEVELDB_DATABASE, TEST_LEVELDB_REPOSITORY_CONFIG);
    }

    /**
     * Test to ensure that the production repository
     * configuration files are valid and the repository
     * can be successfully started
     *
     * @throws Exception
     */
    @Test
    public void testProductionRepositoryConfiguration() throws Exception {
        initLocalRepository(LocalRepository.class, PRODUCTION_REPOSITORY_CONFIG);
        assertNotNull(_repo);

        UnitOfWork uow = _repo.createTransaction(RepositoryImpl.SYSTEM_USER, "test-search-type", true, null);
        List<KomodoObject> results = _repo.searchByType(uow, JcrConstants.NT_UNSTRUCTURED);
        assertTrue(results.size() > 0);

        boolean foundRoot = false;
        boolean foundWksp = false;
        boolean foundLibrary = false;
        for (KomodoObject ko : results) {
        	String path = ko.getAbsolutePath();
        	assertNotNull(path);

        	if (RepositoryImpl.KOMODO_ROOT.equals(path))
        		foundRoot = true;
        	else if (RepositoryImpl.LIBRARY_ROOT.equals(path))
        		foundLibrary = true;
        	else if(RepositoryImpl.komodoWorkspacePath(null).equals(path))
        		foundWksp = true;
        }

        assertTrue(foundRoot);
        assertTrue(foundLibrary);
        assertTrue(foundWksp);
    }

    private KomodoObject createMySqlDriver(UnitOfWork uow, KomodoObject parent, String name) throws Exception {
        KomodoObject driver = parent.addChild(uow, name, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE);
        InputStream contentStream = TestUtilities.mySqlDriver();
        assertNotNull(contentStream);
        byte[] content = FileUtils.write(contentStream);

        KomodoObject fileNode;
        if (! driver.hasChild(uow, JcrLexicon.CONTENT.getString()))
            fileNode = driver.addChild(uow, JcrLexicon.CONTENT.getString(), null);
        else
            fileNode = driver.getChild(uow, JcrLexicon.CONTENT.getString());

        ByteArrayInputStream stream = new ByteArrayInputStream(content);
        fileNode.setProperty(uow, JcrLexicon.DATA.getString(), stream);

        return driver;
    }

    @Test
    public void testBinaryValuePersistence() throws Exception {
        //
        // Get control values for testing the created properties against
        //
        InputStream contentStream = TestUtilities.mySqlDriver();
        assertNotNull(contentStream);
        byte[] content = FileUtils.write(contentStream);
        int contentAvailable = content.length;
        long mySqlChkSum = TestUtilities.checksum(content);

        int testObjectCount = 5;

        // Initialise the repository
        initLocalRepository(LocalRepository.class, PRODUCTION_REPOSITORY_CONFIG);
        assertNotNull(_repo);

        SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = _repo.createTransaction(TEST_USER, "test-persistence-binary-values", false, callback);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(uow);
        assertNotNull(workspace);

        //
        // Create objects with binary properties
        //
        for (int i = 1; i <= testObjectCount; ++i) {
            createMySqlDriver(uow, workspace, "test" + i);
        }

        //
        // Commit the transaction and await the response of the callback
        //
        uow.commit();

        //
        // Wait for the latch to countdown
        //
        assertTrue(callback.await(TIME_TO_WAIT, TimeUnit.MINUTES));
        assertFalse(callback.hasError());

        // Find the objects to confirm what we expect to happen
        uow = _repo.createTransaction(TEST_USER, "test-search-type", true, null);
        List<KomodoObject> results = _repo.searchByType(uow, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE);
        assertEquals(testObjectCount, results.size());
        uow.commit();

        // Shutdown the repository
        destroyLocalRepository();

        // Restart the repository
        initLocalRepository(LocalRepository.class, PRODUCTION_REPOSITORY_CONFIG);
        assertNotNull(_repo);

        // Find the test nodes to confirm repo was persisted
        uow = _repo.createTransaction(TEST_USER, "test-search-type", true, null);
        results = _repo.searchByType(uow, DataVirtLexicon.ResourceFile.DRIVER_FILE_NODE_TYPE);
        assertEquals(testObjectCount, results.size());

        //
        // Interrogate each of the drivers to confirm the
        // child file nodes still have their binary content
        //
        for (KomodoObject driver : results) {
            String name = driver.getName(uow);
            assertTrue(name.startsWith("test"));
            assertTrue(driver.hasChild(uow, JcrLexicon.CONTENT.getString()));

            KomodoObject fileNode = driver.getChild(uow, JcrLexicon.CONTENT.getString());

            Property property = fileNode.getProperty(uow, JcrLexicon.DATA.getString());

            InputStream binaryStream = property.getBinaryValue(uow);
            byte[] binaryBytes = FileUtils.write(binaryStream);
            assertEquals(contentAvailable, binaryBytes.length);
            assertEquals(mySqlChkSum, TestUtilities.checksum(binaryBytes));
        }

        uow.commit();
    }
}
