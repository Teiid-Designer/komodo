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
import static org.mockito.Mockito.mock;
import java.io.File;
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
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.test.utils.AbstractLoggingTest;
import org.komodo.test.utils.LocalRepositoryObserver;
import org.komodo.utils.FileUtils;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class TestLocalRepositoryPersistence extends AbstractLoggingTest implements StringConstants {

    private static final String TEST_DATABASE = System.getProperty("java.io.tmpdir") + 
                                                                                    File.separator
                                                                                    + "TestLocalRepoPersistence";

    private static final String TEST_REPOSITORY_CONFIG = "test-local-repository-on-file-config.json";

    protected LocalRepository _repo = null;

    protected LocalRepositoryObserver _repoObserver = null;

    private File testDb() {
        return new File(TEST_DATABASE);
    }

    private boolean dbExists() {
        File testDb = testDb();
        return testDb.exists();
    }

    private void deleteDbDir() {
        File testDb = testDb();
        if (testDb.exists())
            FileUtils.removeDirectoryAndChildren(testDb);
    }

    public void initLocalRepository() throws Exception {

        URL configUrl = TestLocalRepositoryPersistence.class.getResource(TEST_REPOSITORY_CONFIG);
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

        // Wait for the starting of the repository or timeout of 5 minutes
        if (!_repoObserver.getLatch().await(5, TimeUnit.MINUTES)) {
            throw new RuntimeException("Local repository did not start");
        }
    }

    /**
     * Shutdown and destroy repo
     *
     * @throws Exception
     */
    public void destroyLocalRepository() throws Exception {
        assertNotNull(_repo);
        assertNotNull(_repoObserver);

        if (State.REACHABLE.equals(_repo.getState())) {
            _repoObserver.resetLatch();

            RepositoryClient client = mock(RepositoryClient.class);
            RepositoryClientEvent event = RepositoryClientEvent.createShuttingDownEvent(client);
            _repo.notify(event);
        }

        if (! _repoObserver.getLatch().await(1, TimeUnit.MINUTES))
            throw new RuntimeException("Local repository was not stopped");

        _repo.removeObserver(_repoObserver);

        _repoObserver = null;
        _repo = null;
    }

    @Before
    public void setup() {
        deleteDbDir();
        assertFalse(dbExists());
    }

    @After
    public void cleanup() throws Exception {
        destroyLocalRepository();
        deleteDbDir();
    }

    @Test
    public void testPersistenceWorkspace() throws Exception {
        // Ensure the file store does not already exist
        assertFalse(dbExists());
        assertNull(_repo);

        // Initialise the repository
        initLocalRepository();
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(null);
        assertNotNull(workspace);

        // Find the workspace to confirm what we expect to happen
        List<KomodoObject> results = _repo.searchByType(null, KomodoLexicon.Workspace.NODE_TYPE);
        assertEquals(1, results.size());
        assertEquals(RepositoryImpl.WORKSPACE_ROOT, results.iterator().next().getAbsolutePath());

        // Shutdown the repository
        destroyLocalRepository();

        // Restart the repository
        initLocalRepository();
        assertNotNull(_repo);

        // Find the root and workspace to confirm repo was persisted
        results = _repo.searchByType(null, KomodoLexicon.Komodo.NODE_TYPE);
        assertEquals(1, results.size());

        results = _repo.searchByType(null, KomodoLexicon.Workspace.NODE_TYPE);
        assertEquals(1, results.size());
        assertEquals(RepositoryImpl.WORKSPACE_ROOT, results.iterator().next().getAbsolutePath());
    }

    @Test
    public void testPersistenceObjects() throws Exception {
        int testObjectCount = 5;

        // Ensure the file store does not already exist
        assertFalse(dbExists());
        assertNull(_repo);

        // Initialise the repository
        initLocalRepository();
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(null);
        assertNotNull(workspace);

        for (int i = 1; i <= testObjectCount; ++i) {
            KomodoObject child = workspace.addChild(null, "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(null, KomodoLexicon.VdbModel.MODEL_DEFINITION, "DDL");
        }

        // Find the objects to confirm what we expect to happen
        List<KomodoObject> results = _repo.searchByType(null, KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(testObjectCount, results.size());

        // Shutdown the repository
        destroyLocalRepository();

        // Restart the repository
        initLocalRepository();
        assertNotNull(_repo);

        // Find the test nodes to confirm repo was persisted
        results = _repo.searchByType(null, KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(testObjectCount, results.size());

        for (KomodoObject result : results) {
            String name = result.getName(null);
            assertTrue(name.startsWith("test"));

            Property property = result.getProperty(null, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertEquals("DDL", property.getStringValue(null));
        }
    }
}
