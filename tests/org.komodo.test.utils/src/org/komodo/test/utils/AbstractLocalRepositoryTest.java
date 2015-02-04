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
package org.komodo.test.utils;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import java.net.URL;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.komodo.repository.LocalRepository;
import org.komodo.repository.LocalRepository.LocalRepositoryId;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.spi.repository.RepositoryObserver;

/**
 * Provides framework for testing an instance of the local repository
 * which is only cached in memory hence should be persisted between
 * tests.
 *
 * The initLocalRepository method will be called prior to any tests executing
 * ensuring that the _repo is initialised and reachable. This can be added to
 * the singleton KEngine instance using KEngine.setDefaultRepository if required.
 *
 * When tests are complete, destroyLocalRepository will be called and attempt
 * to stop and close down the repository. Since the repository is in-memory only
 * then nullifying it will destroy all data hence clearance between test classes
 * should be unnecessary. Sub-classes using KEngine should stop the KEngine
 * in an @AfterClass annotated method and use the _repoObserver to await
 * the shutdown of the repository. The destoryLocalRepository function will
 * still run but it should do nothing since _repo is shutdown via the KEngine.
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractLocalRepositoryTest extends AbstractLoggingTest implements StringConstants {

    private static final String TEST_REPOSITORY_CONFIG = "test-local-repository-in-memory-config.json";

    protected static class LocalRepositoryObserver implements RepositoryObserver {

        private CountDownLatch latch;

        public LocalRepositoryObserver() {
            resetLatch();
        }

        public void resetLatch() {
            latch = new CountDownLatch(1);
        }

        /**
         * @return the latch
         */
        public CountDownLatch getLatch() {
            return this.latch;
        }

        @Override
        public void eventOccurred() {
            latch.countDown();
        }
    }

    protected static LocalRepository _repo = null;

    protected static LocalRepositoryObserver _repoObserver = null;

    @BeforeClass
    public static void initLocalRepository() throws Exception {

        URL configUrl = AbstractLocalRepositoryTest.class.getResource(TEST_REPOSITORY_CONFIG);

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
            throw new RuntimeException("Local repository did not start");
        }
    }

    /**
     * Shutdown and destroy repo
     *
     * @throws Exception
     */
    @AfterClass
    public static void destroyLocalRepository() throws Exception {
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

    @After
    public void clearLocalRepository() throws Exception {
        assertNotNull(_repo);

        if (! State.REACHABLE.equals(_repo.getState()))
            return;

        _repoObserver.resetLatch();

        RepositoryClient client = mock(RepositoryClient.class);
        RepositoryClientEvent event = RepositoryClientEvent.createClearEvent(client);
        _repo.notify(event);

        if (! _repoObserver.getLatch().await(1, TimeUnit.MINUTES))
            throw new RuntimeException("Local repository was not cleared");
    }

}