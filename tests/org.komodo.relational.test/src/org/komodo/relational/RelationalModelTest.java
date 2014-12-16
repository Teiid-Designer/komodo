/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.BeforeClass;
import org.komodo.repository.LocalRepository;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.spi.repository.RepositoryObserver;

@SuppressWarnings( {"javadoc", "nls"} )
public abstract class RelationalModelTest {

    protected static Repository _repo = null;

    @BeforeClass
    public static void getLocalRepositoryInstance() throws Exception {
        if (_repo != null) return;

        _repo = LocalRepository.getInstance();
        assertThat(_repo.getState(), is(State.UNKNOWN));
        assertThat(_repo.ping(), is(false));

        final CountDownLatch updateLatch = new CountDownLatch(1);
        final RepositoryObserver observer = new RepositoryObserver() {

            @Override
            public void stateChanged() {
                updateLatch.countDown();
            }
        };

        _repo.addObserver(observer);

        // Start the repository
        final RepositoryClient client = mock(RepositoryClient.class);
        final RepositoryClientEvent event = RepositoryClientEvent.createStartedEvent(client);
        _repo.notify(event);

        // Wait for the starting of the repository or timeout of 1 minute
        if (!updateLatch.await(1, TimeUnit.MINUTES)) {
            throw new RuntimeException("Local repository did not start");
        }
    }

}
