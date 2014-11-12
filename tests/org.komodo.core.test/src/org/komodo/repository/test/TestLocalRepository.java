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

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.Test;
import org.komodo.repository.LocalRepository;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.spi.repository.RepositoryObserver;
import org.komodo.spi.repository.RepositoryClientEvent;

/**
 *
 */
@SuppressWarnings( "javadoc" )
public class TestLocalRepository {

    @Test(timeout = 300000)
    public void testLocalRepositoryStart() throws InterruptedException {
        LocalRepository localRepo = LocalRepository.getInstance();

        final CountDownLatch updateLatch = new CountDownLatch(1);
        RepositoryObserver observer = new RepositoryObserver() {
            
            @Override
            public void stateChanged() {
                updateLatch.countDown();
            }
        };
        localRepo.addObserver(observer);

        // Try and start the repository
        RepositoryClient client = mock(RepositoryClient.class);
        RepositoryClientEvent event = RepositoryClientEvent.createStartedEvent(client);
        localRepo.notify(event);

        // Wait for the starting of the repository or timeout of 3 minutes
        updateLatch.await(3, TimeUnit.MINUTES);

        // Assert the repository has started and is reachable
        assertEquals(Repository.State.REACHABLE, localRepo.getState());
    }
}
