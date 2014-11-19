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
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import java.io.File;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.repository.LocalRepository;
import org.komodo.repository.internal.RepositoryImpl;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.spi.repository.RepositoryObserver;
import org.modeshape.jcr.JcrNtLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class TestLocalRepository {

    private static LocalRepository _repo = null;

    @BeforeClass
    public static void getLocalRepositoryInstance() throws Exception {
        _repo = LocalRepository.getInstance();

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

    @Before
    public void assertReachable() {
        assertThat(_repo.ping(), is(true));
        assertThat(_repo.getState(), is(Repository.State.REACHABLE));
    }

    private boolean hasMixin( final String mixin,
                              final KomodoObject kobject ) throws Exception {
        for (final Descriptor descriptor : kobject.getDescriptors()) {
            if (descriptor.getName().equals(mixin)) {
                return true;
            }
        }

        return false;
    }

    private String getPrimaryType(final KomodoObject kobject) throws Exception {
        return kobject.getPrimaryType().getName();
    }

    @Test( timeout = 60000 )
    public void testImport() throws Exception {
        // setup
        final String name = "bareBones";
        final KomodoObject kobject = _repo.importFile(new File("resources/bare-bones.xml"), name, null);

        // tests
        assertThat(kobject, is(notNullValue()));
        assertThat(kobject.getAbsolutePath(), is(RepositoryImpl.WORKSPACE_ROOT + name));
        assertThat(kobject.getIndex(), is(0));
        assertThat(hasMixin(KomodoLexicon.WorkspaceItem.MIXIN_TYPE, kobject), is(true));
        assertThat(kobject.getName(), is(name));
        assertThat(kobject.getParent(), is(nullValue()));
        assertThat(getPrimaryType(kobject), is(JcrNtLexicon.UNSTRUCTURED.getString()));
        assertThat(kobject.getRepository(), is((Repository)_repo));
    }

}
