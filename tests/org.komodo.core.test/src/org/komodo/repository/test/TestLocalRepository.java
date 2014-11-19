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
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import java.io.File;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.modeshape.test.utils.AbstractLoggingTest;
import org.komodo.repository.LocalRepository;
import org.komodo.repository.internal.RepositoryImpl;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.Id;
import org.komodo.spi.repository.Repository.State;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.RepositoryClient;
import org.komodo.spi.repository.RepositoryClientEvent;
import org.komodo.spi.repository.RepositoryObserver;
import org.modeshape.jcr.JcrNtLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class TestLocalRepository extends AbstractLoggingTest {

    private static LocalRepository _repo = null;

    @BeforeClass
    public static void getLocalRepositoryInstance() throws Exception {

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

    @Before
    public void assertReachable() {
        assertThat(_repo.ping(), is(true));
        assertThat(_repo.getState(), is(Repository.State.REACHABLE));
    }

    private String getPrimaryType( final KomodoObject kobject ) throws Exception {
        return kobject.getPrimaryType().getName();
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

    @Test
    public void shouldAddWorkspaceItemAtRoot() throws Exception {
        // setup
        final String name = "shouldAddWorkspaceItemAtRoot";
        final KomodoObject rootNode = _repo.add(null, name);

        // tests
        assertThat(rootNode, is(notNullValue()));
        assertThat(rootNode.getName(), is(name));
        assertThat(rootNode.getAbsolutePath(), is(RepositoryImpl.WORKSPACE_ROOT + name));
    }

    @Test
    public void shouldCreateRollbackTransaction() throws Exception {
        // setup
        final String name = "elvis";
        final UnitOfWork transaction = _repo.createTransaction(name, true, null);

        // tests
        assertThat(transaction, is(notNullValue()));
        assertThat(transaction.getName(), is(name));
        assertThat(transaction.getCallback(), is(nullValue()));
        assertThat(transaction.isRollbackOnly(), is(true));
    }

    @Test
    public void shouldCreateTransaction() throws Exception {
        // setup
        final String name = "elvis";
        final UnitOfWork transaction = _repo.createTransaction(name, false, null);

        // tests
        assertThat(transaction, is(notNullValue()));
        assertThat(transaction.getName(), is(name));
        assertThat(transaction.getCallback(), is(nullValue()));
        assertThat(transaction.isRollbackOnly(), is(false));
    }

    @Test( expected = KException.class )
    public void shouldFailToAddWorkspaceItemToNonexistingParent() throws Exception {
        _repo.add("does-not-exist", "shouldFailToAddWorkspaceItemToNonexistingParent");
    }

    @Test( expected = KException.class )
    public void shouldFailToImportNonExistentResource() throws Exception {
        _repo.importFile(new File("resources/bogus.xml"), "shouldFailToImportNonExistentResource", null);
    }

    @Test( expected = KException.class )
    public void shouldFailToRemoveWorkspaceItemThatDoesNotExist() throws Exception {
        _repo.remove("shouldFailToRemoveWorkspaceItemThatDoesNotExist");
    }

    @Test
    public void shouldGetId() {
        final Id id = _repo.getId();
        assertThat(id, is(notNullValue()));
        assertThat(id.getName(), is(StringConstants.LOCAL_REPOSITORY));
    }

    @Test
    public void shouldGetNullWhenWorkspaceItemDoesNotExist() throws Exception {
        final KomodoObject doesNotExist = _repo.get("shouldGetNullWhenWorkspaceItemDoesNotExist");
        assertThat(doesNotExist, is(nullValue()));
    }

    @Test
    public void shouldGetType() {
        assertThat(_repo.getType(), is(Repository.Type.LOCAL));
    }

    @Test
    public void shouldGetWorkspaceRoot() throws Exception {
        final KomodoObject rootNode = _repo.get(null);
        assertThat(rootNode, is(notNullValue()));
        assertThat(rootNode.getName(), is(KomodoLexicon.Komodo.WORKSPACE));
    }

    @Test( timeout = 60000 )
    public void shouldImportFile() throws Exception {
        // setup
        final String name = "bareBones";
        final KomodoObject kobject = _repo.importFile(new File("resources/bare-bones.xml"), name, null);

        // tests
        assertThat(kobject, is(notNullValue()));
        assertThat(kobject.getAbsolutePath(), is(RepositoryImpl.WORKSPACE_ROOT + name));
        assertThat(kobject.getIndex(), is(0));
        assertThat(hasMixin(KomodoLexicon.WorkspaceItem.MIXIN_TYPE, kobject), is(true));
        assertThat(kobject.getName(), is(name));
        assertThat(kobject.getParent(), is(notNullValue()));
        assertThat(getPrimaryType(kobject), is(JcrNtLexicon.UNSTRUCTURED.getString()));
        assertThat(kobject.getRepository(), is((Repository)_repo));
        assertThat(kobject.hasChild(KomodoLexicon.WorkspaceItem.ORIGINAL_FILE), is(true));
        assertThat(kobject.getChild(KomodoLexicon.WorkspaceItem.ORIGINAL_FILE).getPrimaryType().getName(),
                   is(JcrNtLexicon.FILE.getString()));
    }

    @Test
    public void shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist() throws Exception {
        // setup
        final String item1 = "shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist-1";
        _repo.add(null, item1);
        final String item2 = "shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist-2";
        _repo.add(null, item2);

        try {
            _repo.remove(item1, item2, "shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist-doesNotExist");
            fail();
        } catch (final KException e) {
            // tests
            assertThat(_repo.get(item1), is(notNullValue()));
            assertThat(_repo.get(item2), is(notNullValue()));
        }
    }

    @Test
    public void shouldRemoveMultipleWorkspaceRootItems() throws Exception {
        // setup
        final String item1 = "shouldRemoveMultipleWorkspaceRootItems-1";
        _repo.add(null, item1);
        final String item2 = "shouldRemoveMultipleWorkspaceRootItems-2";
        _repo.add(null, item2);
        _repo.remove(item1, item2);

        // tests
        assertThat(_repo.get(item1), is(nullValue()));
        assertThat(_repo.get(item2), is(nullValue()));
    }

    @Test
    public void shouldRemoveWorkspaceRootItem() throws Exception {
        // setup
        final String name = "shouldRemoveWorkspaceRootItem";
        _repo.add(null, name);
        _repo.remove(name);

        // tests
        assertThat(_repo.get(name), is(nullValue()));
    }

}
