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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.File;
import java.lang.reflect.Method;
import javax.jcr.Node;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.modeshape.test.utils.AbstractLocalRepositoryTest;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.RepositoryTools;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.Id;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.jcr.JcrNtLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class TestLocalRepository extends AbstractLocalRepositoryTest implements StringConstants {

    private UnitOfWork createTransaction( final String name ) throws Exception {
        return _repo.createTransaction("transaction", false, null);
    }

    @Before
    public void assertReachable() {
        assertThat(_repo.ping(), is(true));
        assertThat(_repo.getState(), is(Repository.State.REACHABLE));
    }

    private String getPrimaryType( final KomodoObject kobject ) throws Exception {
        return kobject.getPrimaryType(null).getName();
    }

    private boolean hasMixin( final String mixin,
                              final KomodoObject kobject ) throws Exception {
        for (final Descriptor descriptor : kobject.getDescriptors(null)) {
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
        final UnitOfWork transaction = createTransaction(name);
        final KomodoObject rootNode = _repo.add(transaction, null, name, null);
        transaction.commit();

        // tests
        assertThat(rootNode, is(notNullValue()));
        assertThat(rootNode.getName(null), is(name));
        assertThat(rootNode.getAbsolutePath(), is(RepositoryImpl.WORKSPACE_ROOT + FORWARD_SLASH + name));
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

        transaction.commit();
    }

    @Test
    public void shouldCreateUpdateTransaction() throws Exception {
        // setup
        final String name = "elvis";
        final UnitOfWork transaction = _repo.createTransaction(name, false, null);

        // tests
        assertThat(transaction, is(notNullValue()));
        assertThat(transaction.getName(), is(name));
        assertThat(transaction.getCallback(), is(nullValue()));
        assertThat(transaction.isRollbackOnly(), is(false));

        transaction.commit();
    }

    @Test( expected = KException.class )
    public void shouldFailToAddWorkspaceItemToNonexistingParent() throws Exception {
        _repo.add(null, "does-not-exist", "shouldFailToAddWorkspaceItemToNonexistingParent", null);
    }

    @Test( expected = KException.class )
    public void shouldFailToImportNonExistentResource() throws Exception {
        _repo.importFile(null, new File("resources/bogus.xml"), "shouldFailToImportNonExistentResource", null);
    }

    @Test( expected = KException.class )
    public void shouldFailToRemoveWorkspaceItemThatDoesNotExist() throws Exception {
        _repo.remove(null, "shouldFailToRemoveWorkspaceItemThatDoesNotExist");
    }

    @Test
    public void shouldGetId() {
        final Id id = _repo.getId();
        assertThat(id, is(notNullValue()));
        assertThat(id.getWorkspaceName(), is(StringConstants.DEFAULT_LOCAL_WORKSPACE_NAME));
    }

    @Test
    public void shouldGetNullWhenWorkspaceItemDoesNotExist() throws Exception {
        final KomodoObject doesNotExist = _repo.getFromWorkspace(null, "shouldGetNullWhenWorkspaceItemDoesNotExist");
        assertThat(doesNotExist, is(nullValue()));
    }

    @Test
    public void shouldGetType() {
        assertThat(_repo.getType(), is(Repository.Type.LOCAL));
    }

    @Test
    public void shouldGetWorkspaceRoot() throws Exception {
        final KomodoObject rootNode = _repo.getFromWorkspace(null, null);
        assertThat(rootNode, is(notNullValue()));
        assertThat(rootNode.getName(null), is(KomodoLexicon.Komodo.WORKSPACE));
    }

    @Test( timeout = 60000 )
    public void shouldImportFile() throws Exception {
        // setup
        final UnitOfWork transaction = createTransaction("shouldImportFile");
        final String name = "bareBones";
        final KomodoObject kobject = _repo.importFile(transaction, new File("resources/bare-bones.xml"), name, null);
        transaction.commit();

        // tests
        assertThat(kobject, is(notNullValue()));
        assertThat(kobject.getAbsolutePath(), is(RepositoryImpl.WORKSPACE_ROOT + FORWARD_SLASH + name));
        assertThat(kobject.getIndex(), is(0));
        assertThat(hasMixin(KomodoLexicon.WorkspaceItem.MIXIN_TYPE, kobject), is(true));
        assertThat(kobject.getName(null), is(name));
        assertThat(kobject.getParent(null), is(notNullValue()));
        assertThat(getPrimaryType(kobject), is(JcrNtLexicon.UNSTRUCTURED.getString()));
        assertThat(kobject.getRepository(), is((Repository)_repo));
        assertThat(kobject.hasChild(null, KomodoLexicon.WorkspaceItem.ORIGINAL_FILE), is(true));
        assertThat(kobject.getChild(null, KomodoLexicon.WorkspaceItem.ORIGINAL_FILE).getPrimaryType(null).getName(),
                   is(JcrNtLexicon.FILE.getString()));
    }

    @Test
    public void shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist() throws Exception {
        final String item1 = "shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist-1";
        final String item2 = "shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist-2";

        { // setup
            final UnitOfWork transaction = createTransaction("shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist");
            _repo.add(transaction, null, item1, null);
            _repo.add(transaction, null, item2, null);
            transaction.commit();
        }

        try {
            final UnitOfWork transaction = createTransaction("shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist");
            _repo.remove(transaction,
                         item1,
                         item2,
                         "shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist-doesNotExist");
            fail();
        } catch (final KException e) {
            // tests
            assertThat(_repo.getFromWorkspace(null, item1), is(notNullValue()));
            assertThat(_repo.getFromWorkspace(null, item2), is(notNullValue()));
        }
    }

    @Test
    public void shouldRemoveMultipleWorkspaceRootItems() throws Exception {
        // setup
        final UnitOfWork transaction = createTransaction("shouldRemoveMultipleWorkspaceRootItems");
        final String item1 = "shouldRemoveMultipleWorkspaceRootItems-1";
        _repo.add(transaction, null, item1, null);
        final String item2 = "shouldRemoveMultipleWorkspaceRootItems-2";
        _repo.add(transaction, null, item2, null);
        _repo.remove(transaction, item1, item2);
        transaction.commit();

        // tests
        assertThat(_repo.getFromWorkspace(null, item1), is(nullValue()));
        assertThat(_repo.getFromWorkspace(null, item2), is(nullValue()));
    }

    @Test
    public void shouldRemoveWorkspaceRootItem() throws Exception {
        // setup
        final UnitOfWork transaction = createTransaction("shouldRemoveWorkspaceRootItem");
        final String name = "shouldRemoveWorkspaceRootItem";
        _repo.add(transaction, null, name, null);
        _repo.remove(transaction, name);

        // tests
        assertThat(_repo.getFromWorkspace(null, name), is(nullValue()));
    }

    private void verifyJcrNode(KomodoObject kObject) throws Exception {
        assertTrue(kObject instanceof ObjectImpl);
        ObjectImpl objImpl = (ObjectImpl) kObject;

        Method nodeMethod = ObjectImpl.class.getDeclaredMethod("node", UnitOfWork.class);
        nodeMethod.setAccessible(true);
        Object node = nodeMethod.invoke(objImpl, (UnitOfWork) null);
        assertTrue(node instanceof Node);
    }

    @Test
    public void shouldTraverseEntireRepository() throws Exception {
        KomodoObject komodoWksp = _repo.komodoWorkspace(null);
        assertNotNull(komodoWksp);
        assertEquals(FORWARD_SLASH + KomodoLexicon.Komodo.NODE_TYPE +
                             FORWARD_SLASH + KomodoLexicon.Komodo.WORKSPACE,
                             komodoWksp.getAbsolutePath());
        verifyJcrNode(komodoWksp);

        KomodoObject komodoRoot = komodoWksp.getParent(null);
        assertNotNull(komodoRoot);
        assertEquals(FORWARD_SLASH + KomodoLexicon.Komodo.NODE_TYPE, komodoRoot.getAbsolutePath());
        verifyJcrNode(komodoRoot);

        KomodoObject repoRoot = komodoRoot.getParent(null);
        assertNotNull(repoRoot);
        assertEquals(FORWARD_SLASH, repoRoot.getAbsolutePath());
        verifyJcrNode(repoRoot);

        String result = RepositoryTools.traverse(repoRoot);
        assertNotNull(result);
    }

}
