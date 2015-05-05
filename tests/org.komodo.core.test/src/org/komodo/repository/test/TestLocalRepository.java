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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.File;
import java.lang.reflect.Method;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.jcr.Node;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.repository.ObjectImpl;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.RepositoryTools;
import org.komodo.repository.SynchronousCallback;
import org.komodo.repository.SynchronousNestedCallback;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.Descriptor;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.Id;
import org.komodo.spi.repository.Repository.KeywordCriteria;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.modeshape.jcr.JcrNtLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public class TestLocalRepository extends AbstractLocalRepositoryTest {

    @Before
    public void assertReachable() {
        assertThat(_repo.ping(), is(true));
        assertThat(_repo.getState(), is(Repository.State.REACHABLE));
    }

    private String getPrimaryType( final KomodoObject kobject ) throws Exception {
        return kobject.getPrimaryType(this.uow).getName();
    }

    private boolean hasMixin( final String mixin,
                              final KomodoObject kobject ) throws Exception {
        for (final Descriptor descriptor : kobject.getDescriptors(this.uow)) {
            if (descriptor.getName().equals(mixin)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Tests to confirm that the transaction awaits the completion of the sequencers
     * prior to calling the given callback when creating an unrelated object.
     *
     * Confirms that the sequencers are called and complete (with nothing to do)
     *
     * @throws Exception
     */
    @Test
    public void shouldRespondWithCallback() throws Exception {
        // Ensure the workspace is created first and in a different transaction
        _repo.komodoWorkspace(this.uow);
        commit();

        final Boolean[] callbackCalled = new Boolean[1];
        callbackCalled[0] = false;

        //
        // Despite only creating a node the callback should be called
        // and the value of callbackCalled changed to true
        //
        UnitOfWorkListener delegate = new UnitOfWorkListener() {

            @Override
            public void respond(Object results) {
                callbackCalled[0] = true;
            }

            @Override
            public void errorOccurred(Throwable error) {
                // Nothing required since synchronous callback will log the error
            }
        };

        SynchronousCallback callback = new SynchronousNestedCallback(delegate);

        String name = "shouldRespondWithCallback";
        UnitOfWork transaction = createTransaction(name, callback);
        //
        // Create a single test node with no relationship to the sequencers or
        // with any relevant properties
        //
        _repo.add(transaction, RepositoryImpl.WORKSPACE_ROOT, "Test1", null);
        transaction.commit();

        //
        // Stop the test from completing prior to the callback returning
        //
        assertTrue(callback.await(TIME_TO_WAIT, TimeUnit.MINUTES));
        assertFalse(callback.hasError());

        //
        // The callback should have updated the value of callbackCalled to true
        //
        assertTrue(callbackCalled[0]);

        // Create a single test node with no relationship to the sequencers or
        // with any relevant properties
        //
        _repo.add(this.uow, RepositoryImpl.WORKSPACE_ROOT, "Test1", null);
        commit();
    }

    /**
     * Tests to confirm that the transaction awaits the completion of the sequencers
     * prior to calling the given callback when setting a property on an unrelated object
     *
     * Confirms that the sequencers are called and complete (with nothing to do)
     *
     * @throws Exception
     */
    @Test
    public void shouldRespondWithCallback2() throws Exception {
        // Ensure the workspace is created first and in a different transaction
        _repo.komodoWorkspace(this.uow);
        commit();

        // Create the test object to test the addition of a property
        KomodoObject testObject = _repo.add(this.uow, RepositoryImpl.WORKSPACE_ROOT, "Test1", null);
        assertNotNull(testObject);
        commit();

        final Boolean[] callbackCalled = new Boolean[1];
        callbackCalled[0] = false;

        //
        // Despite setting an unrelated (to the sequencers) property the callback
        // should be called and the value of callbackCalled changed to true
        //
        UnitOfWorkListener delegate = new UnitOfWorkListener() {

            @Override
            public void respond(Object results) {
                callbackCalled[0] = true;
            }

            @Override
            public void errorOccurred(Throwable error) {
                // Nothing required since synchronous callback will log the error
            }
        };

        SynchronousCallback callback = new SynchronousNestedCallback(delegate);

        String name = "shouldRespondWithCallback";
        UnitOfWork transaction = createTransaction(name, callback);
        //
        // Create a single test node with no relationship to the sequencers or
        // with any relevant properties
        //
        testObject.setProperty(transaction, "TestProperty1", "My property value");
        transaction.commit();

        //
        // Stop the test from completing prior to the callback returning
        //
        assertTrue(callback.await(TIME_TO_WAIT, TimeUnit.MINUTES));
        assertFalse(callback.hasError());

        //
        // The callback should have updated the value of callbackCalled to true
        //
        assertTrue(callbackCalled[0]);
    }

    @Test
    public void shouldAddWorkspaceItemAtRoot() throws Exception {
        // setup
        final String name = this.name.getMethodName();
        final KomodoObject rootNode = _repo.add(this.uow, null, name, null);
        commit();

        // tests
        assertThat(rootNode, is(notNullValue()));
        assertThat(rootNode.getName(this.uow), is(name));
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
        _repo.add(this.uow, "does-not-exist", "shouldFailToAddWorkspaceItemToNonexistingParent", null);
    }

    @Test( expected = KException.class )
    public void shouldFailToImportNonExistentResource() throws Exception {
        _repo.importFile(this.uow, new File("resources/bogus.xml"), "shouldFailToImportNonExistentResource", null);
    }

    @Test( expected = KException.class )
    public void shouldFailToRemoveWorkspaceItemThatDoesNotExist() throws Exception {
        _repo.remove(this.uow, "shouldFailToRemoveWorkspaceItemThatDoesNotExist");
    }

    @Test
    public void shouldGetId() {
        final Id id = _repo.getId();
        assertThat(id, is(notNullValue()));
        assertThat(id.getWorkspaceName(), is(StringConstants.DEFAULT_LOCAL_WORKSPACE_NAME));
    }

    @Test
    public void shouldGetNullWhenWorkspaceItemDoesNotExist() throws Exception {
        final KomodoObject doesNotExist = _repo.getFromWorkspace(this.uow, "shouldGetNullWhenWorkspaceItemDoesNotExist");
        assertThat(doesNotExist, is(nullValue()));
    }

    @Test
    public void shouldGetType() {
        assertThat(_repo.getType(), is(Repository.Type.LOCAL));
    }

    @Test
    public void shouldGetWorkspaceRoot() throws Exception {
        final KomodoObject rootNode = _repo.getFromWorkspace(this.uow, null);
        assertThat(rootNode, is(notNullValue()));
        assertThat(rootNode.getName(this.uow), is(KomodoLexicon.Komodo.WORKSPACE));
    }

    @Test( timeout = 60000 )
    public void shouldImportFile() throws Exception {
        // setup
        final String name = this.name.getMethodName();
        final KomodoObject kobject = _repo.importFile(this.uow, new File("resources/bare-bones.xml"), name, null);
        commit();

        // tests
        assertThat(kobject, is(notNullValue()));
        assertThat(kobject.getAbsolutePath(), is(RepositoryImpl.WORKSPACE_ROOT + FORWARD_SLASH + name));
        assertThat(kobject.getIndex(), is(0));
        assertThat(hasMixin(KomodoLexicon.WorkspaceItem.MIXIN_TYPE, kobject), is(true));
        assertThat(kobject.getName(this.uow), is(name));
        assertThat(kobject.getParent(this.uow), is(notNullValue()));
        assertThat(getPrimaryType(kobject), is(JcrNtLexicon.UNSTRUCTURED.getString()));
        assertThat(kobject.getRepository(), is((Repository)_repo));
        assertThat(kobject.hasChild(this.uow, KomodoLexicon.WorkspaceItem.ORIGINAL_FILE), is(true));
        assertThat(kobject.getChild(this.uow, KomodoLexicon.WorkspaceItem.ORIGINAL_FILE).getPrimaryType(this.uow).getName(),
                   is(JcrNtLexicon.FILE.getString()));
    }

    @Test
    public void shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist() throws Exception {
        final String item1 = "shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist-1";
        final String item2 = "shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist-2";

        // setup
        _repo.add( this.uow, null, item1, null );
        commit();

        try {
            _repo.remove(this.uow,
                         item1,
                         item2,
                         "shouldNotRemoveExistingItemsIfTryingToRemoveItemThatDoesNotExist-doesNotExist");
            fail();
        } catch (final KException e) {
            // tests
            assertThat(_repo.getFromWorkspace(this.uow, item1), is(notNullValue()));
            assertThat(_repo.getFromWorkspace(this.uow, item2), is(nullValue()));
        }
    }

    @Test
    public void shouldRemoveMultipleWorkspaceRootItems() throws Exception {
        // setup
        final String item1 = "shouldRemoveMultipleWorkspaceRootItems-1";
        _repo.add(this.uow, null, item1, null);
        final String item2 = "shouldRemoveMultipleWorkspaceRootItems-2";
        _repo.add(this.uow, null, item2, null);
        _repo.remove(this.uow, item1, item2);
        commit();

        // tests
        assertThat(_repo.getFromWorkspace(this.uow, item1), is(nullValue()));
        assertThat(_repo.getFromWorkspace(this.uow, item2), is(nullValue()));
    }

    @Test
    public void shouldRemoveWorkspaceRootItem() throws Exception {
        // setup
        final String name = this.name.getMethodName();
        _repo.add(this.uow, null, name, null);
        _repo.remove(this.uow, name);

        // tests
        assertThat(_repo.getFromWorkspace(this.uow, name), is(nullValue()));
    }

    private void verifyJcrNode(KomodoObject kObject) throws Exception {
        assertTrue(kObject instanceof ObjectImpl);
        ObjectImpl objImpl = (ObjectImpl) kObject;

        Method nodeMethod = ObjectImpl.class.getDeclaredMethod("node", UnitOfWork.class);
        nodeMethod.setAccessible(true);
        Object node = nodeMethod.invoke(objImpl, this.uow);
        assertTrue(node instanceof Node);
    }

    @Test
    public void shouldTraverseEntireRepository() throws Exception {
        KomodoObject komodoWksp = _repo.komodoWorkspace(this.uow);
        assertNotNull(komodoWksp);
        assertEquals(FORWARD_SLASH + KomodoLexicon.Komodo.NODE_TYPE +
                             FORWARD_SLASH + KomodoLexicon.Komodo.WORKSPACE,
                             komodoWksp.getAbsolutePath());
        verifyJcrNode(komodoWksp);

        KomodoObject komodoRoot = komodoWksp.getParent(this.uow);
        assertNotNull(komodoRoot);
        assertEquals(FORWARD_SLASH + KomodoLexicon.Komodo.NODE_TYPE, komodoRoot.getAbsolutePath());
        verifyJcrNode(komodoRoot);

        KomodoObject repoRoot = komodoRoot.getParent(this.uow);
        assertNotNull(repoRoot);
        assertEquals(FORWARD_SLASH, repoRoot.getAbsolutePath());
        verifyJcrNode(repoRoot);

        String result = RepositoryTools.traverse(this.uow, repoRoot);
        assertNotNull(result);
    }

    @Test
    public void shouldSearchForPrimaryType() throws Exception {
        KomodoObject komodoWksp = _repo.komodoWorkspace(this.uow);
        assertNotNull(komodoWksp);

        // Setup up 10 nodes to find
        for (int i = 1; i < 6; ++i) {
            KomodoObject child = komodoWksp.addChild(this.uow, "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION, "DDL");
        }

        for (int i = 6; i < 11; ++i) {
            KomodoObject child = komodoWksp.addChild(this.uow, "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION, "TEIIDSQL");
        }

        KomodoObject[] testNodes = komodoWksp.getChildrenOfType(this.uow, KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(10, testNodes.length);
        for (KomodoObject testKO : testNodes) {
            Property property = testKO.getProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertTrue(property.getStringValue(this.uow).equals("DDL") || property.getStringValue(this.uow).equals("TEIIDSQL"));
        }

        commit(); // session save needed before query

        // Perform the search
        List<KomodoObject> results = _repo.searchByType(this.uow, KomodoLexicon.VdbModel.NODE_TYPE);

        // Validate the results are as exepcted
        assertEquals(testNodes.length, results.size());
        for (KomodoObject searchObject : results) {
            String name = searchObject.getName(this.uow);
            assertTrue(name.startsWith("test"));

            String indexStr = name.substring(4);
            int index = Integer.parseInt(indexStr);
            assertTrue(index > 0 && index < 11);

            Property property = searchObject.getProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            if (index < 6)
                assertEquals("DDL", property.getStringValue(this.uow));
            else
                assertEquals("TEIIDSQL", property.getStringValue(this.uow));
        }
    }

    @Test
    public void shouldSearchForKeyword() throws Exception {
        KomodoObject komodoWksp = _repo.komodoWorkspace(this.uow);
        assertNotNull(komodoWksp);

        // Setup up 10 nodes to find
        for (int i = 1; i < 6; ++i) {
            KomodoObject child = komodoWksp.addChild(this.uow, "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION, "DDL");
        }

        for (int i = 6; i < 11; ++i) {
            KomodoObject child = komodoWksp.addChild(this.uow, "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION, "TEIIDSQL");
        }

        KomodoObject[] testNodes = komodoWksp.getChildrenOfType(this.uow, KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(10, testNodes.length);
        for (KomodoObject testKO : testNodes) {
            Property property = testKO.getProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertTrue(property.getStringValue(this.uow).equals("DDL") || property.getStringValue(this.uow).equals("TEIIDSQL"));
        }

        commit(); // session save needed before query

        // Perform the search
        List<KomodoObject> results = _repo.searchByKeyword(
                                                           this.uow, KomodoLexicon.VdbModel.NODE_TYPE,
                                                           KomodoLexicon.VdbModel.MODEL_DEFINITION,
                                                           KeywordCriteria.ANY,
                                                           "DDL");

        // Validate the results are as expected
        assertEquals(5, results.size());
        for (KomodoObject searchObject : results) {
            String name = searchObject.getName(this.uow);
            assertTrue(name.startsWith("test"));

            String indexStr = name.substring(4);
            int index = Integer.parseInt(indexStr);
            assertTrue(index > 0 && index < 11);

            Property property = searchObject.getProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            if (index < 6)
                assertEquals("DDL", property.getStringValue(this.uow));
        }
    }

    @Test
    public void shouldSearchForPath() throws Exception {
        KomodoObject komodoWksp = _repo.komodoWorkspace(this.uow);
        assertNotNull(komodoWksp);

        // Setup up 10 nodes to find
        for (int i = 1; i <= 5; ++i) {
            KomodoObject child = komodoWksp.addChild(this.uow, "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION, "DDL");
        }

        KomodoObject[] testNodes = komodoWksp.getChildrenOfType(this.uow, KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(5, testNodes.length);
        for (KomodoObject testKO : testNodes) {
            Property property = testKO.getProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertTrue(property.getStringValue(this.uow).equals("DDL") || property.getStringValue(this.uow).equals("TEIIDSQL"));
        }

        commit(); // session save needed before query

        // Perform the search
        for (int i = 1; i <= 5; ++i) {
            List<KomodoObject> results = _repo.searchByPath(this.uow,
                                                           komodoWksp.getAbsolutePath() + File.separator + "test" + i);
            // Validate the results are as expected
            assertEquals(1, results.size());
            KomodoObject searchObject = results.iterator().next();
            String name = searchObject.getName(this.uow);
            assertEquals("test" + i, name);

            Property property = searchObject.getProperty(this.uow, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertEquals("DDL", property.getStringValue(this.uow));
        }
    }
}
