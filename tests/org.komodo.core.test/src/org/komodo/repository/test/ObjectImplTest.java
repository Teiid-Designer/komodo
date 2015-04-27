/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.repository.test;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.test.utils.AbstractLocalRepositoryTest;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ObjectImplTest extends AbstractLocalRepositoryTest {

    private static final String NAME = "blah";
    KomodoObject kobject;

    @Before
    public void init() throws Exception {
        this.kobject = _repo.add( null, null, NAME, null );
    }

    @Test
    public void shouldAddChild() throws Exception {
        final String name = "kid";
        final String type = "nt:folder";
        final KomodoObject child = this.kobject.addChild( null, name, type );
        assertThat( _repo.getFromWorkspace( null, child.getAbsolutePath() ), is( notNullValue() ) );
        assertThat( child.getPrimaryType( null ).getName(), is( type ) );
    }

    @Test
    public void shouldAddDescriptor() throws Exception {
        final String descriptorName = "mix:referenceable";
        this.kobject.addDescriptor( null, descriptorName );
        assertThat( this.kobject.hasDescriptor( null, descriptorName ), is( true ) );
        assertThat( this.kobject.getDescriptors( null ).length, is( 1 ) );
        assertThat( this.kobject.getDescriptors( null )[0].getName(), is( descriptorName ) );
    }

    @Test
    public void shouldAddMultipleDescriptors() throws Exception {
        final String descriptor1 = "mix:referenceable";
        final String descriptor2 = "mix:lockable";
        this.kobject.addDescriptor( null, descriptor1, descriptor2 );
        assertThat( this.kobject.hasDescriptor( null, descriptor1 ), is( true ) );
        assertThat( this.kobject.hasDescriptor( null, descriptor2 ), is( true ) );
        assertThat( this.kobject.getDescriptors( null ).length, is( 2 ) );
    }

    @Test
    public void shouldExist() throws Exception {
        final KomodoObject obj = _repo.getFromWorkspace( null, NAME );
        assertThat( obj, is( notNullValue() ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingChildWithEmptyName() throws Exception {
        this.kobject.addChild( null, EMPTY_STRING, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingChildWithNullName() throws Exception {
        this.kobject.addChild( null, null, null );
    }

    @Test( expected = KException.class )
    public void shouldFailAddingEmptyDescriptorName() throws Exception {
        this.kobject.addDescriptor( null, EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailAddingNullDescriptorName() throws Exception {
        this.kobject.addDescriptor( null, ( String )null );
    }

    @Test( expected = KException.class )
    public void shouldFailToGetChildIfItDoesNotExist() throws Exception {
        this.kobject.getChild( null, "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailToGetChildWithEmptyName() throws Exception {
        this.kobject.getChild( null, EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailToGetChildWithNullName() throws Exception {
        this.kobject.getChild( null, null );
    }

    @Test
    public void shouldGetChild() throws Exception {
        final String name = "kid";
        this.kobject.addChild( null, name, null );
        assertThat( this.kobject.getChild( null, name ), is( notNullValue() ) );
    }

    @Test
    public void shouldGetIndex() throws Exception {
        assertThat( this.kobject.getIndex(), is( 0 ) );
    }

    @Test
    public void shouldHaveUnknownTypeIdentifier() throws Exception {
        assertThat( this.kobject.getTypeIdentifier( null ), is( KomodoType.UNKNOWN ) );
    }

    @Test
    public void shouldRemove() throws Exception {
        final KomodoObject obj = _repo.getFromWorkspace( null, NAME );
        obj.remove( null );
        assertThat( _repo.getFromWorkspace( null, NAME ), is( nullValue() ) );
        assertThat( _repo.komodoWorkspace( null ).getChildren( null ).length, is( 0 ) );
    }

    @Test
    @Ignore("Mapping issue MODE-2463 - a remove then a re-add cannot be conducted in the same transaction")
    public void testRemoveThenAdd() throws Exception {
        String name = "testNode";

        UnitOfWork transaction1 = _repo.createTransaction("create-node-to-remove", false, null);
        KomodoObject wkspNode = _repo.komodoWorkspace(transaction1);
        assertNotNull(wkspNode);
        KomodoObject testNode = wkspNode.addChild(transaction1, name, null);
        assertNotNull(testNode);
        String testNodePath = testNode.getAbsolutePath();
        transaction1.commit();

        UnitOfWork transaction2 = _repo.createTransaction("node-removal", false, null);
        wkspNode = _repo.komodoWorkspace(transaction2);
        assertNotNull(wkspNode);
        testNode = _repo.getFromWorkspace(transaction2, testNodePath);
        assertNotNull(testNode);

        testNode.remove(transaction2);
        assertFalse(wkspNode.hasChild(transaction2, name));

        testNode = _repo.getFromWorkspace(transaction2, testNodePath);
        assertNull(testNode); 

        KomodoObject newTestNode = wkspNode.addChild(transaction2, name, null);

        /*
         * ISSUE #1
         *
         * This will fail with:
         * testNodePath = /{kworkspace}/testNode
         * testNode.getPath() = /{kworkspace}/testNode[2]
         */
        assertEquals(testNodePath, newTestNode.getAbsolutePath());        // Uncomment to view failure

        /*
         * ISSUE #2
         *
         * The path of newTestNode is alledgedly /{kworkspace}/testNode[2] so should
         * be able to find it from session2, except it fails
         */
        assertNotNull(_repo.getFromWorkspace(transaction2, testNodePath + "[2]")); //$NON-NLS-1$

        /*
         * ISSUE #3
         *
         * Despite newTestNode claiming its path is /{kworkspace}/testNode[2], transaction2
         * cannot find it so where is newTestNode?
         *
         * Turns out that its been added to /{kworkspace}/testNode which is the correct
         * path but not what is being reported by newTestNode.getPath()
         *
         * Conclusion: bug in node.getPath(), returning incorrect absolute path
         */
        KomodoObject kObject = _repo.getFromWorkspace(transaction2, testNodePath);
        assertEquals("Node path should equal " + testNodePath, testNodePath, kObject.getAbsolutePath()); //$NON-NLS-1$
        assertEquals(newTestNode.getAbsolutePath(), kObject.getAbsolutePath());
    }

    @Test
    public void shouldRemoveDescriptor() throws Exception {
        final String descriptorName = "mix:referenceable";
        this.kobject.addDescriptor( null, descriptorName );
        this.kobject.removeDescriptor( null, descriptorName );
        assertThat( this.kobject.hasDescriptor( null, descriptorName ), is( false ) );
        assertThat( this.kobject.getDescriptors( null ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveMultipleDescriptors() throws Exception {
        final String descriptor1 = "mix:referenceable";
        final String descriptor2 = "mix:lockable";
        this.kobject.addDescriptor( null, descriptor1, descriptor2 );
        this.kobject.removeDescriptor( null, descriptor1, descriptor2 );
        assertThat( this.kobject.hasDescriptor( null, descriptor1 ), is( false ) );
        assertThat( this.kobject.hasDescriptor( null, descriptor2 ), is( false ) );
        assertThat( this.kobject.getDescriptors( null ).length, is( 0 ) );
    }

    @Test
    public void shouldSetPrimaryType() throws Exception {
        final String newType = "nt:folder";
        this.kobject.setPrimaryType( null, newType );
        assertThat( this.kobject.getPrimaryType( null ).getName(), is( newType ) );
    }

}
