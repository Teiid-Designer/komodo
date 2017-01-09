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
package org.komodo.repository;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.PropertyDescriptor;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.modeshape.jcr.JcrNtLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class ObjectImplTest extends AbstractLocalRepositoryTest {

    private static final String NAME = "blah";
    KomodoObject kobject;

    @Before
    public void init() throws Exception {
        this.kobject = _repo.add( getTransaction(), null, NAME, null );
        commit();
    }

    @Test
    public void komodoRootShouldNotHaveAParent() throws Exception {
        assertThat( _repo.komodoRoot( getTransaction() ).getParent( getTransaction() ), is( ( KomodoObject )null ) );
    }

    @Test
    public void shouldAddChild() throws Exception {
        final String name = "kid";
        final String type = "nt:folder";
        final KomodoObject child = this.kobject.addChild( getTransaction(), name, type );
        assertThat( _repo.getFromWorkspace( getTransaction(), child.getAbsolutePath() ), is( notNullValue() ) );
        assertThat( child.getPrimaryType( getTransaction() ).getName(), is( type ) );
    }

    @Test
    public void shouldAddDescriptor() throws Exception {
        final String descriptorName = "mix:referenceable";
        this.kobject.addDescriptor( getTransaction(), descriptorName );
        assertThat( this.kobject.hasDescriptor( getTransaction(), descriptorName ), is( true ) );
        assertThat( this.kobject.getDescriptors( getTransaction() ).length, is( 1 ) );
        assertThat( this.kobject.getDescriptors( getTransaction() )[0].getName(), is( descriptorName ) );
    }

    @Test
    public void shouldAddMultipleDescriptors() throws Exception {
        final String descriptor1 = "mix:referenceable";
        final String descriptor2 = "mix:lockable";
        this.kobject.addDescriptor( getTransaction(), descriptor1, descriptor2 );
        assertThat( this.kobject.hasDescriptor( getTransaction(), descriptor1 ), is( true ) );
        assertThat( this.kobject.hasDescriptor( getTransaction(), descriptor2 ), is( true ) );
        assertThat( this.kobject.getDescriptors( getTransaction() ).length, is( 2 ) );
    }

    @Test
    public void shouldExist() throws Exception {
        final KomodoObject obj = _repo.getFromWorkspace( getTransaction(), NAME );
        assertThat( obj, is( notNullValue() ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingChildWithEmptyName() throws Exception {
        this.kobject.addChild( getTransaction(), EMPTY_STRING, null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailAddingChildWithNullName() throws Exception {
        this.kobject.addChild( getTransaction(), null, null );
    }

    @Test
    public void shouldFailAddingDescriptorToReservedPath() throws Exception {
        final String descriptorName = "mix:referenceable";

        for ( final String reservedPath : RepositoryImpl.getReservedPaths(getTransaction()) ) {
            try {
                final KomodoObject kobject = new ObjectImpl( _repo, reservedPath, 0 );
                kobject.addDescriptor( getTransaction(), descriptorName );
                fail( "addDescriptor should not be successful for path " + reservedPath );
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test( expected = KException.class )
    public void shouldFailAddingEmptyDescriptorName() throws Exception {
        this.kobject.addDescriptor( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = KException.class )
    public void shouldFailAddingNullDescriptorName() throws Exception {
        this.kobject.addDescriptor( getTransaction(), ( String )null );
    }

    @Test( expected = KException.class )
    public void shouldFailToGetChildIfItDoesNotExist() throws Exception {
        this.kobject.getChild( getTransaction(), "blah" );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailToGetChildWithEmptyName() throws Exception {
        this.kobject.getChild( getTransaction(), EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailToGetChildWithNullName() throws Exception {
        this.kobject.getChild( getTransaction(), null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailToGetChildWithEmptyType() throws Exception {
        final String name = "kid";
        this.kobject.addChild( getTransaction(), name, null );
        this.kobject.getChild( getTransaction(), name, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailToGetChildWithNullType() throws Exception {
        final String name = "kid";
        this.kobject.addChild( getTransaction(), name, null );
        this.kobject.getChild( getTransaction(), name, null );
    }

    @Test
    public void shouldGetChild() throws Exception {
        final String name = "kid";
        this.kobject.addChild( getTransaction(), name, null );
        assertThat( this.kobject.getChild( getTransaction(), name ), is( notNullValue() ) );
    }

    @Test( expected = KException.class )
    public void shouldFailToGetChildIfIncorrectType() throws Exception {
        final String name = "kid";
        this.kobject.addChild( getTransaction(), name, null );
        this.kobject.getChild( getTransaction(), name, JcrNtLexicon.FOLDER.getString() );
    }

    @Test
    public void shouldGetChildWithCorrectType() throws Exception {
        final String name = "kid";
        KomodoObject expected = this.kobject.addChild( getTransaction(), name, null );
        this.kobject.addChild( getTransaction(), name, JcrNtLexicon.FOLDER.getString() );
        assertThat( this.kobject.getChild( getTransaction(), name, JcrNtLexicon.UNSTRUCTURED.getString() ), is( expected ) );
    }

    @Test
    public void shouldGetIndex() throws Exception {
        assertThat( this.kobject.getIndex(), is( 0 ) );
    }

    @Test
    public void shouldGetNamedDescriptor() throws Exception {
        final String descriptorName = "mix:referenceable";
        this.kobject.addDescriptor( getTransaction(), descriptorName );
        this.kobject.addDescriptor( getTransaction(), "mix:lockable" );
        assertThat( this.kobject.getDescriptors( getTransaction() ).length, is( 2 ) );
        assertThat( this.kobject.getDescriptor( getTransaction(), descriptorName ).getName(), is( descriptorName ) );
    }

    @Test
    public void shouldHaveSameNumberRawDescriptorsAsDescriptors() throws Exception {
        assertThat( this.kobject.getDescriptors( getTransaction() ).length, is( this.kobject.getRawDescriptors( getTransaction() ).length ) );
    }

    @Test
    public void shouldHaveSameNumberRawPropertiesAsProperties() throws Exception {
        assertThat( this.kobject.getPropertyNames( getTransaction() ).length, is( this.kobject.getRawPropertyNames( getTransaction() ).length ) );
    }

    @Test
    public void shouldHaveUnknownTypeIdentifier() throws Exception {
        assertThat( this.kobject.getTypeIdentifier( getTransaction() ), is( KomodoType.UNKNOWN ) );
    }

    @Test
    public void shouldNotAllowRemovingObjectsWithReservedPaths() throws Exception {
        for ( final String reservedPath : RepositoryImpl.getReservedPaths(getTransaction()) ) {
            try {
                final KomodoObject kobject = new ObjectImpl( _repo, reservedPath, 0 );
                kobject.remove( getTransaction() );
                fail( "Remove should not be successful for reserved path " + reservedPath );
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldNotAllowRemovingChildObjectsWithReservedPaths() throws Exception {
        final KomodoObject komodoRoot = _repo.komodoRoot( getTransaction() );

        try {
            komodoRoot.removeChild( getTransaction(), _repo.komodoEnvironment( getTransaction() ).getName( getTransaction() ) );
            fail( "Root should not be able to remove the environment child object" );
        } catch ( final KException e ) {
            // expected
        }

        try {
            komodoRoot.removeChild( getTransaction(), _repo.komodoLibrary( getTransaction() ).getName( getTransaction() ) );
            fail( "Root should not be able to remove the library child object" );
        } catch ( final KException e ) {
            // expected
        }

        try {
            komodoRoot.removeChild( getTransaction(), _repo.komodoWorkspace( getTransaction() ).getName( getTransaction() ) );
            fail( "Root should not be able to remove the workspace child object" );
        } catch ( final KException e ) {
            // expected
        }
    }

    @Test
    public void shouldNotAllowRemovingDescriptorFromObjectWithReservedPath() throws Exception {
        final String descriptorName = "mix:referenceable";

        for ( final String reservedPath : RepositoryImpl.getReservedPaths(getTransaction()) ) {
            try {
                final KomodoObject kobject = new ObjectImpl( _repo, reservedPath, 0 );
                kobject.removeDescriptor( getTransaction(), descriptorName );
                fail( "Should not allow removing a descriptor from reserved path " + reservedPath );
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldNotAllowRenamingObjectWithReservedPath() throws Exception {
        for ( final String reservedPath : RepositoryImpl.getReservedPaths(getTransaction()) ) {
            try {
                final KomodoObject kobject = new ObjectImpl( _repo, reservedPath, 0 );
                kobject.rename( getTransaction(), "blah" );
                fail( "Should not allow renaming the object at reserved path " + reservedPath );
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldNotAllowSettingPrimaryTypeOfObjectsWithReservedPaths() throws Exception {
        for ( final String reservedPath : RepositoryImpl.getReservedPaths(getTransaction()) ) {
            try {
                final KomodoObject kobject = new ObjectImpl( _repo, reservedPath, 0 );
                kobject.setPrimaryType( getTransaction(), "nt:unstructured" );
                fail( "Should not allow setting primary type of the object at reserved path " + reservedPath );
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldNotHavePropertyWhenReservedPath() throws Exception {
        final String propertyName = "jcr:primaryType";

        UnitOfWork sysTx = sysTx();
        for ( final String reservedPath : RepositoryImpl.getReservedPaths(sysTx) ) {
            final KomodoObject kobject = new ObjectImpl( _repo, reservedPath, 0 );

            final Property property = kobject.getProperty( sysTx, propertyName );
            assertThat( property, is( nullValue() ) );

            final Property rawProperty = kobject.getRawProperty( sysTx, propertyName );
            assertThat( rawProperty, is( nullValue() ) );

            assertThat( kobject.hasProperty( sysTx, propertyName ), is( false ) );
            assertThat( kobject.hasRawProperty( sysTx, propertyName ), is( false ) );
        }
    }

    @Test
    public void shouldNotHavePropertyDescriptorWhenReservedPath() throws Exception {
        final String propertyName = "jcr:primaryType";

        UnitOfWork sysTx = sysTx();
        for ( final String reservedPath : RepositoryImpl.getReservedPaths(sysTx) ) {
            final KomodoObject kobject = new ObjectImpl( _repo, reservedPath, 0 );
            final PropertyDescriptor descriptor = kobject.getPropertyDescriptor( sysTx, propertyName );
            assertThat( descriptor, is( nullValue() ) );
        }
    }

    @Test
    public void shouldNotHavePropertyDescriptorsWhenReservedPath() throws Exception {
        UnitOfWork sysTx = sysTx();
        for ( final String reservedPath : RepositoryImpl.getReservedPaths(sysTx) ) {
            final KomodoObject kobject = new ObjectImpl( _repo, reservedPath, 0 );
            final PropertyDescriptor[] descriptors = kobject.getPropertyDescriptors( sysTx );
            assertThat( descriptors.length, is( 0 ) );

            final PropertyDescriptor[] rawDescriptors = kobject.getRawPropertyDescriptors( sysTx );
            assertThat( rawDescriptors.length, is( 0 ) );
        }
    }

    @Test
    public void shouldNotHavePropertyNamesWhenReservedPath() throws Exception {
        UnitOfWork sysTx = sysTx();
        for ( final String reservedPath : RepositoryImpl.getReservedPaths(sysTx) ) {
            final KomodoObject kobject = new ObjectImpl( _repo, reservedPath, 0 );
            final String[] names = kobject.getPropertyNames( sysTx );
            assertThat( names.length, is( 0 ) );

            final String[] rawNames = kobject.getRawPropertyNames( sysTx );
            assertThat( rawNames.length, is( 0 ) );
        }
    }

    @Test
    public void shouldRemove() throws Exception {
        final KomodoObject obj = _repo.getFromWorkspace( getTransaction(), NAME );
        obj.remove( getTransaction() );
        assertThat( _repo.getFromWorkspace( getTransaction(), NAME ), is( nullValue() ) );
        assertThat( _repo.komodoWorkspace( getTransaction() ).getChildren( getTransaction() ).length, is( 0 ) );
    }

    @Test
    @Ignore("Mapping issue MODE-2463 - a remove then a re-add cannot be conducted in the same transaction")
    public void testRemoveThenAdd() throws Exception {
        String name = "testNode";

        UnitOfWork transaction1 = getTransaction();
        KomodoObject wkspNode = _repo.komodoWorkspace(transaction1);
        assertNotNull(wkspNode);
        KomodoObject testNode = wkspNode.addChild(transaction1, name, null);
        assertNotNull(testNode);
        String testNodePath = testNode.getAbsolutePath();
        commit();

        UnitOfWork transaction2 = getTransaction();
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
        this.kobject.addDescriptor( getTransaction(), descriptorName );
        this.kobject.removeDescriptor( getTransaction(), descriptorName );
        assertThat( this.kobject.hasDescriptor( getTransaction(), descriptorName ), is( false ) );
        assertThat( this.kobject.getDescriptors( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldRemoveMultipleDescriptors() throws Exception {
        final String descriptor1 = "mix:referenceable";
        final String descriptor2 = "mix:lockable";
        this.kobject.addDescriptor( getTransaction(), descriptor1, descriptor2 );
        this.kobject.removeDescriptor( getTransaction(), descriptor1, descriptor2 );
        assertThat( this.kobject.hasDescriptor( getTransaction(), descriptor1 ), is( false ) );
        assertThat( this.kobject.hasDescriptor( getTransaction(), descriptor2 ), is( false ) );
        assertThat( this.kobject.getDescriptors( getTransaction() ).length, is( 0 ) );
    }

    @Test
    public void shouldSetPrimaryType() throws Exception {
        final String newType = "nt:folder";
        this.kobject.setPrimaryType( getTransaction(), newType );
        assertThat( this.kobject.getPrimaryType( getTransaction() ).getName(), is( newType ) );
    }

}
