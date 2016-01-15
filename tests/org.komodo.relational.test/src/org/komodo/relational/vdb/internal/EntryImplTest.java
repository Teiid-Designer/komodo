/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.vdb.internal;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.RelationalModelTest;
import org.komodo.relational.RelationalObject.Filter;
import org.komodo.relational.RelationalProperties;
import org.komodo.relational.RelationalProperty;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class EntryImplTest extends RelationalModelTest {

    private Entry entry;

    @Before
    public void init() throws Exception {
        final Vdb vdb = createVdb();
        this.entry = vdb.addEntry( getTransaction(), "entry", "path" );
        commit();
    }

    @Test
    public void shouldBeChildRestricted() {
        assertThat( this.entry.isChildRestricted(), is( true ) );
    }

    @Test
    public void shouldFailConstructionIfNotEntry() {
        if ( RelationalObjectImpl.VALIDATE_INITIAL_STATE ) {
            try {
                new EntryImpl( getTransaction(), _repo, this.entry.getParent( getTransaction() ).getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.entry.getPrimaryType( getTransaction() ).getName(), is( VdbLexicon.Entry.ENTRY ) );
    }

    @Test
    public void shouldHaveCorrectTypeIdentifier() throws Exception {
        assertThat(this.entry.getTypeIdentifier( getTransaction() ), is(KomodoType.VDB_ENTRY));
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.entry.getPropertyNames( getTransaction() );
        final String[] rawProps = this.entry.getRawPropertyNames( getTransaction() );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentVdb() throws Exception {
        assertThat( this.entry.getParent( getTransaction() ), is( instanceOf( Vdb.class ) ) );
    }

    @Test
    public void shouldHavePathAfterConstruction() throws Exception {
        assertThat( this.entry.getPath( getTransaction() ), is( notNullValue() ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.entry.addChild( getTransaction(), "blah", null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetEmptyPPath() throws Exception {
        this.entry.setPath( getTransaction(), StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetNullPath() throws Exception {
        this.entry.setPath( getTransaction(), null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.entry.getPropertyNames( getTransaction() );
        final Filter[] filters = this.entry.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveDescriptionAfterConstruction() throws Exception {
        assertThat( this.entry.getDescription( getTransaction() ), is( nullValue() ) );
    }

    @Test
    public void shouldRename() throws Exception {
        final String newName = "blah";
        this.entry.rename( getTransaction(), newName );
        assertThat( this.entry.getName( getTransaction() ), is( newName ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String newValue = "newDescription";
        this.entry.setDescription( getTransaction(), newValue );
        assertThat( this.entry.getDescription( getTransaction() ), is( newValue ) );
    }

    @Test
    public void shouldSetPath() throws Exception {
        final String newValue = "newPath";
        this.entry.setPath( getTransaction(), newValue );
        assertThat( this.entry.getPath( getTransaction() ), is( newValue ) );
    }

    /*
     * ********************************************************************
     * *****                  Resolver Tests                          *****
     * ********************************************************************
     */

    @Test
    public void shouldCreateUsingResolver() throws Exception {
        final String name = "blah";

        final RelationalProperties props = new RelationalProperties();
        props.add( new RelationalProperty( VdbLexicon.Entry.PATH, "entryPath" ) );

        final KomodoObject kobject = Entry.RESOLVER.create( getTransaction(), _repo, this.entry.getParent( getTransaction() ), name, props );
        assertThat( kobject, is( notNullValue() ) );
        assertThat( kobject, is( instanceOf( Entry.class ) ) );
        assertThat( kobject.getName( getTransaction() ), is( name ) );
    }

    @Test( expected = KException.class )
    public void shouldFailCreateUsingResolverWithInvalidParent() throws Exception {
        final KomodoObject bogusParent = _repo.add( getTransaction(), null, "bogus", null );
        Entry.RESOLVER.create( getTransaction(), _repo, bogusParent, "blah", new RelationalProperties() );
    }

}
