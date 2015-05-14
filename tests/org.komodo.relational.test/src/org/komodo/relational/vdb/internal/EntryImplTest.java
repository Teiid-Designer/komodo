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
import org.komodo.relational.internal.RelationalModelFactory;
import org.komodo.relational.internal.RelationalObjectImpl;
import org.komodo.relational.vdb.Entry;
import org.komodo.relational.vdb.Vdb;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

@SuppressWarnings( { "javadoc", "nls" } )
public final class EntryImplTest extends RelationalModelTest {

    private Entry entry;
    private Vdb vdb;

    @Before
    public void init() throws Exception {
        this.vdb = RelationalModelFactory.createVdb( this.uow, _repo, null, "vdb", "/Users/sledge/hammer/MyVdb.vdb" );
        this.entry = RelationalModelFactory.createEntry( this.uow, _repo, this.vdb, "entry", "path" );
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
                new EntryImpl( this.uow, _repo, this.vdb.getAbsolutePath() );
                fail();
            } catch ( final KException e ) {
                // expected
            }
        }
    }

    @Test
    public void shouldHaveCorrectPrimaryType() throws Exception {
        assertThat( this.entry.getPrimaryType( this.uow ).getName(), is( VdbLexicon.Entry.ENTRY ) );
    }

    @Test
    public void shouldHaveMoreRawProperties() throws Exception {
        final String[] filteredProps = this.entry.getPropertyNames( this.uow );
        final String[] rawProps = this.entry.getRawPropertyNames( this.uow );
        assertThat( ( rawProps.length > filteredProps.length ), is( true ) );
    }

    @Test
    public void shouldHaveParentVdb() throws Exception {
        assertThat( this.entry.getParent( this.uow ), is( instanceOf( Vdb.class ) ) );
    }

    @Test
    public void shouldHavePathAfterConstruction() throws Exception {
        assertThat( this.entry.getPath( this.uow ), is( notNullValue() ) );
    }

    @Test( expected = UnsupportedOperationException.class )
    public void shouldNotAllowChildren() throws Exception {
        this.entry.addChild( this.uow, "blah", null );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetEmptyPPath() throws Exception {
        this.entry.setPath( this.uow, StringConstants.EMPTY_STRING );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldNotBeAbleToSetNullPath() throws Exception {
        this.entry.setPath( this.uow, null );
    }

    @Test
    public void shouldNotContainFilteredProperties() throws Exception {
        final String[] filteredProps = this.entry.getPropertyNames( this.uow );
        final Filter[] filters = this.entry.getFilters();

        for ( final String name : filteredProps ) {
            for ( final Filter filter : filters ) {
                assertThat( filter.rejectProperty( name ), is( false ) );
            }
        }
    }

    @Test
    public void shouldNotHaveDescriptionAfterConstruction() throws Exception {
        assertThat( this.entry.getDescription( this.uow ), is( nullValue() ) );
    }

    @Test
    public void shouldSetDescription() throws Exception {
        final String newValue = "newDescription";
        this.entry.setDescription( this.uow, newValue );
        assertThat( this.entry.getDescription( this.uow ), is( newValue ) );
    }

    @Test
    public void shouldSetPath() throws Exception {
        final String newValue = "newPath";
        this.entry.setPath( this.uow, newValue );
        assertThat( this.entry.getPath( this.uow ), is( newValue ) );
    }

}
