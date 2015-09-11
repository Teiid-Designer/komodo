/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbEntryTest {

    private static final String DESCRIPTION = "my description";
    private static final String NAME = "MyEntry";
    private static final String PATH = "/my/entry/path";

    private RestVdbEntry entry;

    @Before
    public void init() {
        this.entry = new RestVdbEntry( NAME, PATH );
        this.entry.setDescription( DESCRIPTION );
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbEntry thatEntry = new RestVdbEntry( this.entry.getName(), this.entry.getPath() );
        thatEntry.setDescription( this.entry.getDescription() );
        thatEntry.setLinks( this.entry.getLinks() );
        thatEntry.setProperties( this.entry.getProperties() );

        assertThat( this.entry, is( thatEntry ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyEntries() {
        final RestVdbEntry empty1 = new RestVdbEntry();
        final RestVdbEntry empty2 = new RestVdbEntry();
        assertThat( empty1, is( empty2 ) );
    }

    @Test
    public void shouldConstructEmptyEntry() {
        final RestVdbEntry empty = new RestVdbEntry();
        assertThat( empty.getDescription(), is( nullValue() ) );
        assertThat( empty.getName(), is( nullValue() ) );
        assertThat( empty.getPath(), is( nullValue() ) );
        assertThat( empty.getProperties().isEmpty(), is( true ) );
        assertThat( empty.getLinks().length, is( 0 ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbEntry thatEntry = new RestVdbEntry( this.entry.getName(), this.entry.getPath() );
        thatEntry.setDescription( this.entry.getDescription() );
        thatEntry.setLinks( this.entry.getLinks() );
        thatEntry.setProperties( this.entry.getProperties() );

        assertThat( this.entry.hashCode(), is( thatEntry.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenDescriptionIsDifferent() {
        final RestVdbEntry thatEntry = new RestVdbEntry( this.entry.getName(), this.entry.getPath() );
        thatEntry.setDescription( this.entry.getDescription() + "blah" );
        thatEntry.setLinks( this.entry.getLinks() );
        thatEntry.setProperties( this.entry.getProperties() );

        assertThat( this.entry.getDescription(), is( not( thatEntry.getDescription() ) ) );
        assertThat( this.entry, is( not( thatEntry ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenNameIsDifferent() {
        final RestVdbEntry thatEntry = new RestVdbEntry( this.entry.getName() + "blah", this.entry.getPath() );
        thatEntry.setDescription( this.entry.getDescription() );
        thatEntry.setLinks( this.entry.getLinks() );
        thatEntry.setProperties( this.entry.getProperties() );

        assertThat( this.entry.getName(), is( not( thatEntry.getName() ) ) );
        assertThat( this.entry, is( not( thatEntry ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenPathIsDifferent() {
        final RestVdbEntry thatEntry = new RestVdbEntry( this.entry.getName(), this.entry.getPath() + "blah" );
        thatEntry.setDescription( this.entry.getDescription() );
        thatEntry.setLinks( this.entry.getLinks() );
        thatEntry.setProperties( this.entry.getProperties() );

        assertThat( this.entry.getPath(), is( not( thatEntry.getPath() ) ) );
        assertThat( this.entry, is( not( thatEntry ) ) );
    }

    @Test
    public void shouldSetDescription() {
        final String newDescription = "blah";
        this.entry.setDescription( newDescription );
        assertThat( this.entry.getDescription(), is( newDescription ) );
    }

    @Test
    public void shouldSetName() {
        final String newName = "blah";
        this.entry.setName( newName );
        assertThat( this.entry.getName(), is( newName ) );
    }

    @Test
    public void shouldSetPath() {
        final String newPath = "blah";
        this.entry.setPath( newPath );
        assertThat( this.entry.getPath(), is( newPath ) );
    }

}
