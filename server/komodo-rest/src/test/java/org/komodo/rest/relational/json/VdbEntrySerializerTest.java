/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational.json;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.relational.RestVdbEntry;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbEntrySerializerTest {

    private static final String DESCRIPTION = "my description";
    private static final String JSON = "{\"id\":\"MyEntry\",\"path\":\"/my/entry/path\",\"description\":\"my description\"}";
    private static final String NAME = "MyEntry";
    private static final String PATH = "/my/entry/path";

    private RestVdbEntry entry;

    @Before
    public void init() {
        this.entry = new RestVdbEntry( NAME, PATH );
        this.entry.setDescription( DESCRIPTION );
    }

    @Test
    public void shouldExport() {
        assertThat( KomodoJsonMarshaller.marshall( this.entry ), is( JSON ) );
    }

    @Test
    public void shouldExportWhenDescriptionIsMissing() {
        this.entry.setDescription( null );
        final String json = "{\"id\":\"MyEntry\",\"path\":\"/my/entry/path\"}";
        assertThat( KomodoJsonMarshaller.marshall( this.entry ), is( json ) );
    }

    @Test
    public void shouldImport() {
        final RestVdbEntry entry = KomodoJsonMarshaller.unmarshall( JSON, RestVdbEntry.class );
        assertThat( entry.getDescription(), is( DESCRIPTION ) );
        assertThat( entry.getName(), is( NAME ) );
        assertThat( entry.getPath(), is( PATH ) );
        assertThat( entry.getLinks().length, is( 0 ) );
        assertThat( entry.getProperties().isEmpty(), is( true ) );
    }

    @Test
    public void shouldImportWhenDescriptionIsMissing() {
        final String json = "{\"id\":\"MyEntry\",\"path\":\"/my/entry/path\"}";
        final RestVdbEntry entry = KomodoJsonMarshaller.unmarshall( json, RestVdbEntry.class );
        assertThat( entry.getDescription(), is( nullValue() ) );
        assertThat( entry.getName(), is( NAME ) );
        assertThat( entry.getPath(), is( PATH ) );
        assertThat( entry.getLinks().length, is( 0 ) );
        assertThat( entry.getProperties().isEmpty(), is( true ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotExportWhenNameIsMissing() {
        final RestVdbEntry incomplete = new RestVdbEntry();
        incomplete.setDescription( DESCRIPTION );
        incomplete.setPath( PATH );
        KomodoJsonMarshaller.marshall( incomplete );
    }

    @Test( expected = Exception.class )
    public void shouldNotExportWhenPathIsMissing() {
        final RestVdbEntry incomplete = new RestVdbEntry();
        incomplete.setDescription( DESCRIPTION );
        incomplete.setName( NAME );
        KomodoJsonMarshaller.marshall( incomplete );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportWhenIdIsMissing() {
        final String malformed = "{\"path\":\"/my/entry/path\",\"description\":\"my description\"}";
        KomodoJsonMarshaller.unmarshall( malformed, RestVdbEntry.class );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportWhenPathIsMissing() {
        final String malformed = "{\"id\":\"MyEntry\",\"description\":\"my description\"}";
        KomodoJsonMarshaller.unmarshall( malformed, RestVdbEntry.class );
    }

}
