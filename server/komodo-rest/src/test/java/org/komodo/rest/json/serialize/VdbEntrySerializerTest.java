/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json.serialize;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;
import static org.junit.Assert.assertThat;
import static org.komodo.rest.json.JsonConstants.JSON_BUILDER;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.json.RestVdbEntry;

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
        assertThat( JSON_BUILDER.toJson( this.entry ), is( JSON ) );
    }

    @Test
    public void shouldExportWhenDescriptionIsMissing() {
        this.entry.setDescription( null );
        final String json = "{\"id\":\"MyEntry\",\"path\":\"/my/entry/path\"}";
        assertThat( JSON_BUILDER.toJson( this.entry ), is( json ) );
    }

    @Test
    public void shouldImport() {
        final RestVdbEntry entry = JSON_BUILDER.fromJson( JSON, RestVdbEntry.class );
        assertThat( entry.getDescription(), is( DESCRIPTION ) );
        assertThat( entry.getName(), is( NAME ) );
        assertThat( entry.getPath(), is( PATH ) );
        assertThat( entry.getLinks().length, is( 0 ) );
        assertThat( entry.getProperties().isEmpty(), is( true ) );
    }

    @Test
    public void shouldImportWhenDescriptionIsMissing() {
        final String json = "{\"id\":\"MyEntry\",\"path\":\"/my/entry/path\"}";
        final RestVdbEntry entry = JSON_BUILDER.fromJson( json, RestVdbEntry.class );
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
        JSON_BUILDER.toJson( incomplete );
    }

    @Test( expected = Exception.class )
    public void shouldNotExportWhenPathIsMissing() {
        final RestVdbEntry incomplete = new RestVdbEntry();
        incomplete.setDescription( DESCRIPTION );
        incomplete.setName( NAME );
        JSON_BUILDER.toJson( incomplete );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportWhenIdIsMissing() {
        final String malformed = "{\"path\":\"/my/entry/path\",\"description\":\"my description\"}";
        JSON_BUILDER.fromJson( malformed, RestVdbEntry.class );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportWhenPathIsMissing() {
        final String malformed = "{\"id\":\"MyEntry\",\"description\":\"my description\"}";
        JSON_BUILDER.fromJson( malformed, RestVdbEntry.class );
    }

}
