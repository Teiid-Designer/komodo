/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json.serialize;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import static org.komodo.rest.json.JsonConstants.JSON_BUILDER;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.json.RestVdb;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbSerializerTest {

    private static final String DESCRIPTION = "my description";
    private static final String JSON = "{\"id\":\"MyVdb\",\"description\":\"my description\",\"originalFile\":\"/Users/ElvisIsKing/MyVdb.xml\"}";
    private static final String ORIGINAL_FILE = "/Users/ElvisIsKing/MyVdb.xml";
    private static final String VDB_NAME = "MyVdb";

    private RestVdb vdb;

    @Before
    public void init() {
        this.vdb = new RestVdb( VDB_NAME );
        this.vdb.setDescription( DESCRIPTION );
        this.vdb.setOriginalFilePath( ORIGINAL_FILE );
    }

    @Test
    public void shouldExportJson() {
        assertThat( JSON_BUILDER.toJson( this.vdb ), is( JSON ) );
    }

    @Test
    public void shouldImportJson() {
        final RestVdb descriptor = JSON_BUILDER.fromJson( JSON, RestVdb.class );
        assertThat( descriptor.getName(), is( VDB_NAME ) );
        assertThat( descriptor.getDescription(), is( DESCRIPTION ) );
        assertThat( descriptor.getOriginalFilePath(), is( ORIGINAL_FILE ) );
        assertThat( descriptor.getLinks().length, is( 0 ) );
        assertThat( descriptor.getProperties().isEmpty(), is( true ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotExportJsonWhenNameIsMissing() {
        final RestVdb descriptor = new RestVdb();
        JSON_BUILDER.toJson( descriptor );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenIdIsMissing() {
        final String malformed = "{\"description\":\"my description\",\"links\":[{\"rel\":\"self\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb\",\"method\":\"GET\"},{\"rel\":\"parent\",\"href\":\"http://localhost:8080/v1/workspace/vdbs\",\"method\":\"GET\"},{\"rel\":\"manifest\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb/manifest\",\"method\":\"GET\"}]}";
        JSON_BUILDER.fromJson( malformed, RestVdb.class );
    }

}
