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
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.json.RestLink.LinkType;
import org.komodo.rest.json.RestVdbDescriptor;
import org.komodo.rest.json.RestVdbDirectory;

@SuppressWarnings( { "javadoc", "nls" } )
public final class VdbDescriptorSerializerTest {

    private static final String DESCRIPTION = "my description";
    private static final String JSON = "{\"id\":\"MyVdb\",\"description\":\"my description\",\"links\":[{\"rel\":\"self\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb\",\"method\":\"GET\"},{\"rel\":\"parent\",\"href\":\"http://localhost:8080/v1/workspace/vdbs\",\"method\":\"GET\"},{\"rel\":\"manifest\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb/manifest\",\"method\":\"GET\"}]}";
    private static final LinkType[] LINK_TYPES = new LinkType[] { LinkType.SELF, LinkType.PARENT, LinkType.MANIFEST };
    private static final URI URI = UriBuilder.fromUri( "http://localhost:8080/v1" ).build();
    private static final String VDB_NAME = "MyVdb";

    private RestVdbDescriptor descriptor;

    @Before
    public void init() {
        this.descriptor = new RestVdbDescriptor( VDB_NAME, URI, LINK_TYPES );
        this.descriptor.setDescription( DESCRIPTION );
    }

    @Test
    public void shouldExportEmptyJsonDocumentWithEmptyDescriptors() {
        final RestVdbDirectory vdbDir = new RestVdbDirectory( new RestVdbDescriptor[ 0 ] );
        assertThat( KomodoJsonMarshaller.marshall( vdbDir ), is( "{\"vdbs\":[]}" ) );
    }

    @Test
    public void shouldExportEmptyJsonDocumentWithNullDescriptors() {
        final RestVdbDirectory vdbDir = new RestVdbDirectory( ( RestVdbDescriptor[] )null );
        assertThat( KomodoJsonMarshaller.marshall( vdbDir ), is( "{\"vdbs\":[]}" ) );
    }

    @Test
    public void shouldExportJson() {
        assertThat( KomodoJsonMarshaller.marshall( this.descriptor ), is( JSON ) );
    }

    @Test
    public void shouldImportJson() {
        final RestVdbDescriptor descriptor = KomodoJsonMarshaller.unmarshall( JSON, RestVdbDescriptor.class );
        assertThat( descriptor.getName(), is( VDB_NAME ) );
        assertThat( descriptor.getDescription(), is( DESCRIPTION ) );
        assertThat( descriptor.getLinks().length, is( LINK_TYPES.length ) );
        assertThat( descriptor.getProperties().isEmpty(), is( true ) );
    }

    @Test( expected = Exception.class )
    public void shouldNotExportWhenNameIsMissing() {
        final RestVdbDescriptor incomplete = new RestVdbDescriptor();
        KomodoJsonMarshaller.marshall( incomplete );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportWhenIdIsMissing() {
        final String malformed = "{\"description\":\"my description\",\"links\":[{\"rel\":\"self\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb\",\"method\":\"GET\"},{\"rel\":\"parent\",\"href\":\"http://localhost:8080/v1/workspace/vdbs\",\"method\":\"GET\"},{\"rel\":\"manifest\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb/manifest\",\"method\":\"GET\"}]}";
        KomodoJsonMarshaller.unmarshall( malformed, RestVdbDescriptor.class );
    }

}
