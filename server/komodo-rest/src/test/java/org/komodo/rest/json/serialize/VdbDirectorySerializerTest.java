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
public final class VdbDirectorySerializerTest {

    private static final RestVdbDescriptor[] DESCRIPTORS;
    private static final String JSON;
    private static final RestVdbDescriptor MY_VDB;
    private static final RestVdbDescriptor YOUR_VDB;

    static {
        final URI uri = UriBuilder.fromUri( "http://localhost:8080" ).build();
        MY_VDB = new RestVdbDescriptor( "MyVdb", uri, LinkType.SELF, LinkType.PARENT, LinkType.MANIFEST );
        MY_VDB.setDescription( "my description" );

        YOUR_VDB = new RestVdbDescriptor( "YourVdb", uri, LinkType.SELF, LinkType.PARENT, LinkType.MANIFEST, LinkType.DELETE );
        YOUR_VDB.setDescription( "your description" );

        DESCRIPTORS = new RestVdbDescriptor[] { MY_VDB, YOUR_VDB };
        JSON = "{\"vdbs\":[" + KomodoJsonMarshaller.marshall( MY_VDB ) + ',' + KomodoJsonMarshaller.marshall( YOUR_VDB ) + "]}";
    }

    private RestVdbDirectory vdbDir;

    @Before
    public void init() {
        this.vdbDir = new RestVdbDirectory( DESCRIPTORS );
    }

    @Test
    public void shouldExportJson() {
        assertThat( KomodoJsonMarshaller.marshall( this.vdbDir ), is( JSON ) );
    }

    @Test
    public void shouldImportJson() {
        final RestVdbDirectory vdbDir = KomodoJsonMarshaller.unmarshall( JSON, RestVdbDirectory.class );
        assertThat( vdbDir.getDescriptors().length, is( DESCRIPTORS.length ) );
        assertThat( vdbDir.getDescriptors()[ 0 ], is( MY_VDB ) );
        assertThat( vdbDir.getDescriptors()[ 1 ], is( YOUR_VDB ) );
        assertThat( vdbDir.getLinks().length, is( 0 ) );
        assertThat( vdbDir.getProperties().isEmpty(), is( true ) );
    }

}
