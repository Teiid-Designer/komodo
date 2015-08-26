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
import static org.junit.Assert.assertThat;
import java.net.URI;
import java.util.Arrays;
import javax.ws.rs.core.UriBuilder;
import org.junit.Test;
import org.komodo.rest.json.RestLink.LinkType;
import com.google.gson.Gson;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbDirectoryTest {

    private static final Gson BUILDER = new Gson();
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
        JSON = "{\"vdbs\":[" + BUILDER.toJson( MY_VDB ) + ',' + BUILDER.toJson( YOUR_VDB ) + "]}";
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbDescriptor[] descriptors = new RestVdbDescriptor[] { MY_VDB, YOUR_VDB };
        final RestVdbDirectory vdbDir1 = new RestVdbDirectory( descriptors );
        final RestVdbDirectory vdbDir2 = new RestVdbDirectory( descriptors );
        assertThat( vdbDir1, is( vdbDir2 ) );
    }

    @Test
    public void shouldConstructEmptyJsonDocumentWithEmptyDescriptors() {
        final RestVdbDirectory vdbDir = new RestVdbDirectory( new RestVdbDescriptor[ 0 ] );
        assertThat( BUILDER.toJson( vdbDir ), is( "{\"vdbs\":[]}" ) );
    }

    @Test
    public void shouldConstructEmptyJsonDocumentWithNullDescriptors() {
        final RestVdbDirectory vdbDir = new RestVdbDirectory( ( RestVdbDescriptor[] )null );
        assertThat( BUILDER.toJson( vdbDir ), is( "{\"vdbs\":[]}" ) );
    }

    @Test
    public void shouldExportJson() {
        final RestVdbDescriptor[] descriptors = new RestVdbDescriptor[] { MY_VDB, YOUR_VDB };
        final RestVdbDirectory vdbDir = new RestVdbDirectory( descriptors );
        assertThat( BUILDER.toJson( vdbDir ), is( JSON ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbDescriptor[] descriptors = new RestVdbDescriptor[] { MY_VDB, YOUR_VDB };
        final RestVdbDirectory vdbDir1 = new RestVdbDirectory( descriptors );
        final RestVdbDirectory vdbDir2 = new RestVdbDirectory( descriptors );
        assertThat( vdbDir1.hashCode(), is( vdbDir2.hashCode() ) );
    }

    @Test
    public void shouldImportJson() {
        final RestVdbDirectory vdbDir = BUILDER.fromJson( JSON, RestVdbDirectory.class );
        assertThat( vdbDir.getDescriptors().length, is( DESCRIPTORS.length ) );
        assertThat( vdbDir.getDescriptors()[ 0 ], is( MY_VDB ) );
        assertThat( vdbDir.getDescriptors()[ 1 ], is( YOUR_VDB ) );
    }

    @Test
    public void shouldNotBeEqualWhenDescriptorsAreDifferent() {
        final RestVdbDescriptor[] descriptors = new RestVdbDescriptor[] { MY_VDB, YOUR_VDB };
        final RestVdbDirectory vdbDir1 = new RestVdbDirectory( descriptors );
        final RestVdbDirectory vdbDir2 = new RestVdbDirectory( MY_VDB );
        assertThat( Arrays.deepEquals( vdbDir1.getDescriptors(), vdbDir2.getDescriptors() ), is( false ) );
        assertThat( vdbDir1, is( not( vdbDir2 ) ) );
    }

}
