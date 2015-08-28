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
import static org.komodo.rest.json.JsonConstants.JSON_BUILDER;
import java.net.URI;
import java.util.Arrays;
import javax.ws.rs.core.UriBuilder;
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.json.RestLink.LinkType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbDirectoryTest {

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
        JSON = "{\"vdbs\":[" + JSON_BUILDER.toJson( MY_VDB ) + ',' + JSON_BUILDER.toJson( YOUR_VDB ) + "]}";
    }

    private RestVdbDirectory vdbDir;

    @Before
    public void init() {
        this.vdbDir = new RestVdbDirectory( DESCRIPTORS );
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbDirectory vdbDir2 = new RestVdbDirectory( this.vdbDir.getDescriptors() );
        assertThat( this.vdbDir, is( vdbDir2 ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyDirectories() {
        final RestVdbDirectory empty1 = new RestVdbDirectory();
        final RestVdbDirectory empty2 = new RestVdbDirectory();
        assertThat( empty1, is( empty2 ) );
    }

    @Test
    public void shouldConstructEmptyJsonDocumentWithEmptyDescriptors() {
        final RestVdbDirectory vdbDir = new RestVdbDirectory( new RestVdbDescriptor[ 0 ] );
        assertThat( JSON_BUILDER.toJson( vdbDir ), is( "{\"vdbs\":[]}" ) );
    }

    @Test
    public void shouldConstructEmptyJsonDocumentWithNullDescriptors() {
        final RestVdbDirectory vdbDir = new RestVdbDirectory( ( RestVdbDescriptor[] )null );
        assertThat( JSON_BUILDER.toJson( vdbDir ), is( "{\"vdbs\":[]}" ) );
    }

    @Test
    public void shouldExportJson() {
        assertThat( JSON_BUILDER.toJson( this.vdbDir ), is( JSON ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbDirectory vdbDir2 = new RestVdbDirectory( this.vdbDir.getDescriptors() );
        assertThat( this.vdbDir.hashCode(), is( vdbDir2.hashCode() ) );
    }

    @Test
    public void shouldImportJson() {
        final RestVdbDirectory vdbDir = JSON_BUILDER.fromJson( JSON, RestVdbDirectory.class );
        assertThat( vdbDir.getDescriptors().length, is( DESCRIPTORS.length ) );
        assertThat( vdbDir.getDescriptors()[ 0 ], is( MY_VDB ) );
        assertThat( vdbDir.getDescriptors()[ 1 ], is( YOUR_VDB ) );
        assertThat( vdbDir.getLinks().length, is( 0 ) );
        assertThat( vdbDir.getProperties().isEmpty(), is( true ) );
    }

    @Test
    public void shouldNotBeEqualWhenDescriptorsAreDifferent() {
        final RestVdbDirectory vdbDir2 = new RestVdbDirectory( MY_VDB );
        assertThat( Arrays.deepEquals( this.vdbDir.getDescriptors(), vdbDir2.getDescriptors() ), is( false ) );
        assertThat( this.vdbDir, is( not( vdbDir2 ) ) );
    }

    @Test
    public void shouldSetVdbs() {
        final URI uri = UriBuilder.fromUri( "http://com.elvis:8080" ).build();
        final RestVdbDescriptor descriptor = new RestVdbDescriptor( "Different", uri, LinkType.SELF, LinkType.PARENT );
        descriptor.setDescription( "my different description" );

        final RestVdbDescriptor[] different = new RestVdbDescriptor[] { descriptor };
        this.vdbDir.setVdbs( different );

        assertThat( this.vdbDir.getDescriptors().length, is( different.length ) );
        assertThat( this.vdbDir.getDescriptors()[ 0 ], is( descriptor ) );
    }

}
