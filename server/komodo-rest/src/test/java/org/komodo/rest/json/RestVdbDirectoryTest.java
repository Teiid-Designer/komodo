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
import org.junit.Before;
import org.junit.Test;
import org.komodo.rest.json.RestLink.LinkType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestVdbDirectoryTest {

    private static final RestVdbDescriptor[] DESCRIPTORS;
    private static final RestVdbDescriptor MY_VDB;
    private static final RestVdbDescriptor YOUR_VDB;

    static {
        final URI uri = UriBuilder.fromUri( "http://localhost:8080" ).build();
        MY_VDB = new RestVdbDescriptor( "MyVdb", uri, LinkType.SELF, LinkType.PARENT, LinkType.MANIFEST );
        MY_VDB.setDescription( "my description" );

        YOUR_VDB = new RestVdbDescriptor( "YourVdb", uri, LinkType.SELF, LinkType.PARENT, LinkType.MANIFEST, LinkType.DELETE );
        YOUR_VDB.setDescription( "your description" );

        DESCRIPTORS = new RestVdbDescriptor[] { MY_VDB, YOUR_VDB };
    }

    private RestVdbDirectory vdbDir;

    @Before
    public void init() {
        this.vdbDir = new RestVdbDirectory( DESCRIPTORS );
    }

    @Test
    public void shouldBeEqual() {
        final RestVdbDirectory thatVdbDir = new RestVdbDirectory( this.vdbDir.getDescriptors() );
        thatVdbDir.setLinks( this.vdbDir.getLinks() );
        thatVdbDir.setProperties( this.vdbDir.getProperties() );
        assertThat( this.vdbDir, is( thatVdbDir ) );
    }

    @Test
    public void shouldBeEqualWhenComparingEmptyDirectories() {
        final RestVdbDirectory empty1 = new RestVdbDirectory();
        final RestVdbDirectory empty2 = new RestVdbDirectory();
        assertThat( empty1, is( empty2 ) );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestVdbDirectory thatVdbDir = new RestVdbDirectory( this.vdbDir.getDescriptors() );
        thatVdbDir.setLinks( this.vdbDir.getLinks() );
        thatVdbDir.setProperties( this.vdbDir.getProperties() );

        assertThat( this.vdbDir.hashCode(), is( thatVdbDir.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenDescriptorsAreDifferent() {
        final RestVdbDirectory thatVdbDir = new RestVdbDirectory( MY_VDB );
        thatVdbDir.setLinks( this.vdbDir.getLinks() );
        thatVdbDir.setProperties( this.vdbDir.getProperties() );

        assertThat( Arrays.deepEquals( this.vdbDir.getDescriptors(), thatVdbDir.getDescriptors() ), is( false ) );
        assertThat( this.vdbDir, is( not( thatVdbDir ) ) );
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
