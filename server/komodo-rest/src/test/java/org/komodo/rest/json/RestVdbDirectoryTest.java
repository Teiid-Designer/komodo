/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.net.URI;
import org.junit.Before;
import org.junit.Test;
import com.google.gson.Gson;

@SuppressWarnings( { "javadoc", "nls" } )
public class RestVdbDirectoryTest {

    private URI baseUri;
    private final Gson gson = new Gson();

    @Before
    public void constructBaseUri() throws Exception {
        this.baseUri = new URI( "http://localhost:8080/komodo-rest/" );
    }

    @Test
    public void shouldConstructEmptyJsonDocumentWithEmptyDescriptors() {
        final RestVdbDirectory vdbDir = new RestVdbDirectory( new RestVdbDescriptor[ 0 ] );
        assertThat( this.gson.toJson( vdbDir ), is( "{\"vdbs\":[]}" ) );
    }

    @Test
    public void shouldConstructEmptyJsonDocumentWithNullDescriptors() {
        final RestVdbDirectory vdbDir = new RestVdbDirectory( null );
        assertThat( this.gson.toJson( vdbDir ), is( "{\"vdbs\":[]}" ) );
    }

    @Test
    public void shouldHaveCorrectJson() {
        final RestVdbDescriptor vdb1 = new RestVdbDescriptor( "MyVdb", this.baseUri );
        vdb1.setDescription( "my description" );

        final RestVdbDescriptor vdb2 = new RestVdbDescriptor( "YourVdb", this.baseUri );
        vdb2.setDescription( "your description" );

        final RestVdbDescriptor[] descriptors = new RestVdbDescriptor[] { vdb1, vdb2 };
        final RestVdbDirectory vdbDir = new RestVdbDirectory( descriptors );

        // {"vdbs":[{"id":"MyVdb","description":"my description","links":[{"rel":"self","href":"http://localhost:8080/komodo-rest/vdbs/MyVdb"},{"rel":"parent","href":"http://localhost:8080/komodo-rest/vdbs/"},{"rel":"content","href":"http://localhost:8080/komodo-rest/vdbs/MyVdb.xml"}]},{"id":"YourVdb","description":"your description","links":[{"rel":"self","href":"http://localhost:8080/komodo-rest/vdbs/YourVdb"},{"rel":"parent","href":"http://localhost:8080/komodo-rest/vdbs/"},{"rel":"content","href":"http://localhost:8080/komodo-rest/vdbs/YourVdb.xml"}]}]}
        final String expected = "{\"vdbs\":[" + this.gson.toJson( vdb1 ) + ',' + this.gson.toJson( vdb2 ) + "]}";
        assertThat( this.gson.toJson( vdbDir ), is( expected ) );
    }

}
