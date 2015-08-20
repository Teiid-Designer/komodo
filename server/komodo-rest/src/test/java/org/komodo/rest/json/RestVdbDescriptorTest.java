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
public class RestVdbDescriptorTest {

    private URI baseUri;
    private final Gson gson = new Gson();

    @Before
    public void constructBaseUri() throws Exception {
        this.baseUri = new URI( "http://localhost:8080/komodo-rest" );
    }

    @Test
    public void shouldHaveCorrectJson() {
        final String vdbName = "MyVdb";
        final String description = "my description";

        final RestVdbDescriptor descriptor = new RestVdbDescriptor( vdbName, this.baseUri );
        descriptor.setDescription( description );

        // {"id":"MyVdb","description":"my description","links":[{"rel":"self","href":"http://localhost:8080/komodo/v1/workspace/komodo/workspace/vdbs/vdbName"},{"rel":"parent","href":"http://localhost:8080/komodo-rest/komodo/v1/workspace/vdbs/"},{"rel":"content","href":"http://localhost:8080/komodo-rest/komodo/v1/workspace/vdbs/vdbName.xml"}]}
        assertThat( this.gson.toJson( descriptor ),
                    is( "{\"id\":\"MyVdb\",\"description\":\"my description\",\"links\":[{\"rel\":\"self\",\"href\":\"http://localhost:8080/komodo-rest/komodo/v1/workspace/vdbs/MyVdb\"},{\"rel\":\"parent\",\"href\":\"http://localhost:8080/komodo-rest/komodo/v1/workspace/vdbs\"},{\"rel\":\"content\",\"href\":\"http://localhost:8080/komodo-rest/komodo/v1/workspace/vdbs/MyVdb.xml\"}]}" ) );
    }

}
