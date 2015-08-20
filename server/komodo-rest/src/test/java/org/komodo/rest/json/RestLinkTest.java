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
import javax.ws.rs.core.UriBuilder;
import org.junit.Test;
import com.google.gson.Gson;

@SuppressWarnings( { "javadoc", "nls" } )
public class RestLinkTest {

    private final Gson gson = new Gson();

    @Test
    public void shouldHaveCorrectJson() {
        final RestLink link = new RestLink( RestLink.LinkType.SELF, UriBuilder.fromUri( "http://localhost:8080" ).build() );

        // {"rel":"self","href":"http://org.komodo/link"}
        assertThat( this.gson.toJson( link ), is( "{\"rel\":\"self\",\"href\":\"http://localhost:8080\"}" ) );
    }

}
