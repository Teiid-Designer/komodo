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
import org.junit.Test;
import org.komodo.rest.json.RestLink;
import org.komodo.rest.json.RestLink.LinkType;
import org.komodo.rest.json.RestLink.MethodType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class LinkSerializerTest {

    private static final String JSON = "{\"rel\":\"self\",\"href\":\"http://localhost:8080\",\"method\":\"GET\"}";
    private static final LinkType LINK_TYPE = LinkType.SELF;
    private static final MethodType METHOD_TYPE = MethodType.GET;
    private static final URI URI = UriBuilder.fromUri( "http://localhost:8080" ).build();

    @Test
    public void shouldExportJson() {
        final RestLink link = new RestLink( LINK_TYPE, URI, METHOD_TYPE );
        assertThat( KomodoJsonMarshaller.BUILDER.toJson( link ), is( JSON ) );
    }

    @Test
    public void shouldImportJson() {
        final RestLink link = KomodoJsonMarshaller.BUILDER.fromJson( JSON, RestLink.class );
        assertThat( link.getRel(), is( LINK_TYPE ) );
        assertThat( link.getHref(), is( URI ) );
        assertThat( link.getMethod(), is( METHOD_TYPE ) );
    }

}
