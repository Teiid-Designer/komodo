/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.json;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.junit.Test;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class LinkSerializerTest {

    private static final String JSON = "{\"rel\":\"self\",\"href\":\"http://localhost:8080\"}";
    private static final LinkType LINK_TYPE = LinkType.SELF;
    private static final URI URI = UriBuilder.fromUri( "http://localhost:8080" ).build();
    private static final LinkSerializer BUILDER = new LinkSerializer();

    @Test
    public void shouldExportJson() throws Exception {
        final RestLink link = new RestLink( LINK_TYPE, URI);
        String json = BUILDER.toJson( link );
        assertEquals(JSON, json);
    }

    @Test
    public void shouldImportJson() throws Exception {
        final RestLink link = BUILDER.fromJson( JSON );
        assertThat( link.getRel(), is( LINK_TYPE ) );
        assertThat( link.getHref(), is( URI ) );
    }

}
