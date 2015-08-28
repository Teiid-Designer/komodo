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
import javax.ws.rs.core.UriBuilder;
import org.junit.Test;
import org.komodo.rest.json.RestLink.LinkType;
import org.komodo.rest.json.RestLink.MethodType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestLinkTest {

    private static final String JSON = "{\"rel\":\"self\",\"href\":\"http://localhost:8080\",\"method\":\"GET\"}";
    private static final LinkType LINK_TYPE = LinkType.SELF;
    private static final LinkType LINK_TYPE_2 = LinkType.DELETE;
    private static final MethodType METHOD_TYPE = MethodType.GET;
    private static final MethodType METHOD_TYPE_2 = MethodType.DELETE;
    private static final URI URI = UriBuilder.fromUri( "http://localhost:8080" ).build();

    @Test
    public void shouldBeEqual() {
        final RestLink link1 = new RestLink( LINK_TYPE, URI );
        final RestLink link2 = new RestLink( link1.getRel(), link1.getHref(), link1.getMethod() );
        assertThat( link1, is( link2 ) );
    }

    @Test
    public void shouldExportJson() {
        final RestLink link = new RestLink( LINK_TYPE, URI, METHOD_TYPE );
        assertThat( JSON_BUILDER.toJson( link ), is( JSON ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenLinkTypeIsNull() {
        new RestLink( null, URI );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenLinkTypeIsNull2() {
        new RestLink( null, URI, METHOD_TYPE );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenMethodTypeIsNull() {
        new RestLink( LINK_TYPE, URI, null );
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestLink link1 = new RestLink( LINK_TYPE, URI, METHOD_TYPE );
        final RestLink link2 = new RestLink( link1.getRel(), link1.getHref(), link1.getMethod() );
        assertThat( link1.hashCode(), is( link2.hashCode() ) );
    }

    @Test
    public void shouldImportJson() {
        final RestLink link = JSON_BUILDER.fromJson( JSON, RestLink.class );
        assertThat( link.getRel(), is( LINK_TYPE ) );
        assertThat( link.getHref(), is( URI ) );
        assertThat( link.getMethod(), is( METHOD_TYPE ) );
    }

    @Test
    public void shouldNotBeEqualWhenHrefDifferent() {
        final RestLink link1 = new RestLink( LINK_TYPE, URI, METHOD_TYPE );
        final RestLink link2 = new RestLink( link1.getRel(),
                                             UriBuilder.fromUri( "http://org.komodo:1234" ).build(),
                                             link1.getMethod() );
        assertThat( link1.getHref(), is( not( link2.getHref() ) ) );
        assertThat( link1, is( not( link2 ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenMethodDifferent() {
        final RestLink link1 = new RestLink( LINK_TYPE, URI, METHOD_TYPE );
        final RestLink link2 = new RestLink( link1.getRel(), link1.getHref(), METHOD_TYPE_2 );
        assertThat( link1.getMethod(), is( not( link2.getMethod() ) ) );
        assertThat( link1, is( not( link2 ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenRelDifferent() {
        final RestLink link1 = new RestLink( LINK_TYPE, URI, METHOD_TYPE );
        final RestLink link2 = new RestLink( LINK_TYPE_2, link1.getHref(), link1.getMethod() );
        assertThat( link1.getRel(), is( not( link2.getRel() ) ) );
        assertThat( link1, is( not( link2 ) ) );
    }

}
