/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNot.not;
import static org.junit.Assert.assertThat;
import java.net.URI;
import javax.ws.rs.core.UriBuilder;
import org.junit.Test;
import org.komodo.rest.RestLink.LinkType;

@SuppressWarnings( { "javadoc", "nls" } )
public final class RestLinkTest {

    private static final LinkType LINK_TYPE = LinkType.SELF;

    private static final LinkType LINK_TYPE_2 = LinkType.PARENT;

    private static final URI URI = UriBuilder.fromUri( "http://localhost:8080" ).build();

    @Test
    public void shouldBeEqual() {
        final RestLink thisLink = new RestLink( LINK_TYPE, URI );
        final RestLink thatLink = new RestLink( thisLink.getRel(), thisLink.getHref());
        assertThat( thisLink, is( thatLink ) );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenLinkTypeIsNull() {
        new RestLink( null, URI );
    }

    @Test( expected = IllegalArgumentException.class )
    public void shouldFailWhenLinkTypeIsNull2() {
        new RestLink( null, URI);
    }

    @Test
    public void shouldHaveSameHashCode() {
        final RestLink thisLink = new RestLink( LINK_TYPE, URI);
        final RestLink thatLink = new RestLink( thisLink.getRel(), thisLink.getHref());
        assertThat( thisLink.hashCode(), is( thatLink.hashCode() ) );
    }

    @Test
    public void shouldNotBeEqualWhenHrefDifferent() {
        final RestLink thisLink = new RestLink( LINK_TYPE, URI);
        final RestLink thatLink = new RestLink( thisLink.getRel(),
                                                UriBuilder.fromUri( "http://org.komodo:1234" ).build());
        assertThat( thisLink.getHref(), is( not( thatLink.getHref() ) ) );
        assertThat( thisLink, is( not( thatLink ) ) );
    }

    @Test
    public void shouldNotBeEqualWhenRelDifferent() {
        final RestLink thisLink = new RestLink( LINK_TYPE, URI);
        final RestLink thatLink = new RestLink( LINK_TYPE_2, thisLink.getHref());
        assertThat( thisLink.getRel(), is( not( thatLink.getRel() ) ) );
        assertThat( thisLink, is( not( thatLink ) ) );
    }

}
