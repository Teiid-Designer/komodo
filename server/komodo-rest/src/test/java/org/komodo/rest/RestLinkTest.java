/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
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
