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
