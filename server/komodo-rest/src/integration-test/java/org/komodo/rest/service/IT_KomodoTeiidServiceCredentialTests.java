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
package org.komodo.rest.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import java.net.URI;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoTeiidAttributes;
import org.komodo.rest.relational.response.RestTeiid;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import net.jcip.annotations.NotThreadSafe;

@NotThreadSafe
@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls", "deprecation"} )
public final class IT_KomodoTeiidServiceCredentialTests extends AbstractKomodoTeiidServiceTest implements StringConstants {

    @Override
    protected int getTestTotalInClass() {
        return 1;
    }

    @Test
    public void shouldSetCredentials() throws Exception {
        // post
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.TEIID_SEGMENT)
                                            .path(V1Constants.TEIID_CREDENTIALS)
                                            .build();

        KomodoTeiidAttributes teiidAttrs = new KomodoTeiidAttributes();
        teiidAttrs.setAdminUser(TeiidAdminInfo.DEFAULT_ADMIN_USERNAME);
        teiidAttrs.setAdminPasswd(TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD);
        teiidAttrs.setJdbcUser(TeiidJdbcInfo.DEFAULT_JDBC_USERNAME);
        teiidAttrs.setJdbcPasswd(TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.body(MediaType.APPLICATION_JSON_TYPE, teiidAttrs);

        ClientResponse<String> response = request.post(String.class);
        String entity = response.getEntity();
        System.out.println("Response:\n" + entity);
        assertEquals(200, response.getStatus());

        RestTeiid rt = KomodoJsonMarshaller.unmarshall(entity, RestTeiid.class);
        assertNotNull(TeiidInstance.DEFAULT_HOST, rt.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, rt.getBaseUri().toString());
        assertEquals(TEIID_DATA_PATH, rt.getDataPath());
        assertEquals(KomodoType.TEIID, rt.getkType());
        assertFalse(rt.hasChildren());

        assertEquals(TeiidInstance.DEFAULT_HOST, rt.getHost());
        assertEquals(teiidVersion.toString(), rt.getVersion());

        assertEquals(TeiidAdminInfo.DEFAULT_ADMIN_USERNAME, rt.getAdminUser());
        assertEquals(TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD, rt.getAdminPasswd());
        assertEquals(TeiidAdminInfo.Util.defaultPort(teiidVersion), rt.getAdminPort());

        assertEquals(TeiidJdbcInfo.DEFAULT_JDBC_USERNAME, rt.getJdbcUser());
        assertEquals(TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD, rt.getJdbcPasswd());
        assertEquals(TeiidJdbcInfo.DEFAULT_PORT, rt.getJdbcPort());
    }
}
