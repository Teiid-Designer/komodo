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

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.net.URI;
import java.util.Properties;
import javax.ws.rs.core.MediaType;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;

@SuppressWarnings( {"javadoc", "nls"} )
public final class KomodoConnectionServiceTest extends AbstractKomodoServiceTest {

    public static final String CONNECTION_NAME = "MyConnection"; 
    
    @Rule
    public TestName testName = new TestName();

    @Test
    @Ignore
    public void shouldGetConnections() throws Exception {
        createConnection(CONNECTION_NAME);

        // get
        URI uri = _uriBuilder.workspaceConnectionsUri();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        assertNotNull(response.getEntity());

        final String entities = response.getEntity();
        assertThat(entities, is(notNullValue()));

        // System.out.println("Response:\n" + entities);
        // make sure the Dataservice JSON document is returned for each dataservice
        RestConnection[] connection = KomodoJsonMarshaller.unmarshallArray(entities, RestConnection[].class);

        assertEquals(1, connection.length);
        RestConnection mySource = connection[0];
        assertNotNull(mySource.getId());
        assertTrue(CONNECTION_NAME.equals(mySource.getId()));
        assertNotNull(mySource.getDataPath());
        assertNotNull(mySource.getkType());
    }
    
    @Test
    public void shouldReturnEmptyListWhenNoDataservicesInWorkspace() throws Exception {
        URI uri = _uriBuilder.workspaceDataservicesUri();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestConnection[] connections = KomodoJsonMarshaller.unmarshallArray(entity, RestConnection[].class);
        assertNotNull(connections);
        assertEquals(0, connections.length);
    }
    
    @Test
    @Ignore
    public void shouldGetConnection() throws Exception {
        createConnection(CONNECTION_NAME);

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.CONNECTION_NAME, CONNECTION_NAME);
        _uriBuilder.addSetting(settings, SettingNames.CONNECTION_PARENT_PATH, _uriBuilder.workspaceConnectionsUri());

        URI uri = _uriBuilder.connectionUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        System.out.println("Response:\n" + entity);

        RestConnection connection = KomodoJsonMarshaller.unmarshall(entity, RestConnection.class);
        assertNotNull(connection);
        
        assertEquals(connection.getId(), CONNECTION_NAME);
    }

}
