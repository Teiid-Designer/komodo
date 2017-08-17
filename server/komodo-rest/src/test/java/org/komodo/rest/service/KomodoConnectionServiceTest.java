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
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;
import javax.ws.rs.core.MediaType;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoConnectionAttributes;

@SuppressWarnings( {"javadoc", "nls"} )
public final class KomodoConnectionServiceTest extends AbstractKomodoServiceTest {

    public static final String CONNECTION_NAME = "MyConnection"; 
    
    @Rule
    public TestName testName = new TestName();

    @Test
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
    public void shouldGetConnection() throws Exception {
        createConnection(CONNECTION_NAME);

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.CONNECTION_NAME, CONNECTION_NAME);
        _uriBuilder.addSetting(settings, SettingNames.PARENT_PATH, _uriBuilder.workspaceConnectionsUri());

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

    @Test
    public void shouldCreateConnection() throws Exception {
        // post
        Properties settings = _uriBuilder.createSettings(SettingNames.CONNECTION_NAME, CONNECTION_NAME);
        _uriBuilder.addSetting(settings, SettingNames.PARENT_PATH, _uriBuilder.workspaceConnectionsUri());

        URI uri = _uriBuilder.connectionUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);

        KomodoConnectionAttributes rcAttr = new KomodoConnectionAttributes();
        rcAttr.setJndi("jndi:/MySqlDS1");
        rcAttr.setDriver("mysql");
        rcAttr.setJdbc(true);
        rcAttr.setParameter("username", "test");
        rcAttr.setParameter("password", "myPassword");

        addBody(request, rcAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));
        System.out.println("Response:\n" + entity);

        RestConnection rsObj = KomodoJsonMarshaller.unmarshall(entity, RestConnection.class);
        assertEquals(rcAttr.getDriver(), rsObj.getDriverName());
        assertEquals(rcAttr.getJndi(), rsObj.getJndiName());
        assertEquals(rcAttr.isJdbc(), rsObj.isJdbc());

        List<RestProperty> rsProps = rsObj.getProperties();
        for (Entry<String, Object> parameter : rcAttr.getParameters().entrySet()) {
            RestProperty rsProp = null;

            for (RestProperty rsp : rsProps) {
                if (! rsp.getName().equals(parameter.getKey()))
                    continue;

                rsProp = rsp;
            }

            assertNotNull(parameter.getKey() + " property not handled", rsProp);
            assertEquals(parameter.getValue(), rsProp.getValue());
        }
    }

    @Test
    public void shouldUpdateConnection() throws Exception {
        createConnection(CONNECTION_NAME);

        // put
        Properties settings = _uriBuilder.createSettings(SettingNames.CONNECTION_NAME, CONNECTION_NAME);
        _uriBuilder.addSetting(settings, SettingNames.PARENT_PATH, _uriBuilder.workspaceConnectionsUri());

        URI uri = _uriBuilder.connectionUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);

        KomodoConnectionAttributes rcAttr = new KomodoConnectionAttributes();
        rcAttr.setJndi("jndi:/MySqlDS1");
        rcAttr.setDriver("mysql");
        rcAttr.setJdbc(true);
        rcAttr.setParameter("username", "test");
        rcAttr.setParameter("password", "myPassword");

        addBody(request, rcAttr);
        ClientResponse<String> response = request.put(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));
        System.out.println("Response:\n" + entity);

        RestConnection rsObj = KomodoJsonMarshaller.unmarshall(entity, RestConnection.class);
        assertEquals(rcAttr.getDriver(), rsObj.getDriverName());
        assertEquals(rcAttr.getJndi(), rsObj.getJndiName());
        assertEquals(rcAttr.isJdbc(), rsObj.isJdbc());

        List<RestProperty> rsProps = rsObj.getProperties();
        for (Entry<String, Object> parameter : rcAttr.getParameters().entrySet()) {
            RestProperty rsProp = null;

            for (RestProperty rsp : rsProps) {
                if (! rsp.getName().equals(parameter.getKey()))
                    continue;

                rsProp = rsp;
            }

            assertNotNull(parameter.getKey() + " property not handled", rsProp);
            assertEquals(parameter.getValue(), rsProp.getValue());
        }
    }
}
