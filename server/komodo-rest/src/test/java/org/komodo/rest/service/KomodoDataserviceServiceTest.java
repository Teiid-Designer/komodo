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
import java.util.Collection;
import java.util.Properties;
import javax.ws.rs.core.MediaType;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.datasource.RestDataSource;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.test.utils.TestUtilities;

@SuppressWarnings( {"javadoc", "nls"} )
public final class KomodoDataserviceServiceTest extends AbstractKomodoServiceTest {

    public static final String DATASERVICE_NAME = "MyDataService"; 
    
    @Rule
    public TestName testName = new TestName();

    @Test
    public void shouldGetDataservices() throws Exception {
        createDataservice(DATASERVICE_NAME);

        // get
        URI uri = _uriBuilder.workspaceDataservicesUri();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        assertNotNull(response.getEntity());

        final String entities = response.getEntity();
        assertThat(entities, is(notNullValue()));

        // System.out.println("Response:\n" + entities);
        // make sure the Dataservice JSON document is returned for each dataservice
        RestDataservice[] dataservices = KomodoJsonMarshaller.unmarshallArray(entities, RestDataservice[].class);

        assertEquals(1, dataservices.length);
        RestDataservice myService = dataservices[0];
        assertNotNull(myService.getId());
        assertTrue(DATASERVICE_NAME.equals(myService.getId()));
        assertNotNull(myService.getDataPath());
        assertNotNull(myService.getkType());
    }
    
    @Test
    public void shouldReturnEmptyListWhenNoDataservicesInWorkspace() throws Exception {
        URI uri = _uriBuilder.workspaceDataservicesUri();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestDataservice[] dataservices = KomodoJsonMarshaller.unmarshallArray(entity, RestDataservice[].class);
        assertNotNull(dataservices);
        assertEquals(0, dataservices.length);
    }
    
    @Test
    public void shouldGetDataservice() throws Exception {
        createDataservice(DATASERVICE_NAME);

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.DATA_SERVICE_NAME, DATASERVICE_NAME);
        _uriBuilder.addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, _uriBuilder.workspaceDataservicesUri());

        URI uri = _uriBuilder.dataserviceUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

//        System.out.println("Response:\n" + entity);

        RestDataservice dataservice = KomodoJsonMarshaller.unmarshall(entity, RestDataservice.class);
        assertNotNull(dataservice);
        
        assertEquals(dataservice.getId(), DATASERVICE_NAME);
    }

    @Test
    public void shouldGetDataserviceConnections() throws Exception {
        loadDataServices();

        // get
        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        Properties settings = _uriBuilder.createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        _uriBuilder.addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, _uriBuilder.workspaceDataservicesUri());

        URI uri = _uriBuilder.dataserviceUri(LinkType.CONNECTIONS, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        System.out.println("Response:\n" + entity);

        RestDataSource[] datasources = KomodoJsonMarshaller.unmarshallArray(entity, RestDataSource[].class);
        assertNotNull(datasources);
        assertEquals(1, datasources.length);

        RestDataSource dataSource = datasources[0];
        assertEquals("MySqlPool", dataSource.getId());
        assertEquals("java:/MySqlDS", dataSource.getJndiName());
        assertEquals("mysql-connector-java-5.1.39-bin.jarcom.mysql.jdbc.Driver_5_1", dataSource.getDriverName());

        Collection<RestLink> links = dataSource.getLinks();
        assertNotNull(links);
        assertEquals(3, links.size());

        for(RestLink link : links) {
            LinkType rel = link.getRel();
            assertTrue(LinkType.SELF.equals(rel) || LinkType.PARENT.equals(rel) || LinkType.CHILDREN.equals(rel));

            if (LinkType.SELF.equals(rel)) {
                String href = _uriBuilder.workspaceDatasourcesUri() + FORWARD_SLASH + dataSource.getId();
                assertEquals(href, link.getHref().toString());
            }
        }
    }
}
