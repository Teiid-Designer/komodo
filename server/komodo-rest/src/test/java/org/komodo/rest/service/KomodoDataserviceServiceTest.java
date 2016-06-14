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
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;

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
        this.response = request(uri).get();
        assertTrue(response.hasEntity());

        final String entities = response.readEntity(String.class);
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
    public void shouldReturnEmptyListWhenNoDataservicesInWorkspace() {
        this.response = request(_uriBuilder.workspaceDataservicesUri()).get();
        final String entity = this.response.readEntity(String.class);
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
        this.response = request(_uriBuilder.dataserviceUri(LinkType.SELF, settings)).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestDataservice dataservice = KomodoJsonMarshaller.unmarshall(entity, RestDataservice.class);
        assertNotNull(dataservice);
        
        assertEquals(dataservice.getId(), DATASERVICE_NAME);
    }
   
}
