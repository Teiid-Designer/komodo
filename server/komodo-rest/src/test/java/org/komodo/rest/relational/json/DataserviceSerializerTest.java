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
package org.komodo.rest.relational.json;

import static org.junit.Assert.assertEquals;
import org.jboss.resteasy.util.Encode;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.vdb.Vdb;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.PropertyDescriptor;
import org.mockito.Mockito;

@SuppressWarnings( { "javadoc", "nls" } )
public final class DataserviceSerializerTest extends AbstractSerializerTest  {

    private static final String DESCRIPTION = "my description";
    private static final KomodoType kType = KomodoType.DATASERVICE;

    private static final String JSON = OPEN_BRACE + NEW_LINE +
        "  \"" + BASE_URI + "\": \"" + MY_BASE_URI + "\"," + NEW_LINE +
        "  \"keng__id\": \"" + DATASERVICE_NAME + "\"," + NEW_LINE +
        "  \"keng__dataPath\": \"" + DATASERVICE_DATA_PATH + "\"," + NEW_LINE +
        "  \"keng__kType\": \"Dataservice\"," + NEW_LINE +
        "  \"keng__hasChildren\": true," + NEW_LINE +
        "  \"tko__description\": \"my description\"," + NEW_LINE +
        "  \"serviceVdbVersion\": \"1\"," + NEW_LINE +
        "  \"connections\": 0," + NEW_LINE +
        "  \"drivers\": 0," + NEW_LINE +
        "  \"keng___links\": [" + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"self\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + DATASERVICE_DATA_PATH + "\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"parent\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + "/workspace/dataservices\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"children\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + SEARCH + "parent\\u003d" + Encode.encodeQueryParam(DATASERVICE_DATA_PATH) + "\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"vdbs\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + DATASERVICE_DATA_PATH + "/Vdbs\"" + NEW_LINE +
        "    " + CLOSE_BRACE + COMMA + NEW_LINE +
        "    " + OPEN_BRACE + NEW_LINE +
        "      \"rel\": \"connections\"," + NEW_LINE +
        "      \"href\": \"" + BASE_URI_PREFIX + DATASERVICE_DATA_PATH + "/connections\"" + NEW_LINE +
        "    " + CLOSE_BRACE + NEW_LINE +
        "  " + CLOSE_SQUARE_BRACKET + NEW_LINE +
        CLOSE_BRACE;


    private RestDataservice dataservice;

    @Before
    public void init() throws Exception {
        KomodoObject workspace = Mockito.mock(KomodoObject.class);
        Mockito.when(workspace.getAbsolutePath()).thenReturn(WORKSPACE_DATA_PATH);

        Vdb serviceVdb = Mockito.mock(Vdb.class);
        Mockito.when(serviceVdb.getName(transaction)).thenReturn("ServiceVdb");
        Mockito.when(serviceVdb.getVersion(transaction)).thenReturn(1);

        Dataservice theService = mockObject(Dataservice.class, DATASERVICE_NAME, DATASERVICE_DATA_PATH, kType, true);
        Mockito.when(theService.getPropertyNames(transaction)).thenReturn(new String[0]);
        Mockito.when(theService.getPropertyDescriptors(transaction)).thenReturn(new PropertyDescriptor[0]);
        Mockito.when(theService.getParent(transaction)).thenReturn(workspace);
        Mockito.when(theService.getServiceVdb(transaction)).thenReturn(serviceVdb);

        this.dataservice = new RestDataservice(MY_BASE_URI, theService, false, transaction);
        this.dataservice.setDescription(DESCRIPTION);
    }

    @Test
    public void shouldExportJson() {
        String json = KomodoJsonMarshaller.marshall( this.dataservice );
        assertEquals(JSON, json);
    }

    @Test
    public void shouldImportJson() {
        final RestDataservice descriptor = KomodoJsonMarshaller.unmarshall( JSON, RestDataservice.class );
        assertEquals(DATASERVICE_NAME, descriptor.getId());
        assertEquals(DESCRIPTION, descriptor.getDescription());
        assertEquals(5, descriptor.getLinks().size());
        assertEquals(true, descriptor.getProperties().isEmpty());
    }

    @Test( expected = Exception.class )
    public void shouldNotExportJsonWhenNameIsMissing() {
        final RestDataservice descriptor = new RestDataservice();
        KomodoJsonMarshaller.marshall( descriptor );
    }

    @Test( expected = Exception.class )
    public void shouldNotImportJsonWhenIdIsMissing() {
        final String malformed = "{\"description\":\"my description\",\"links\":[{\"rel\":\"self\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb\",\"method\":\"GET\"},{\"rel\":\"parent\",\"href\":\"http://localhost:8080/v1/workspace/vdbs\",\"method\":\"GET\"},{\"rel\":\"manifest\",\"href\":\"http://localhost:8080/v1/workspace/vdbs/MyVdb/manifest\",\"method\":\"GET\"}]}";
        KomodoJsonMarshaller.unmarshall( malformed, RestDataservice.class );
    }

}
