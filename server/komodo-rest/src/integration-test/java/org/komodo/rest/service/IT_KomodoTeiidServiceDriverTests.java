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
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.InputStream;
import java.net.URI;
import java.util.Base64;
import java.util.Map;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoFileAttributes;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.spi.constants.StringConstants;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;
import net.jcip.annotations.NotThreadSafe;

@NotThreadSafe
@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls", "deprecation"} )
public final class IT_KomodoTeiidServiceDriverTests extends AbstractKomodoTeiidServiceTest implements StringConstants {

    @Override
    protected int getTestTotalInClass() {
        return 2;
    }

    @Test
    public void shouldDeployDriver() throws Exception {
        undeployDrivers();
        assertNoMysqlDriver();

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.TEIID_SEGMENT)
                                            .path(V1Constants.TEIID_DRIVER)
                                            .build();

        KomodoFileAttributes fileAttr = new KomodoFileAttributes();
        fileAttr.setName(MYSQL_DRIVER);

        InputStream driverStream = TestUtilities.mySqlDriver();
        assertNotNull(driverStream);

        byte[] driverBytes = TestUtilities.streamToBytes(driverStream);
        String content = Base64.getEncoder().encodeToString(driverBytes);
        fileAttr.setContent(content);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.body(MediaType.APPLICATION_JSON_TYPE, fileAttr);
        ClientResponse<String> response = request.post(String.class);
        final String entity = response.getEntity();

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
        assertEquals(title, status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        assertFalse(attributes.isEmpty());

        String deployMsg = RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_DEPLOYED);
        boolean foundValue = false;
        for (String value : attributes.values()) {
            if (deployMsg.equals(value)) {
               foundValue = true;
               break;
            }
        }

        assertTrue(foundValue);

        assertMysqlDriver();
    }

    @Test
    public void shouldUndeployDriver() throws Exception {
        InputStream driverStream = TestUtilities.mySqlDriver();
        assertNotNull(driverStream);
        byte[] driverBytes = TestUtilities.streamToBytes(driverStream);
        File driverFile = File.createTempFile(MYSQL_DRIVER, DOT + JAR);
        FileUtils.write(driverBytes, driverFile);

        helperInstance.deployDriver(MYSQL_DRIVER, driverFile);
        assertMysqlDriver();

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.TEIID_SEGMENT)
                                            .path(V1Constants.TEIID_DRIVER)
                                            .path(MYSQL_DRIVER)
                                            .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.delete(String.class);
        final String entity = response.getEntity();

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        wait(4);

        helperInstance.reconnect();

        String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
        assertEquals(title, status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        assertEquals(1, attributes.size());

        String deployMsg = RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_UNDEPLOYED);
        assertEquals(deployMsg, attributes.values().iterator().next());

        assertNoMysqlDriver();
    }
}
