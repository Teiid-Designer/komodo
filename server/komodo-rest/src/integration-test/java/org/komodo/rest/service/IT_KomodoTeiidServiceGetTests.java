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
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.relational.workspace.ServerManager;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.cors.CorsHeaders;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.connection.RestTemplate;
import org.komodo.rest.relational.connection.RestTemplateEntry;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoTeiidAttributes;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.RestTeiid;
import org.komodo.rest.relational.response.RestTeiidStatus;
import org.komodo.rest.relational.response.RestTeiidVdbStatus;
import org.komodo.rest.relational.response.RestTeiidVdbStatusVdb;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.ExecutionAdmin.ConnectivityType;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.test.utils.TestUtilities;
import net.jcip.annotations.NotThreadSafe;

@NotThreadSafe
@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls"} )
public final class IT_KomodoTeiidServiceGetTests extends AbstractKomodoTeiidServiceTest implements StringConstants {

    private void testTranslators(RestTeiidStatus status) {
        if (teiidVersion.isGreaterThan(Version.TEIID_9_0))
            assertEquals(56, status.getTranslatorSize());
        else
            assertEquals(54, status.getTranslatorSize());
    }

    @Override
    protected int getTestTotalInClass() {
        return 12;
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

    @Test
    public void shouldGetTeiidStatus() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        System.out.println("Response:\n" + entity);
        assertEquals(200, response.getStatus());

        RestTeiidStatus status = KomodoJsonMarshaller.unmarshall(entity, RestTeiidStatus.class);
        assertNotNull(status);

        assertEquals("DefaultServer", status.getId());
        assertEquals("localhost", status.getHost());
        assertEquals(teiidVersion, new DefaultTeiidVersion(status.getVersion()));
        assertTrue(status.isTeiidInstanceAvailable());
        assertTrue(status.isConnected());
        assertEquals(1, status.getDataSourceSize());
        assertEquals(3, status.getDataSourceDriverSize());

        testTranslators(status);

        assertEquals(1, status.getVdbSize());
    }

    @Test
    public void shouldGetTeiidVdbStatus() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .path(V1Constants.VDBS_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        System.out.println("Response:\n" + entity);
        assertEquals(200, response.getStatus());

        RestTeiidVdbStatus status = KomodoJsonMarshaller.unmarshall(entity, RestTeiidVdbStatus.class);
        assertNotNull(status);

        List<RestTeiidVdbStatusVdb> vdbProperties = status.getVdbProperties();
        assertEquals(1, vdbProperties.size());

        RestTeiidVdbStatusVdb vdb = vdbProperties.get(0);
        assertNotNull(vdb);
        
        assertEquals("sample", vdb.getName());
        assertEquals("sample-vdb.xml", vdb.getDeployedName());
        assertEquals("1", vdb.getVersion());
        assertTrue(vdb.isActive());
        assertFalse(vdb.isLoading());
        assertFalse(vdb.isFailed());
        assertEquals(0, vdb.getErrors().size());
    }

    @SuppressWarnings( "incomplete-switch" )
    @Test
    public void shouldGetVdbs() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.VDBS_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        System.out.println("Response:\n" + entity);
        assertEquals(200, response.getStatus());

        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entity, RestVdb[].class);
        assertFalse(vdbs.length == 0);

        RestVdb vdb = vdbs[0];
        String vdbName = TestUtilities.SAMPLE_VDB_NAME;
        assertNotNull(vdbName, vdb.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, vdb.getBaseUri().toString());
        assertEquals(CACHED_TEIID_DATA_PATH + FORWARD_SLASH + V1Constants.VDBS_SEGMENT + FORWARD_SLASH + "sample", vdb.getDataPath());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertTrue(vdb.hasChildren());
        assertEquals(vdbName, vdb.getName());

        for(RestLink link : vdb.getLinks()) {
            switch(link.getRel()) {
                case SELF:
                    assertEquals(BASE_URI + FORWARD_SLASH +
                                             V1Constants.TEIID_SEGMENT + FORWARD_SLASH +
                                             ServerManager.DEFAULT_SERVER_NAME + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT + FORWARD_SLASH +
                                             vdbName,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(BASE_URI + FORWARD_SLASH +
                                             V1Constants.TEIID_SEGMENT + FORWARD_SLASH +
                                             ServerManager.DEFAULT_SERVER_NAME + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT,
                                             link.getHref().toString());
            }
        }
    }

    @SuppressWarnings( "incomplete-switch" )
    @Test
    public void shouldGetVdb() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.VDBS_SEGMENT)
                                          .path(TestUtilities.SAMPLE_VDB_NAME)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        System.out.println("Response:\n" + entity);
        assertEquals(200, response.getStatus());

        RestVdb vdb = KomodoJsonMarshaller.unmarshall(entity, RestVdb.class);
        assertNotNull(vdb);

        String vdbName = TestUtilities.SAMPLE_VDB_NAME;
        assertNotNull(vdbName, vdb.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, vdb.getBaseUri().toString());
        assertEquals(CACHED_TEIID_DATA_PATH + FORWARD_SLASH + V1Constants.VDBS_SEGMENT + FORWARD_SLASH + "sample", vdb.getDataPath());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertTrue(vdb.hasChildren());
        assertEquals(vdbName, vdb.getName());

        for(RestLink link : vdb.getLinks()) {
            switch(link.getRel()) {
                case SELF:
                    assertEquals(BASE_URI + FORWARD_SLASH +
                                             V1Constants.TEIID_SEGMENT + FORWARD_SLASH +
                                             ServerManager.DEFAULT_SERVER_NAME + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT + FORWARD_SLASH +
                                             vdbName,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(BASE_URI + FORWARD_SLASH +
                                             V1Constants.TEIID_SEGMENT + FORWARD_SLASH +
                                             ServerManager.DEFAULT_SERVER_NAME + FORWARD_SLASH +
                                             V1Constants.VDBS_SEGMENT,
                                             link.getHref().toString());
            }
        }
    }

    @Test
    public void shouldGetTeiidStatusMultiQueries() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .build();

        int iterations = 3;
        final CountDownLatch latch = new CountDownLatch(iterations);
        final List<Throwable> assertionFailures = new ArrayList<Throwable>();

        for (int i = 0; i < iterations; ++i) {
            Runnable runnable = new Runnable() {

                @Override
                public void run() {
                    try {
                        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
                        ClientResponse<String> response = request.get(String.class);

                        Thread.yield();

                        String entity = response.getEntity();
                        System.out.println("Response:\n" + entity);
                        //
                        // Don't want the thread dying since the latch will never
                        // countdown and the test will be stuck for 3 minutes
                        // waiting to timeout.
                        // Better to add the assertion errors into a bucket and once
                        // the countdown has been completed, check the bucket for
                        // errors. Don't really care if there is more than one, just that
                        // there is one, the test should fail
                        //
                        assertEquals(200, response.getStatus());
                        RestTeiidStatus status = KomodoJsonMarshaller.unmarshall(entity, RestTeiidStatus.class);
                        assertNotNull(status);

                        assertEquals("DefaultServer", status.getId());
                        assertEquals("localhost", status.getHost());
                        assertEquals(teiidVersion, new DefaultTeiidVersion(status.getVersion()));
                        assertTrue(status.isTeiidInstanceAvailable());
                        assertTrue(status.isConnected());
                        assertEquals(1, status.getDataSourceSize());
                        assertEquals(3, status.getDataSourceDriverSize());
                        testTranslators(status);
                        assertEquals(1, status.getVdbSize());
                    } catch (Throwable ex) {
                        assertionFailures.add(ex);
                    } finally {
                        latch.countDown();
                    }
                }
            };

            Thread thread = new Thread(runnable);
            thread.start();
        }

        assertTrue(latch.await(3, TimeUnit.MINUTES));
        for (Throwable t : assertionFailures) {
            // Give a clue as to what failed
            t.printStackTrace();
        }
        assertTrue(assertionFailures.isEmpty());
    }

    @Test
    public void shouldGetTranslators() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.TRANSLATORS_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        System.out.println("Response:\n" + entity);
        assertEquals(200, response.getStatus());

        RestVdbTranslator[] translators = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbTranslator[].class);
        assertTrue(translators.length > 0);

        for (RestVdbTranslator translator : translators) {
            assertNotNull(translator.getId());
            assertEquals(3, translator.getLinks().size());
        }
    }

    @Test
    public void shouldGetConnections() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.CONNECTIONS_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        System.out.println("Response:\n" + entity);
        assertEquals(200, response.getStatus());

        RestConnection[] connections = KomodoJsonMarshaller.unmarshallArray(entity, RestConnection[].class);
        assertTrue(connections.length > 0);

        for (RestConnection connection : connections) {
            assertNotNull(connection.getId());
            assertEquals(3, connection.getLinks().size());
        }
    }

    @Test
    public void shouldGetConnectionTemplates() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.TEMPLATES_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        assertEquals(200, response.getStatus());

        RestTemplate[] templates = KomodoJsonMarshaller.unmarshallArray(entity, RestTemplate[].class);
        assertTrue(templates.length > 0);

        for (RestTemplate template : templates) {
            assertNotNull(template.getId());
            assertFalse(template.getEntries().isEmpty());
            assertEquals(4, template.getLinks().size());
        }
    }

    @Test
    public void shouldGetConnectionTemplate() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.TEMPLATES_SEGMENT)
                                          .path("webservice")
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        assertEquals(200, response.getStatus());

        RestTemplate template = KomodoJsonMarshaller.unmarshall(entity, RestTemplate.class);
        assertNotNull(template);

        assertNotNull(template.getId());
        assertFalse(template.getEntries().isEmpty());
        assertEquals(4, template.getLinks().size());
    }

    @Test
    public void shouldGetConnectionTemplateEntries() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.TEMPLATES_SEGMENT)
                                          .path("webservice")
                                          .path(V1Constants.TEMPLATE_ENTRIES_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        assertEquals(200, response.getStatus());

        RestTemplateEntry[] templateEntries = KomodoJsonMarshaller.unmarshallArray(entity, RestTemplateEntry[].class);
        assertTrue(templateEntries.length > 0);

        for (RestTemplateEntry entry : templateEntries) {
            assertNotNull(entry.getId());
            assertEquals(2, entry.getLinks().size());
        }
    }

    @Test
    public void shouldAdminPing() throws Exception {
        shouldPing(ConnectivityType.ADMIN);
    }

    @Test
    public void shouldJdbcPing() throws Exception {
        shouldPing(ConnectivityType.JDBC);
    }

    @Test
    public void shouldFailJdbcPing() throws Exception {
        setJdbcName("IamTheWrongJdbcUserName");

        try {
            ClientResponse<String> response = ping(ConnectivityType.JDBC);
            String entity = response.getEntity();
            System.out.println("Entity: " + entity);
            KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
            assertNotNull(status);

            Map<String, String> attributes = status.getAttributes();
            assertEquals("false", attributes.get("OK"));
            assertTrue(attributes.get("Message").contains("Unable to establish a jdbc connection to teiid instance"));
            assertTrue(attributes.get("Exception").contains("The username \"IamTheWrongJdbcUserName\" and/or password and/or payload token could not be authenticated by security domain teiid-security"));
        } finally {
            setJdbcName(TeiidJdbcInfo.DEFAULT_JDBC_USERNAME);
        }
    }

    @Test
    public void shouldAbout() throws Exception {
        String[] EXPECTED = {
                "\"Information\": " +  OPEN_BRACE + NEW_LINE,
                "\"Repository Workspace\": \"komodoLocalWorkspace\"," + NEW_LINE,
                "\"Repository Configuration\"", // Configuration Url contains local file names so impossible to test
                "\"Repository Vdb Total\":",
            };

        // get
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.ABOUT).build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addHeader(request, CorsHeaders.ORIGIN, "http://localhost:2772");

        ClientResponse<String> response = request.get(String.class);
        assertNotNull(response.getEntity());
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        final String entity = response.getEntity();
        System.out.println("Response from uri " + uri + ":\n" + entity);
        for (String expected : EXPECTED) {
            assertTrue(expected + " is not contained in " + entity, entity.contains(expected));
        }
    }
}
