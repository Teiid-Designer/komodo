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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.io.File;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.resteasy.test.TestPortProvider;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.osgi.PluginService;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.KomodoTeiidAttributes;
import org.komodo.rest.relational.RestTeiid;
import org.komodo.rest.relational.RestVdb;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.test.utils.DummyEventManager;
import org.komodo.test.utils.TestUtilities;

@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls"} )
public final class IT_KomodoTeiidServiceTest implements StringConstants {

    private static final String TEST_PORT = "8080";

    private static Path _kengineDataDir;

    private static KomodoRestUriBuilder _uriBuilder;

    private Client client;

    private Response response;

    private TeiidService service;

    private static URI BASE_URI;

    private TeiidInstance helperInstance;

    //
    // testable=false : Keeps the test in client mode, rather than tests being run in the server container
    //
    @Deployment(testable = false)
    public static WebArchive createRestDeployment() {
        return ShrinkWrap.createFromZipFile(WebArchive.class, new File("target/vdb-builder.war"));
    }

    @BeforeClass
    public static void beforeAll() throws Exception {
        _kengineDataDir = Files.createTempDirectory(null, new FileAttribute[0]);
        System.setProperty(SystemConstants.ENGINE_DATA_DIR, _kengineDataDir.toString());

        System.setProperty("org.jboss.resteasy.port", TEST_PORT);
        final URI baseUri = URI.create(TestPortProvider.generateBaseUrl());
        BASE_URI = UriBuilder.fromUri(baseUri).path("/vdb-builder/v1").build();
        _uriBuilder = new KomodoRestUriBuilder(BASE_URI);
    }

    private Invocation.Builder request(final URI uri) {
        return this.client.target(uri.toString()).request();
    }

    private TeiidInstance getTeiidInstance() throws Exception {
        EventManager eventMgr = new DummyEventManager();

        TeiidParent parent = mock(TeiidParent.class);
        when(parent.getHost()).thenReturn(HostProvider.DEFAULT_HOST);
        when(parent.getUsername()).thenReturn(TeiidAdminInfo.DEFAULT_ADMIN_USERNAME);
        when(parent.getPassword()).thenReturn(TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD);
        when(parent.getPort()).thenReturn(TeiidAdminInfo.DEFAULT_PORT);
        when(parent.getEventManager()).thenReturn(eventMgr);

        TeiidJdbcInfo jdbcInfo = mock(TeiidJdbcInfo.class);
        when(jdbcInfo.getHostProvider()).thenReturn(parent);
        when(jdbcInfo.getUsername()).thenReturn(TeiidJdbcInfo.DEFAULT_JDBC_USERNAME);
        when(jdbcInfo.getPassword()).thenReturn(TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD);
        when(jdbcInfo.getPort()).thenReturn(TeiidJdbcInfo.DEFAULT_PORT);
        when(jdbcInfo.isSecure()).thenReturn(true);

        return service.getTeiidInstance(parent, jdbcInfo);
    }

    @Before
    public void beforeEach() throws Exception {
        this.client = ClientBuilder.newClient();

        this.service = PluginService.getInstance().getDefaultTeiidService();

        helperInstance = getTeiidInstance();
        helperInstance.connect();

        // Deploy sample vdb for service to find
        helperInstance.deployDynamicVdb(TestUtilities.SAMPLE_VDB_FILE, TestUtilities.sampleExample());
        Thread.sleep(2000);
    }

    @After
    public void afterEach() throws Exception {
        helperInstance.undeployDynamicVdb(TestUtilities.SAMPLE_VDB_FILE);
        Thread.sleep(2000);

        if (helperInstance != null)
            helperInstance.disconnect();

        helperInstance = null;
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

        this.response = request(uri).post(Entity.json(teiidAttrs));
        assertEquals(200, this.response.getStatus());
        final String entity = this.response.readEntity(String.class);
//        System.out.println("Response:\n" + entity);

        RestTeiid rt = KomodoJsonMarshaller.unmarshall(entity, RestTeiid.class);
        assertNotNull(TeiidInstance.DEFAULT_HOST, rt.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, rt.getBaseUri().toString());
        assertEquals("/tko:komodo/tko:workspace/localhost", rt.getDataPath());
        assertEquals(KomodoType.TEIID, rt.getkType());
        assertFalse(rt.hasChildren());

        assertEquals(TeiidInstance.DEFAULT_HOST, rt.getHost());
        assertEquals(TeiidVersionProvider.getInstance().getTeiidVersion().toString(), rt.getVersion());

        assertEquals(TeiidAdminInfo.DEFAULT_ADMIN_USERNAME, rt.getAdminUser());
        assertEquals(TeiidAdminInfo.DEFAULT_ADMIN_PASSWORD, rt.getAdminPassword());
        assertEquals(TeiidAdminInfo.DEFAULT_PORT, rt.getAdminPort());

        assertEquals(TeiidJdbcInfo.DEFAULT_JDBC_USERNAME, rt.getJdbcUser());
        assertEquals(TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD, rt.getJdbcPassword());
        assertEquals(TeiidJdbcInfo.DEFAULT_PORT, rt.getJdbcPort());
    }

    @SuppressWarnings( "incomplete-switch" )
    @Test
    public void shouldGetVdbs() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.VDBS_SEGMENT)
                                          .build();

        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertEquals(200, this.response.getStatus());
//        System.out.println("Response:\n" + entity);

        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entity, RestVdb[].class);
        assertTrue(vdbs.length == 1);

        RestVdb vdb = vdbs[0];
        String vdbName = TestUtilities.SAMPLE_VDB_NAME;
        assertNotNull(vdbName, vdb.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, vdb.getBaseUri().toString());
        assertEquals("/tko:komodo/tko:workspace/tko:teiidCache/localhost/sample", vdb.getDataPath());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertFalse(vdb.hasChildren());
        assertEquals(vdbName, vdb.getName());

        for(RestLink link : vdb.getLinks()) {
            switch(link.getRel()) {
                case SELF:
                    assertEquals(BASE_URI + File.separator +
                                             V1Constants.TEIID_SEGMENT + File.separator +
                                             "localhost" + File.separator +
                                             V1Constants.VDBS_SEGMENT + File.separator +
                                             vdbName,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(BASE_URI + File.separator +
                                             V1Constants.TEIID_SEGMENT + File.separator +
                                             "localhost" + File.separator +
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

        this.response = request(uri).get();

        final String entity = this.response.readEntity(String.class);
        assertEquals(200, this.response.getStatus());
//        System.out.println("Response:\n" + entity);

        RestVdb vdb = KomodoJsonMarshaller.unmarshall(entity, RestVdb.class);
        assertNotNull(vdb);

        String vdbName = TestUtilities.SAMPLE_VDB_NAME;
        assertNotNull(vdbName, vdb.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, vdb.getBaseUri().toString());
        assertEquals("/tko:komodo/tko:workspace/tko:teiidCache/localhost/sample", vdb.getDataPath());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertFalse(vdb.hasChildren());
        assertEquals(vdbName, vdb.getName());

        for(RestLink link : vdb.getLinks()) {
            switch(link.getRel()) {
                case SELF:
                    assertEquals(BASE_URI + File.separator +
                                             V1Constants.TEIID_SEGMENT + File.separator +
                                             "localhost" + File.separator +
                                             V1Constants.VDBS_SEGMENT + File.separator +
                                             vdbName,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(BASE_URI + File.separator +
                                             V1Constants.TEIID_SEGMENT + File.separator +
                                             "localhost" + File.separator +
                                             V1Constants.VDBS_SEGMENT,
                                             link.getHref().toString());
            }
        }
    }
}
