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
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.util.Base64;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.core.MediaType;
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
import org.komodo.relational.workspace.ServerManager;
import org.komodo.repository.RepositoryImpl;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.relational.KomodoFileAttributes;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.KomodoStatusObject;
import org.komodo.rest.relational.KomodoTeiidAttributes;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.RestTeiid;
import org.komodo.rest.relational.RestTeiidStatus;
import org.komodo.rest.relational.RestTeiidVdbStatus;
import org.komodo.rest.relational.RestTeiidVdbStatusVdb;
import org.komodo.rest.relational.RestVdb;
import org.komodo.rest.relational.RestVdbTranslator;
import org.komodo.rest.relational.datasource.RestDataSource;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.DataSourceDriver;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.test.utils.DummyEventManager;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;

@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls"} )
public final class IT_KomodoTeiidServiceTest implements StringConstants {

    private static final String TEST_PORT = "8080";

    private static final String TEIID_DATA_PATH = RepositoryImpl.SERVERS_ROOT + FORWARD_SLASH + ServerManager.DEFAULT_SERVER_NAME;

    private static final String CACHED_TEIID_DATA_PATH = RepositoryImpl.TEIID_CACHE_ROOT + FORWARD_SLASH + ServerManager.DEFAULT_SERVER_NAME;

    private static final String MYSQL_DRIVER = "mysql-connector";

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

    private Invocation.Builder request(final URI uri, MediaType... types) {
        if (types == null || types.length == 0)
            return this.client.target(uri.toString()).request();

        return this.client.target(uri.toString()).request(types);
    }

    private void assertNoMysqlDriver() throws Exception {
        Collection<DataSourceDriver> drivers = helperInstance.getDataSourceDrivers();
        for (DataSourceDriver driver : drivers) {
            assertFalse(driver.getName().startsWith(MYSQL_DRIVER));
        }
    }

    private void assertMysqlDriver() throws Exception {
        boolean found = false;
        Collection<DataSourceDriver> drivers = helperInstance.getDataSourceDrivers();
        for (DataSourceDriver driver : drivers) {
            // Use startswith rather than equals since the
            // mysql connector gives up 2 drivers rather than just 1
            if (driver.getName().startsWith(MYSQL_DRIVER)) {
                found = true;
                break;
            }
        }
        assertTrue("Cannot find deployed driver", found);
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

        try {
            helperInstance.undeployDriver(MYSQL_DRIVER);
        } catch (Exception ex) {
            // Ignore errors
        }

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
        final String entity = this.response.readEntity(String.class);
        System.out.println("Response:\n" + entity);
        assertEquals(200, this.response.getStatus());

        RestTeiid rt = KomodoJsonMarshaller.unmarshall(entity, RestTeiid.class);
        assertNotNull(TeiidInstance.DEFAULT_HOST, rt.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, rt.getBaseUri().toString());
        assertEquals(TEIID_DATA_PATH, rt.getDataPath());
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

    @Test
    public void shouldGetTeiidStatus() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .build();

        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);

        System.out.println("Response:\n" + entity);
        assertEquals(200, this.response.getStatus());

        RestTeiidStatus status = KomodoJsonMarshaller.unmarshall(entity, RestTeiidStatus.class);
        assertNotNull(status);

        assertEquals("DefaultServer", status.getId());
        assertEquals("localhost", status.getHost());
        assertEquals("8.12.4", status.getVersion());
        assertTrue(status.isTeiidInstanceAvailable());
        assertTrue(status.isConnected());
        assertEquals(1, status.getDataSourceSize());
        assertEquals(3, status.getDataSourceDriverSize());
        assertEquals(54, status.getTranslatorSize());
        assertEquals(1, status.getVdbSize());
    }

    @Test
    public void shouldGetTeiidVdbStatus() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.STATUS_SEGMENT)
                                          .path(V1Constants.VDBS_SEGMENT)
                                          .build();

        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);

        System.out.println("Response:\n" + entity);
        assertEquals(200, this.response.getStatus());

        RestTeiidVdbStatus status = KomodoJsonMarshaller.unmarshall(entity, RestTeiidVdbStatus.class);
        assertNotNull(status);

        List<RestTeiidVdbStatusVdb> vdbProperties = status.getVdbProperties();
        assertEquals(1, vdbProperties.size());

        RestTeiidVdbStatusVdb vdb = vdbProperties.get(0);
        assertNotNull(vdb);
        
        assertEquals("sample", vdb.getName());
        assertEquals("sample-vdb.xml", vdb.getDeployedName());
        assertEquals(1, vdb.getVersion());
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

        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);;
        System.out.println("Response:\n" + entity);
        assertEquals(200, this.response.getStatus());

        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entity, RestVdb[].class);
        assertTrue(vdbs.length == 1);

        RestVdb vdb = vdbs[0];
        String vdbName = TestUtilities.SAMPLE_VDB_NAME;
        assertNotNull(vdbName, vdb.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, vdb.getBaseUri().toString());
        assertEquals(CACHED_TEIID_DATA_PATH + FORWARD_SLASH + "sample", vdb.getDataPath());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertTrue(vdb.hasChildren());
        assertEquals(vdbName, vdb.getName());

        for(RestLink link : vdb.getLinks()) {
            switch(link.getRel()) {
                case SELF:
                    assertEquals(BASE_URI + File.separator +
                                             V1Constants.TEIID_SEGMENT + File.separator +
                                             ServerManager.DEFAULT_SERVER_NAME + File.separator +
                                             V1Constants.VDBS_SEGMENT + File.separator +
                                             vdbName,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(BASE_URI + File.separator +
                                             V1Constants.TEIID_SEGMENT + File.separator +
                                             ServerManager.DEFAULT_SERVER_NAME + File.separator +
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
        System.out.println("Response:\n" + entity);
        assertEquals(200, this.response.getStatus());

        RestVdb vdb = KomodoJsonMarshaller.unmarshall(entity, RestVdb.class);
        assertNotNull(vdb);

        String vdbName = TestUtilities.SAMPLE_VDB_NAME;
        assertNotNull(vdbName, vdb.getId());
        assertEquals(_uriBuilder.baseUri() + FORWARD_SLASH, vdb.getBaseUri().toString());
        assertEquals(CACHED_TEIID_DATA_PATH + FORWARD_SLASH + "sample", vdb.getDataPath());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertTrue(vdb.hasChildren());
        assertEquals(vdbName, vdb.getName());

        for(RestLink link : vdb.getLinks()) {
            switch(link.getRel()) {
                case SELF:
                    assertEquals(BASE_URI + File.separator +
                                             V1Constants.TEIID_SEGMENT + File.separator +
                                             ServerManager.DEFAULT_SERVER_NAME + File.separator +
                                             V1Constants.VDBS_SEGMENT + File.separator +
                                             vdbName,
                                             link.getHref().toString());
                    break;
                case PARENT:
                    assertEquals(BASE_URI + File.separator +
                                             V1Constants.TEIID_SEGMENT + File.separator +
                                             ServerManager.DEFAULT_SERVER_NAME + File.separator +
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

        for (int i = 0; i < iterations; ++i) {
            Runnable runnable = new Runnable() {

                @Override
                public void run() {
                    Client client = ClientBuilder.newClient();
                    Response response = client.target(uri.toString()).request().get();
                    Thread.yield();

                    String entity = response.readEntity(String.class);
                    System.out.println("Response:\n" + entity);
                    assertEquals(200, response.getStatus());
                    RestTeiidStatus status = KomodoJsonMarshaller.unmarshall(entity, RestTeiidStatus.class);
                    assertNotNull(status);

                    assertEquals("DefaultServer", status.getId());
                    assertEquals("localhost", status.getHost());
                    assertEquals("8.12.4", status.getVersion());
                    assertTrue(status.isTeiidInstanceAvailable());
                    assertTrue(status.isConnected());
                    assertEquals(1, status.getDataSourceSize());
                    assertEquals(3, status.getDataSourceDriverSize());
                    assertEquals(54, status.getTranslatorSize());
                    assertEquals(1, status.getVdbSize());

                    latch.countDown();
                }
            };

            Thread thread = new Thread(runnable);
            thread.start();
        }

        assertTrue(latch.await(3, TimeUnit.MINUTES));
    }

    @Test
    public void shouldGetTranslators() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.TRANSLATORS_SEGMENT)
                                          .build();

        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);;
        System.out.println("Response:\n" + entity);
        assertEquals(200, this.response.getStatus());

        RestVdbTranslator[] translators = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbTranslator[].class);
        assertTrue(translators.length > 0);

        for (RestVdbTranslator translator : translators) {
            assertNotNull(translator.getId());
            assertEquals(3, translator.getLinks().size());
        }
    }

    @Test
    public void shouldGetDataSources() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.DATA_SOURCES_SEGMENT)
                                          .build();

        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        System.out.println("Response:\n" + entity);
        assertEquals(200, this.response.getStatus());

        RestDataSource[] dataSources = KomodoJsonMarshaller.unmarshallArray(entity, RestDataSource[].class);
        assertTrue(dataSources.length > 0);

        for (RestDataSource dataSource : dataSources) {
            assertNotNull(dataSource.getId());
            assertEquals(3, dataSource.getLinks().size());
        }
    }

    @Test
    public void shouldDeployDriver() throws Exception {
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

        this.response = request(uri, MediaType.APPLICATION_JSON_TYPE).post(Entity.json(fileAttr));
        final String entity = this.response.readEntity(String.class);
        assertEquals(Response.Status.OK.getStatusCode(), this.response.getStatus());

        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
        assertEquals(title, status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        assertEquals(1, attributes.size());

        String deployMsg = RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_DEPLOYED);
        assertEquals(deployMsg, attributes.values().iterator().next());

        Thread.sleep(2000);

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
        Thread.sleep(2000);

        assertMysqlDriver();
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.TEIID_SEGMENT)
                                            .path(V1Constants.TEIID_DRIVER)
                                            .path(MYSQL_DRIVER)
                                            .build();

        this.response = request(uri, MediaType.APPLICATION_JSON_TYPE).delete();
        final String entity = this.response.readEntity(String.class);
        assertEquals(Response.Status.OK.getStatusCode(), this.response.getStatus());

        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
        assertEquals(title, status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        assertEquals(1, attributes.size());

        String deployMsg = RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_UNDEPLOYED);
        assertEquals(deployMsg, attributes.values().iterator().next());

        Thread.sleep(2000);

        assertNoMysqlDriver();
    }
}
