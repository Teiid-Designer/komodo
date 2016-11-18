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
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.io.File;
import java.io.InputStream;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.security.GeneralSecurityException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import javax.net.ssl.SSLContext;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.apache.http.auth.Credentials;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLContextBuilder;
import org.apache.http.conn.ssl.TrustStrategy;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.conn.BasicHttpClientConnectionManager;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.jboss.resteasy.client.core.executors.ApacheHttpClient4Executor;
import org.jboss.resteasy.test.TestPortProvider;
import org.jboss.shrinkwrap.api.ShrinkWrap;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.komodo.osgi.PluginService;
import org.komodo.relational.workspace.ServerManager;
import org.komodo.repository.RepositoryImpl;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.cors.CorsHeaders;
import org.komodo.rest.RestLink;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.datasource.RestDataSource;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoFileAttributes;
import org.komodo.rest.relational.request.KomodoPathAttribute;
import org.komodo.rest.relational.request.KomodoQueryAttribute;
import org.komodo.rest.relational.request.KomodoTeiidAttributes;
import org.komodo.rest.relational.response.KomodoStatusObject;
import org.komodo.rest.relational.response.KomodoStorageAttributes;
import org.komodo.rest.relational.response.RestQueryResult;
import org.komodo.rest.relational.response.RestQueryRow;
import org.komodo.rest.relational.response.RestTeiid;
import org.komodo.rest.relational.response.RestTeiidStatus;
import org.komodo.rest.relational.response.RestTeiidVdbStatus;
import org.komodo.rest.relational.response.RestTeiidVdbStatusVdb;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.runtime.DataSourceDriver;
import org.komodo.spi.runtime.EventManager;
import org.komodo.spi.runtime.ExecutionAdmin.ConnectivityType;
import org.komodo.spi.runtime.HostProvider;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.test.utils.DummyEventManager;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;

@RunWith(Arquillian.class)
@SuppressWarnings( {"javadoc", "nls"} )
public final class IT_KomodoTeiidServiceTest implements StringConstants {

    private static final String USER_NAME = "user";

    private static final String PASSWORD = "user";

    private static final String TEST_PORT = "8443";

    private static final String TEIID_DATA_PATH = RepositoryImpl.SERVERS_ROOT + FORWARD_SLASH + ServerManager.DEFAULT_SERVER_NAME;

    private static final String CACHED_TEIID_DATA_PATH = RepositoryImpl.TEIID_CACHE_ROOT + FORWARD_SLASH + ServerManager.DEFAULT_SERVER_NAME;

    private static final String MYSQL_DRIVER = "mysql-connector";

    private static TeiidVersion teiidVersion;

    private static Path _kengineDataDir;

    private static KomodoRestUriBuilder _uriBuilder;

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
        teiidVersion = TeiidVersionProvider.getInstance().getTeiidVersion();

        _kengineDataDir = Files.createTempDirectory(null, new FileAttribute[0]);
        System.setProperty(SystemConstants.ENGINE_DATA_DIR, _kengineDataDir.toString());

        System.setProperty("org.jboss.resteasy.port", TEST_PORT);
        final URI baseUri = URI.create(TestPortProvider.generateBaseUrl());
        BASE_URI = UriBuilder.fromUri(baseUri).scheme("https").path("/vdb-builder/v1").build();
        _uriBuilder = new KomodoRestUriBuilder(BASE_URI);
    }


    @AfterClass
    public static void afterAll() throws Exception {
        if (_kengineDataDir != null) {
            FileUtils.removeDirectoryAndChildren(_kengineDataDir.toFile());
        }
    }

    private void addHeader(ClientRequest request, String name, Object value) {
        request.getHeadersAsObjects().add(name, value);
    }

    /**
     * Builds an {@link ApacheHttpClient4Executor} which does NOT verify ssl certificates so allows for the
     * self-signed certificates used in the integration testing.
     *
     * @return client executor with no ssl certificate verification
     *
     * @throws GeneralSecurityException
     */
    private ApacheHttpClient4Executor createSSLTrustingClientExecutor() throws GeneralSecurityException {
        RegistryBuilder<ConnectionSocketFactory> registryBuilder = RegistryBuilder.<ConnectionSocketFactory>create()
            .register("http", PlainConnectionSocketFactory.getSocketFactory());

        TrustStrategy trustStrategy = new TrustStrategy() {
            @Override
            public boolean isTrusted(X509Certificate[] chain, String authType) throws CertificateException {
                return true;
            }
        };

        SSLContextBuilder sslctxBuilder = new SSLContextBuilder();
        sslctxBuilder.loadTrustMaterial(null, trustStrategy);
        SSLContext sslContext = sslctxBuilder.build();

        SSLConnectionSocketFactory sslSocketFactory = new SSLConnectionSocketFactory(sslContext,
                                                                                                 SSLConnectionSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER);
        registryBuilder.register("https", sslSocketFactory);

        BasicHttpClientConnectionManager mgr = new BasicHttpClientConnectionManager(registryBuilder.build());

        HttpClientBuilder clientBuilder = HttpClientBuilder.create();
        clientBuilder.setConnectionManager(mgr);

        Credentials credentials = new UsernamePasswordCredentials(USER_NAME, PASSWORD);
        CredentialsProvider credentialsProvider = new BasicCredentialsProvider();
        credentialsProvider.setCredentials(org.apache.http.auth.AuthScope.ANY, credentials);
        clientBuilder.setDefaultCredentialsProvider(credentialsProvider);

        return new ApacheHttpClient4Executor(clientBuilder.build());
    }

    private ClientRequest request(final URI uri, MediaType type) throws Exception {
        ClientRequest request = new ClientRequest(uri.toString(), createSSLTrustingClientExecutor());
        if (type != null)
            request.accept(type);

        return request;
    }

    private void assertNoMysqlDriver() throws Exception {
        wait(2);

        Collection<DataSourceDriver> drivers = helperInstance.getDataSourceDrivers();
        for (DataSourceDriver driver : drivers) {
            assertFalse(driver.getName().startsWith(MYSQL_DRIVER));
        }
    }

    private void assertMysqlDriver() throws Exception {
        boolean found = false;
        for (int i = 0; i < 10 && !found; i++) {
            wait(3);
            Collection<DataSourceDriver> drivers = helperInstance.getDataSourceDrivers();
            for (DataSourceDriver driver : drivers) {
                // Use startswith rather than equals since the
                // mysql connector gives up 2 drivers rather than just 1
                if (driver.getName().startsWith(MYSQL_DRIVER)) {
                    found = true;
                    break;
                }
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
        when(parent.getPort()).thenReturn(TeiidAdminInfo.Util.defaultPort(teiidVersion));
        when(parent.getEventManager()).thenReturn(eventMgr);

        TeiidJdbcInfo jdbcInfo = mock(TeiidJdbcInfo.class);
        when(jdbcInfo.getHostProvider()).thenReturn(parent);
        when(jdbcInfo.getUsername()).thenReturn(TeiidJdbcInfo.DEFAULT_JDBC_USERNAME);
        when(jdbcInfo.getPassword()).thenReturn(TeiidJdbcInfo.DEFAULT_JDBC_PASSWORD);
        when(jdbcInfo.getPort()).thenReturn(TeiidJdbcInfo.DEFAULT_PORT);
        when(jdbcInfo.isSecure()).thenReturn(true);

        return service.getTeiidInstance(parent, jdbcInfo);
    }

    private void setJdbcName(String jdbcUser) throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.TEIID_SEGMENT)
                                            .path(V1Constants.TEIID_CREDENTIALS)
                                            .build();

        KomodoTeiidAttributes teiidAttrs = new KomodoTeiidAttributes();
        teiidAttrs.setJdbcUser(jdbcUser);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.body(MediaType.APPLICATION_JSON_TYPE, teiidAttrs);

        ClientResponse<String> response = request.post(String.class);
        assertEquals(200, response.getStatus());
    }

    private void waitForVdb(String vdbName) throws Exception {
        TeiidVdb vdb = null;
        int TIMEOUT = 40;

        //
        // Timeout after 120 seconds
        //
        for (int i = 0; vdb == null || i < TIMEOUT; ++i) {
            vdb = helperInstance.getVdb(vdbName);

            System.out.println("Waiting on status of vdb " + vdbName + " active: " + vdb.isActive() + " loading: " + vdb.isLoading() + " ....");

            if (vdb != null && vdb.isActive())
                return; // Found it and its active

            wait(3);
            helperInstance.reconnect();
        }

        fail("Timed out waiting for vdb " + vdbName + " to become active");
    }

    private void wait(int seconds) {
        try {
            Thread.sleep(seconds * 1000);
        } catch (Exception ex) {
            // Nothing required
        }
    }

    @Before
    public void beforeEach() throws Exception {
        this.service = PluginService.getInstance().getDefaultTeiidService();

        helperInstance = getTeiidInstance();
        helperInstance.connect();

        // Deploy sample vdb for service to find
        helperInstance.deployDynamicVdb(TestUtilities.SAMPLE_VDB_FILE, TestUtilities.sampleExample());
        waitForVdb(TestUtilities.SAMPLE_VDB_NAME);
    }

    @After
    public void afterEach() throws Exception {
        setJdbcName(TeiidJdbcInfo.DEFAULT_JDBC_USERNAME);

        //
        // Refresh the artifacts of this client instance
        //
        helperInstance.reconnect();

        helperInstance.undeployDynamicVdb(TestUtilities.SAMPLE_VDB_FILE);

        Set<String> undeployDrivers = new HashSet<String>();
        Collection<DataSourceDriver> drivers = helperInstance.getDataSourceDrivers();
        for (DataSourceDriver driver : drivers) {
            if (driver.getName().startsWith(MYSQL_DRIVER)) {
                String driverName = driver.getName();
                //
                // MySQL has 2 drivers so concatenates the class name
                // to the end of the driver names but means that the driver
                // cannot be undeployed unless the class name is removed
                //
                int endsWithClass = driverName.lastIndexOf(driver.getClassName());
                if (endsWithClass > -1)
                    driverName = driverName.substring(0, endsWithClass);

                undeployDrivers.add(driverName);
            }
        }

        for (String driver : undeployDrivers) {
            try {
                helperInstance.undeployDriver(driver);
            } catch (Exception ex) {
                // Flag as a warning that something in the test is going awry
                ex.printStackTrace();
            }
        }

        wait(2);

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
                        assertEquals("8.12.4", status.getVersion());
                        assertTrue(status.isTeiidInstanceAvailable());
                        assertTrue(status.isConnected());
                        assertEquals(1, status.getDataSourceSize());
                        assertEquals(3, status.getDataSourceDriverSize());
                        assertEquals(54, status.getTranslatorSize());
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
    public void shouldGetDataSources() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.TEIID_SEGMENT)
                                          .path(V1Constants.DATA_SOURCES_SEGMENT)
                                          .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();

        System.out.println("Response:\n" + entity);
        assertEquals(200, response.getStatus());

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

        assertEquals(1, attributes.size());

        String deployMsg = RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_DEPLOYED);
        assertEquals(deployMsg, attributes.values().iterator().next());

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

        wait(2);

        helperInstance.reconnect();

        String title = RelationalMessages.getString(RelationalMessages.Info.DRIVER_DEPLOYMENT_STATUS_TITLE);
        assertEquals(title, status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        assertEquals(1, attributes.size());

        String deployMsg = RelationalMessages.getString(RelationalMessages.Info.DRIVER_SUCCESSFULLY_UNDEPLOYED);
        assertEquals(deployMsg, attributes.values().iterator().next());

        assertNoMysqlDriver();
    }

    private void importDataService() throws Exception {
        //
        // Import the data service into the workspace
        //
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                          .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                          .path(V1Constants.IMPORT)
                                          .build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        storageAttr.setDocumentType(DocumentType.ZIP);

        InputStream usStatesDSStream = TestUtilities.usStatesDataserviceExample();
        byte[] sampleBytes = TestUtilities.streamToBytes(usStatesDSStream);
        String content = Base64.getEncoder().encodeToString(sampleBytes);
        storageAttr.setContent(content);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.body(MediaType.APPLICATION_JSON_TYPE, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
    }

    private void deployDataService() throws Exception {
        KomodoPathAttribute pathAttr = new KomodoPathAttribute();
        String path = RepositoryImpl.komodoWorkspacePath(null) + FORWARD_SLASH +
                                        USER_NAME + FORWARD_SLASH + "UsStatesService";
        pathAttr.setPath(path);

        //
        // Deploy the data service
        //
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                    .path(V1Constants.TEIID_SEGMENT)
                                    .path(V1Constants.DATA_SERVICE_SEGMENT)
                                    .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.body(MediaType.APPLICATION_JSON_TYPE, pathAttr);
        ClientResponse<String> response = request.post(String.class);
        final String entity = response.getEntity();

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        Map<String, String> attributes = status.getAttributes();
        for (Map.Entry<String, String> attribute : attributes.entrySet()) {
            assertFalse("Error occurred in deployment: " + attribute.getValue(),
                        attribute.getKey().startsWith("ErrorMessage"));
        }
    }

    @Test
    public void shouldDeployDataService() throws Exception {
        try {
            importDataService();

            deployDataService();

        } finally {
            try {
                helperInstance.undeployDynamicVdb(TestUtilities.US_STATES_VDB_NAME);
                helperInstance.deleteDataSource(TestUtilities.US_STATES_DATA_SOURCE_NAME);
            } catch (Exception ex) {
                // Nothing to do
            }
        }
    }

    private void queryDataService(KomodoQueryAttribute queryAttr, int expRowCount, int firstCellValue) throws Exception {
        URI uri;
        String entity;
        //
        // Query the deployed vdb
        //
        uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                    .path(V1Constants.TEIID_SEGMENT)
                                    .path(V1Constants.QUERY_SEGMENT)
                                    .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        request.body(MediaType.APPLICATION_JSON_TYPE, queryAttr);
        ClientResponse<String> response = request.post(String.class);
        entity = response.getEntity();

        System.out.println("Entity: " + entity);
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        RestQueryResult result = KomodoJsonMarshaller.unmarshall(entity, RestQueryResult.class);
        assertNotNull(result);
        assertEquals(expRowCount, result.getRows().length);

        RestQueryRow firstRow = result.getRows()[0];
        String value = firstRow.getValues()[0];
        assertEquals(new Integer(firstCellValue).toString(), value);
    }

    @Test
    public void shouldQueryTeiid() throws Exception {
        try {
            importDataService();

            deployDataService();

            //
            // Give the vdb time to become active
            //
            waitForVdb("usstates");

            KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
            queryAttr.setQuery("SELECT * FROM state");
            queryAttr.setTarget("usstates");

            queryDataService(queryAttr, 59, 1);

        } finally {
            try {
                helperInstance.undeployDynamicVdb(TestUtilities.US_STATES_VDB_NAME);
                helperInstance.deleteDataSource(TestUtilities.US_STATES_DATA_SOURCE_NAME);
            } catch (Exception ex) {
                // Nothing to do
            }
        }
    }

    @Test
    public void shouldQueryTeiidWithLimitAndOffset() throws Exception {
        try {
            importDataService();

            deployDataService();

            //
            // Give the vdb time to become active
            //
            waitForVdb("usstates");

            KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
            queryAttr.setQuery("SELECT * FROM state");
            queryAttr.setTarget("usstates");

            int offset = 5;
            int limit = 10;
            queryAttr.setLimit(limit);
            queryAttr.setOffset(offset);

            queryDataService(queryAttr, limit, offset);

        } finally {
            try {
                helperInstance.undeployDynamicVdb(TestUtilities.US_STATES_VDB_NAME);
                helperInstance.deleteDataSource(TestUtilities.US_STATES_DATA_SOURCE_NAME);
            } catch (Exception ex) {
                // Nothing to do
            }
        }
    }

    @Test
    public void shouldQueryTeiidUsingDataservice() throws Exception {
        try {
            importDataService();

            deployDataService();

            //
            // Give the vdb time to become active
            //
            waitForVdb("usstates");

            String dsPath = RepositoryImpl.komodoWorkspacePath(null) + FORWARD_SLASH +
                                        USER_NAME + FORWARD_SLASH + "UsStatesService";

            KomodoQueryAttribute queryAttr = new KomodoQueryAttribute();
            queryAttr.setQuery("SELECT * FROM state");
            queryAttr.setTarget(dsPath);

            queryDataService(queryAttr, 59, 1);

        } finally {
            try {
                helperInstance.undeployDynamicVdb(TestUtilities.US_STATES_VDB_NAME);
                helperInstance.deleteDataSource(TestUtilities.US_STATES_DATA_SOURCE_NAME);
            } catch (Exception ex) {
                // Nothing to do
            }
        }
    }

    private ClientResponse<String> ping(ConnectivityType cType) throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.TEIID_SEGMENT)
                                            .path(V1Constants.PING_SEGMENT)
                                            .queryParam(V1Constants.PING_TYPE_PARAMETER, cType.toString().toLowerCase())
                                            .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        assertEquals(200, response.getStatus());
        return response;
    }

    private void shouldPing(ConnectivityType cType) throws Exception {

        ClientResponse<String> response = ping(cType);
        String entity = response.getEntity();

        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        Map<String, String> attributes = status.getAttributes();
        assertEquals("true", attributes.get("OK"));
        assertEquals("OK", attributes.get("Message"));
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

    @Ignore
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
