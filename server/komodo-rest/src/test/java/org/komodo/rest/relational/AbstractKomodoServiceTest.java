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
package org.komodo.rest.relational;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URI;
import java.net.URL;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileAttribute;
import java.security.GeneralSecurityException;
import java.security.Principal;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.net.ssl.SSLContext;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;
import org.apache.http.auth.BasicUserPrincipal;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.conn.socket.ConnectionSocketFactory;
import org.apache.http.conn.socket.PlainConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLConnectionSocketFactory;
import org.apache.http.conn.ssl.SSLContextBuilder;
import org.apache.http.conn.ssl.TrustStrategy;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.impl.conn.BasicHttpClientConnectionManager;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.core.executors.ApacheHttpClient4Executor;
import org.jboss.resteasy.plugins.server.embedded.SecurityDomain;
import org.jboss.resteasy.plugins.server.tjws.TJWSEmbeddedJaxrsServer;
import org.jboss.resteasy.test.TestPortProvider;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.komodo.core.KEngine;
import org.komodo.repository.SynchronousCallback;
import org.komodo.repository.search.ComparisonOperator;
import org.komodo.repository.search.ObjectSearcher;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.test.utils.TestUtilities;
import org.modeshape.jcr.ModeShapeLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

/**
 *
 */
@SuppressWarnings( {"nls", "javadoc"} )
public abstract class AbstractKomodoServiceTest implements V1Constants {

    private static final int TEST_PORT = 8443;

    protected static final String USER_NAME = "user";

    protected static final String PASSWORD = "user";

    private static Path _kengineDataDir;
    protected static KomodoRestV1Application _restApp;
    protected static TJWSEmbeddedJaxrsServer _server;
    protected static KomodoRestUriBuilder _uriBuilder;
    private static URI _appUri;

    /*
     * Mocks the real jboss basic authentication security domain
     */
    @SuppressWarnings( "unused" )
    private static class BasicAthenticationSecurityDomain implements SecurityDomain {

        @Override
        public Principal authenticate(String user, String password) throws SecurityException {
            if (!user.equals(USER_NAME) || !password.equals(PASSWORD))
                throw new SecurityException("Access denied to user " + user);

            return new BasicUserPrincipal(user);
        }

        /**
         * Rest 2 version of method (no @Override to ensure compilation)
         */
        public boolean isUserInRoll(Principal aUsername, String aRole) {
            // No role based checks so return true
            return true;
        }

        /**
         * Rest 3 version of method (no @Override to ensure compilation)
         */
        public boolean isUserInRole(Principal username, String role) {
            // No role based checks so return true
            return true;
        }
    }

    @AfterClass
    public static void afterAll() throws Exception {
        if (_server != null)
            _server.stop();

        if (_restApp != null)
            _restApp.stop();

        //
        // Allow other instances of the KomodoRestV1Application to be deployed
        // with a clean komodo engine by destroying the current static instance
        // loaded from these tests
        //
        Field instanceField = KEngine.class.getDeclaredField("_instance");
        instanceField.setAccessible(true);
        instanceField.set(KEngine.class, null);

        //
        // Remove the temp repository
        //
        Files.walkFileTree(_kengineDataDir, new SimpleFileVisitor<Path>() {
            @Override
            public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
                try {
                    Files.delete(file);
                } catch (Exception ex) {
                    file.toFile().deleteOnExit();
                }
                return FileVisitResult.CONTINUE;
            }

            @Override
            public FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
                try {
                    Files.delete(dir);
                } catch (Exception ex) {
                    dir.toFile().deleteOnExit();
                }
                return FileVisitResult.CONTINUE;
            }
        });

        try {
            Files.deleteIfExists(_kengineDataDir);
        } catch (Exception ex) {
            _kengineDataDir.toFile().deleteOnExit();
        }
    }

    @BeforeClass
    public static void beforeAll() throws Exception {
        _kengineDataDir = Files.createTempDirectory(null, new FileAttribute[0]);
        System.setProperty(SystemConstants.ENGINE_DATA_DIR, _kengineDataDir.toString());

        _server = new TJWSEmbeddedJaxrsServer();
        _server.setSSLPort(TEST_PORT);

        _restApp = new KomodoRestV1Application();

        URL resource = AbstractKomodoServiceTest.class.getClassLoader().getResource("ssl/server.keystore");
        File keyStore = new File(resource.toURI());
        assertTrue(keyStore.exists());

        _server.setSSLKeyStoreFile(keyStore.getAbsolutePath());
        _server.setSSLKeyStorePass("raleigh");
        _server.setSSLKeyStoreType("JKS");
        _server.setSecurityDomain(new BasicAthenticationSecurityDomain());

        _server.getDeployment().setApplication(_restApp);
        _server.getDeployment().setSecurityEnabled(true);
        _server.start();

        System.setProperty("org.jboss.resteasy.port", Integer.toString(TEST_PORT));
        final URI baseUri = URI.create(TestPortProvider.generateBaseUrl());
        //
        // Note this lacks the /v1 context since the embedded server does not
        // seem to detect context from the application
        //
        _appUri = UriBuilder.fromUri(baseUri).scheme("https").build();
        _uriBuilder = new KomodoRestUriBuilder(_appUri);
    }

    private TeiidVersion teiidVersion;

    /**
     *
     */
    public AbstractKomodoServiceTest() {
        super();
        this.teiidVersion = TeiidVersionProvider.getInstance().getTeiidVersion();
    }

    public TeiidVersion getTeiidVersion() {
        return teiidVersion;
    }

    @After
    public void afterEach() throws Exception {
        _restApp.clearRepository();
    }

    @Before
    public void beforeEach() {
    }

    protected KomodoRestV1Application getRestApp() {
        return _restApp;
    }

    protected void loadVdbs() throws Exception {
        _restApp.importVdb(TestUtilities.allElementsExample(), USER_NAME);
        _restApp.importVdb(TestUtilities.portfolioExample(), USER_NAME);
        _restApp.importVdb(TestUtilities.partsWithKeysExample(), USER_NAME);
        _restApp.importVdb(TestUtilities.tweetExample(), USER_NAME);

        Assert.assertEquals(4, _restApp.getVdbs(USER_NAME).length);
    }

    protected void loadStatesDataService() throws Exception {
        _restApp.importDataservice(TestUtilities.usStatesDataserviceExample(), USER_NAME);
    }

    protected void loadServiceSourceVdb() throws Exception {
        _restApp.importVdb(TestUtilities.usStatesSourceExample(), USER_NAME);

        Assert.assertEquals(1, _restApp.getVdbs(USER_NAME).length);
    }

    protected void loadDsbSingleSourceDataService() throws Exception {
        _restApp.importDataservice(TestUtilities.dsbDataserviceSingleSourceParts(), USER_NAME);
    }

    protected void loadDsbJoinDifferentTableNamesDataService() throws Exception {
        _restApp.importDataservice(TestUtilities.dsbDataserviceJoinDifferentTableNames(), USER_NAME);
    }

    protected void loadDsbJoinSameTableNamesDataService() throws Exception {
        _restApp.importDataservice(TestUtilities.dsbDataserviceJoinSameTableNames(), USER_NAME);
    }

    protected void createDataservice( String serviceName ) throws Exception {
        _restApp.createDataservice(serviceName, false, USER_NAME);

        Assert.assertEquals(1, _restApp.getDataservices(USER_NAME).length);
    }

    protected void createConnection( String connectionName ) throws Exception {
        _restApp.createConnection(connectionName, USER_NAME);

        Assert.assertEquals(1, _restApp.getConnections(USER_NAME).length);
    }

    protected void createDriver( String driverName ) throws Exception {
        _restApp.createDriver(driverName);

        Assert.assertEquals(1, _restApp.getDrivers().length);
    }

    protected void createVdb( String vdbName ) throws Exception {
        _restApp.createVdb(vdbName, USER_NAME);

        Assert.assertEquals(1, _restApp.getVdbs(USER_NAME).length);
    }

    protected void createVdbModel( String vdbName, String modelName ) throws Exception {
        _restApp.createVdbModel(vdbName, modelName, USER_NAME);

        Assert.assertEquals(1, _restApp.getVdbs(USER_NAME).length);
    }

    protected List<String> loadSampleSearches() throws Exception {
        List<String> searchNames = new ArrayList<>();
        Repository repository = _restApp.getDefaultRepository();

        final SynchronousCallback callback = new SynchronousCallback();
        UnitOfWork uow = repository.createTransaction(USER_NAME,
                                                      getClass().getSimpleName() + COLON + "SaveSearchInWorkspace" + COLON + System.currentTimeMillis(),
                                                      false, callback);

        ObjectSearcher vdbsSearch = new ObjectSearcher(repository);
        vdbsSearch.setFromType(VdbLexicon.Vdb.VIRTUAL_DATABASE, "vdbs");
        String vdbSearchName = "Vdbs Search";
        vdbsSearch.write(uow, vdbSearchName);

        ObjectSearcher columnsSearch = new ObjectSearcher(repository);
        columnsSearch.setFromType(TeiidDdlLexicon.CreateTable.TABLE_ELEMENT, "c");
        String columnSearchName = "Columns Search";
        columnsSearch.write(uow, columnSearchName);

        ObjectSearcher columnsWithParamSearch = new ObjectSearcher(repository);
        columnsWithParamSearch.setFromType(TeiidDdlLexicon.CreateTable.TABLE_ELEMENT, "c");
        columnsWithParamSearch.addWhereCompareClause(null, "c", ModeShapeLexicon.LOCALNAME.getString(), ComparisonOperator.LIKE, "{valueParam}");
        String columnsWithParamSearchName = "Columns Search With Where Parameter";
        columnsWithParamSearch.write(uow, columnsWithParamSearchName);

        ObjectSearcher fromParameterSearch = new ObjectSearcher(repository);
        fromParameterSearch.setFromType("{fromTypeParam}", "c");
        String fromParamSearchName = "From Parameter Search";
        fromParameterSearch.write(uow, fromParamSearchName);

        uow.commit();

        if (!callback.await(3, TimeUnit.MINUTES)) {
            throw new Exception("Timed out while loading saved searches");
        }

        if (callback.error() != null)
            throw new Exception(callback.error());

        searchNames.add(vdbSearchName);
        searchNames.add(columnSearchName);
        searchNames.add(columnsWithParamSearchName);
        searchNames.add(fromParamSearchName);

        return searchNames;
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

        return new ApacheHttpClient4Executor(clientBuilder.build());
    }

    protected ClientRequest request(final URI uri, MediaType type) throws Exception {
        ClientRequest request = new ClientRequest(uri.toString(), createSSLTrustingClientExecutor());
        addBasicAuthorizationHeader(request);

        if (type != null)
            request.accept(type);

        return request;
    }

    protected void addBasicAuthorizationHeader(ClientRequest request) {
        //
        // Add this to enable BASIC authorization
        //
        String credentials = USER_NAME + COLON + PASSWORD;
        byte[] encCredentials = Base64.getEncoder().encode(credentials.getBytes());
        String value = "Basic " + new String(encCredentials);
        request.header("Authorization", value);
    }

    protected void addJsonConsumeContentType(ClientRequest request) {
        //
        // Have to add this as the REST operation has a @Consumes annotation
        //
        request.header("Content-Type", MediaType.APPLICATION_JSON);
    }

    protected void addXmlConsumeContentType(ClientRequest request) {
        //
        // Have to add this as the REST operation has a @Consumes annotation
        //
        request.header("Content-Type", MediaType.APPLICATION_XML);
    }

    protected void addBody(ClientRequest request, Object data) {
        request.body(MediaType.APPLICATION_JSON_TYPE, data);
    }

    protected void addHeader(ClientRequest request, String name, Object value) {
        request.getHeadersAsObjects().add(name, value);
    }

    protected void assertPortfolio(RestVdb vdb) {
        String expectedPath = "/tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio";
        assertEquals(expectedPath, vdb.getDataPath());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertTrue(vdb.hasChildren());
        assertEquals(TestUtilities.PORTFOLIO_VDB_NAME, vdb.getName());
        assertEquals("The Portfolio Dynamic VDB", vdb.getDescription());
        assertEquals(expectedPath, vdb.getOriginalFilePath());
        assertFalse(vdb.isPreview());
        assertEquals(1, vdb.getVersion());

        List<RestProperty> properties = vdb.getProperties();
        assertEquals(1, properties.size());
        RestProperty property = properties.iterator().next();
        assertEquals("UseConnectorMetadata", property.getName());
        assertEquals("true", property.getValue());

        Collection<RestLink> links = vdb.getLinks();
        assertEquals(7, links.size());

        int linkCounter = 0;
        for (RestLink link : links) {
            String href = link.getHref().toString();

            if (link.getRel().equals(LinkType.SELF)) {
                linkCounter++;
                assertTrue(href.startsWith(_appUri.toString() + "/workspace/vdbs"));
                assertTrue(href.endsWith(TestUtilities.PORTFOLIO_VDB_NAME));
            } else if (link.getRel().equals(LinkType.PARENT)) {
                linkCounter++;
                assertTrue(href.startsWith(_appUri.toString() + "/workspace/vdbs"));
            } else if (link.getRel().equals(LinkType.CHILDREN)) {
                linkCounter++;
                assertTrue(href.startsWith(_appUri.toString() + "/workspace/search"));
            } else {
                assertTrue(href.startsWith(_appUri.toString() + "/workspace/vdbs"));

                String suffixPrefix = TestUtilities.PORTFOLIO_VDB_NAME + FORWARD_SLASH;

                if (link.getRel().equals(LinkType.IMPORTS)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.IMPORTS.uriName()));
                } else if (link.getRel().equals(LinkType.MODELS)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.MODELS.uriName()));
                } else if (link.getRel().equals(LinkType.TRANSLATORS)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.TRANSLATORS.uriName()));
                } else if (link.getRel().equals(LinkType.DATA_ROLES)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.DATA_ROLES.uriName()));
                }
            }
        }

        assertEquals(7, linkCounter);
    }

}
