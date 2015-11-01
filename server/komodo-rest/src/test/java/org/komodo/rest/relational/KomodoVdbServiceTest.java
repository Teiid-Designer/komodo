/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest.relational;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.lang.reflect.Field;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.util.List;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.Invocation.Builder;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;
import org.jboss.resteasy.plugins.server.undertow.UndertowJaxrsServer;
import org.jboss.resteasy.test.TestPortProvider;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.core.KEngine;
import org.komodo.relational.model.Model.Type;
import org.komodo.rest.KomodoRestV1Application;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.RestProperty;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.TestUtilities;

@SuppressWarnings( {"javadoc", "nls"} )
public final class KomodoVdbServiceTest implements StringConstants {

    private static Path _kengineDataDir;
    private static KomodoRestV1Application _restApp;
    private static UndertowJaxrsServer _server;
    private static KomodoRestUriBuilder _uriBuilder;

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
    }

    @BeforeClass
    public static void beforeAll() throws Exception {
        _kengineDataDir = Files.createTempDirectory(null, new FileAttribute[0]);
        System.setProperty(SystemConstants.ENGINE_DATA_DIR, _kengineDataDir.toString());

        _server = new UndertowJaxrsServer().start();

        _restApp = new KomodoRestV1Application();
        _server.deploy(_restApp);

        final URI baseUri = URI.create(TestPortProvider.generateBaseUrl());
        final URI appUri = UriBuilder.fromUri(baseUri).path("/v1").build();
        _uriBuilder = new KomodoRestUriBuilder(appUri);
    }

    private Client client;
    private Response response;

    @Rule
    public TestName testName = new TestName();

    @After
    public void afterEach() throws Exception {
        if (this.response != null) {
            this.response.close();
        }

        this.client.close();

        _restApp.clearRepository();
    }

    @Before
    public void beforeEach() {
        this.client = ClientBuilder.newClient();
    }

    private void loadVdbs() throws Exception {
        _restApp.importVdb(TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME, TestUtilities.allElementsExample());
        _restApp.importVdb(TestUtilities.PORTFOLIO_VDB_NAME, TestUtilities.portfolioExample());
        _restApp.importVdb(TestUtilities.PARTS_VDB_NAME, TestUtilities.partsExample());
        _restApp.importVdb(TestUtilities.TWEET_EXAMPLE_NAME, TestUtilities.tweetExample());

        Assert.assertEquals(4, _restApp.getVdbs().length);
    }

    private RestVdb[] createVdbs(final int numVdbsToCreate) {
        final RestVdb[] result = new RestVdb[numVdbsToCreate];

        //        for ( int i = 0; i < numVdbsToCreate; ++i ) {
        //            final String vdbName = ( this.testName.getMethodName() + '_' + ( i + 1 ) );
        //            final String description = vdbName + " description goes here";
        //            final String extPath = "/vdbs/" + vdbName + ".xml";
        //
        //            final RestVdb restVdb = new RestVdb( vdbName );
        //            restVdb.setDescription( description );
        //            restVdb.setOriginalFilePath( extPath );
        //            result[ i ] = restVdb;
        //
        //            final String input = KomodoJsonMarshaller.marshall( restVdb );
        //            this.response = request( _uriBuilder.getVdbsUri() ).post( Entity.json( input ) );
        //            assertThat( this.response.getStatus(), is( Status.OK.getStatusCode() ) );
        //
        //            // make sure the VDB descriptor JSON document is returned
        //            final String entity = this.response.readEntity( String.class );
        //            assertThat( entity, is( notNullValue() ) );
        //
        //            final RestVdbDescriptor descriptor = KomodoJsonMarshaller.unmarshall( entity, RestVdbDescriptor.class );
        //            assertThat( descriptor.getName(), is( restVdb.getName() ) );
        //            assertThat( descriptor.getDescription(), is( restVdb.getDescription() ) );
        //            assertThat( descriptor.getLinks().length, is( 4 ) );
        //
        //            this.response.close(); // must close before making another request
        //        }

        return result;
    }

    protected Invocation.Builder request(final URI uri) {
        return this.client.target(uri.toString()).request();
    }

    private void assertPortfolio(RestVdb vdb) {
        assertEquals("/tko__komodo/tko__workspace/Portfolio", vdb.getDataPath());
        assertEquals(KomodoType.VDB, vdb.getkType());
        assertTrue(vdb.hasChildren());
        assertEquals(TestUtilities.PORTFOLIO_VDB_NAME, vdb.getName());
        assertEquals("The Portfolio Dynamic VDB", vdb.getDescription());
        assertEquals("/tko:komodo/tko:workspace/Portfolio", vdb.getOriginalFilePath());
        assertFalse(vdb.isPreview());
        assertEquals("BY_VERSION", vdb.getConnectionType());
        assertEquals(1, vdb.getVersion());

        List<RestProperty> properties = vdb.getProperties();
        assertEquals(1, properties.size());
        RestProperty property = properties.iterator().next();
        assertEquals("UseConnectorMetadata", property.getName());
        assertEquals("true", property.getValue());

        List<RestLink> links = vdb.getLinks();
        assertEquals(6, links.size());

        int linkCounter = 0;
        for (RestLink link : links) {
            String href = link.getHref().toString();
            assertTrue(href.startsWith("http://localhost:8081/v1/workspace/vdbs"));

            if (link.getRel().equals(LinkType.SELF)) {
                linkCounter++;
                assertTrue(href.endsWith(TestUtilities.PORTFOLIO_VDB_NAME));
            } else
                if (link.getRel().equals(LinkType.PARENT)) {
                linkCounter++;
            } else {
                String suffixPrefix = TestUtilities.PORTFOLIO_VDB_NAME + FORWARD_SLASH;

                if (link.getRel().equals(LinkType.IMPORTS)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.IMPORTS.uriName()));
                } else
                    if (link.getRel().equals(LinkType.MODELS)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.MODELS.uriName()));
                } else
                        if (link.getRel().equals(LinkType.TRANSLATORS)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.TRANSLATORS.uriName()));
                } else
                            if (link.getRel().equals(LinkType.DATA_ROLES)) {
                    linkCounter++;
                    assertTrue(href.endsWith(suffixPrefix + LinkType.DATA_ROLES.uriName()));
                }
            }
        }

        assertEquals(6, linkCounter);
    }

    @Test
    public void shouldGetVdbs() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbsUri();
        this.response = request(uri).get();
        assertTrue(response.hasEntity());

        final String entities = response.readEntity(String.class);
        assertThat(entities, is(notNullValue()));

        //System.out.println("Response:\n" + entities);
        // make sure the VDB JSON document is returned for each vdb
        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entities, RestVdb[].class);
        assertEquals(4, vdbs.length);

        boolean foundPortfolio = false;
        for (RestVdb vdb : vdbs) {
            assertNotNull(vdb.getId());
            assertNotNull(vdb.getDataPath());
            assertNotNull(vdb.getkType());

            if (TestUtilities.PORTFOLIO_VDB_NAME.equals(vdb.getId())) {
                foundPortfolio = true;
                assertPortfolio(vdb);
            }
        }

        assertTrue(foundPortfolio);

    }

    @Test
    public void shouldNotGetVdbsXml() throws Exception {
        loadVdbs();

        URI uri = _uriBuilder.generateVdbsUri();
        Builder request = this.client.target(uri.toString()).request(MediaType.APPLICATION_XML);
        this.response = request.get();

        assertTrue(response.hasEntity());

        //
        // Internal server error since the server does not support
        // '/vdbs' url returning anything in xml
        //
        assertEquals(500, response.getStatus());

        final String entity = response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        assertEquals("No match for accept header", entity);
    }

    //    @Test
    //    public void shouldDeleteVdb() throws Exception {
    //        final RestVdb restVdb = createVdbs( 1 )[ 0 ];
    //        this.response = request( _uriBuilder.buildVdbUri( LinkType.DELETE, restVdb.getName() ) ).delete();
    //        assertThat( this.response.getStatus(), is( Status.NO_CONTENT.getStatusCode() ) );
    //    }

    @Test
    public void shouldGetVdb() throws Exception {
        loadVdbs();

        // get
        this.response = request(_uriBuilder.buildVdbUri(LinkType.SELF, TestUtilities.PORTFOLIO_VDB_NAME)).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdb vdb = KomodoJsonMarshaller.unmarshall(entity, RestVdb.class);
        assertNotNull(vdb);

        assertPortfolio(vdb);
    }

    @Test
    public void shouldGetVdbXml() throws Exception {
        loadVdbs();

        URI uri = _uriBuilder.buildVdbUri(LinkType.SELF, TestUtilities.PORTFOLIO_VDB_NAME);
        Builder request = this.client.target(uri.toString()).request(MediaType.APPLICATION_XML);
        this.response = request.get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);
        assertTrue(entity.contains("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
        assertTrue(entity.contains("<vdb name=\"Portfolio\" version=\"1\">"));
        assertTrue(entity.contains("</vdb>"));
    }

    @Test
    public void shouldGetVdbWhenPatternMatches() throws Exception {
        loadVdbs();

        final String pattern = STAR + "P" + STAR;
        this.response = request(UriBuilder.fromUri(_uriBuilder.generateVdbsUri()).queryParam(KomodoVdbService.QueryParamKeys.PATTERN, pattern).build()).get();

        final String entities = response.readEntity(String.class);
        assertThat(entities, is(notNullValue()));

        //System.out.println("Response:\n" + entities);
        // make sure the VDB JSON document is returned for each vdb
        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entities, RestVdb[].class);
        assertEquals(2, vdbs.length);

        // Should return both MyPartsVDB_Dynamic and Portfoliio
        for (RestVdb vdb : vdbs) {
            if (TestUtilities.PARTS_VDB_NAME.equals(vdb.getName())) {
                // Not checking this one
            } else
                if (TestUtilities.PORTFOLIO_VDB_NAME.equals(vdb.getName()))
                    assertPortfolio(vdb);
            else
                fail("Invalid VDB returned from search pattern " + pattern);
        }
    }

    @Test
    public void shouldLimitNumberOfVdbsReturnedWhenUsingSizeQueryParameter() throws Exception {
        loadVdbs();

        final int resultSize = 3;

        this.response = request(UriBuilder.fromUri(_uriBuilder.generateVdbsUri()).queryParam(KomodoVdbService.QueryParamKeys.SIZE, resultSize).build()).get();
        final String entities = response.readEntity(String.class);
        assertThat(entities, is(notNullValue()));

        //System.out.println("Response:\n" + entities);
        // make sure the VDB JSON document is returned for each vdb
        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entities, RestVdb[].class);
        assertEquals(resultSize, vdbs.length);
    }

    @Test
    public void shouldLimitNumberOfVdbsReturnedWhenUsingStartQueryParameter() throws Exception {
        loadVdbs();

        final int start = 3;

        this.response = request(UriBuilder.fromUri(_uriBuilder.generateVdbsUri()).queryParam(KomodoVdbService.QueryParamKeys.START, start).build()).get();
        final String entities = response.readEntity(String.class);
        assertThat(entities, is(notNullValue()));

        //System.out.println("Response:\n" + entities);
        // make sure the VDB JSON document is returned for each vdb
        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entities, RestVdb[].class);
        assertEquals(1, vdbs.length);
    }

    //
    //    @Test
    //    public void shouldNotDeleteVdb() throws Exception {
    //        this.response = request( _uriBuilder.buildVdbUri( LinkType.DELETE, "vdbDoesNotExist" ) ).delete();
    //        assertThat( this.response.getStatus(), is( Status.NOT_FOUND.getStatusCode() ) );
    //    }

    @Test
    public void shouldNotFindVdb() throws Exception {
        this.response = request(UriBuilder.fromUri(_uriBuilder.generateVdbsUri()).path("blah").build()).get();
        assertThat(this.response.getStatus(), is(Status.NOT_FOUND.getStatusCode()));
    }

    @Test
    public void shouldReturnEmptyListWhenNoVdbsInWorkspace() {
        this.response = request(_uriBuilder.generateVdbsUri()).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entity, RestVdb[].class);
        assertNotNull(vdbs);
        assertEquals(0, vdbs.length);
    }

    @Test
    public void shouldGetVdbModels() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbChildGroupUri(TestUtilities.PORTFOLIO_VDB_NAME, LinkType.MODELS);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbModel[] models = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbModel[].class);
        assertNotNull(models);
        assertEquals(5, models.length);
        for (RestVdbModel model : models) {
            assertEquals(KomodoType.MODEL, model.getkType());
            assertEquals(true, model.isVisible());

            if ("MarketData".equals(model.getId())) {
                assertEquals(Type.PHYSICAL, model.getModelType());
            } else
                if ("Accounts".equals(model.getId())) {
                assertEquals(Type.PHYSICAL, model.getModelType());
            } else
                    if ("PersonalValuations".equals(model.getId())) {
                assertEquals(Type.PHYSICAL, model.getModelType());
            } else
                        if ("Stocks".equals(model.getId())) {
                assertEquals(Type.VIRTUAL, model.getModelType());
            } else
                            if ("StocksMatModel".equals(model.getId())) {
                assertEquals(Type.VIRTUAL, model.getModelType());
            } else
                fail("Model has invalid id");

            List<RestLink> links = model.getLinks();
            assertEquals(3, links.size());
        }
    }

    @Test
    public void shouldGetVdbModel() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbModelUri(LinkType.SELF, TestUtilities.PORTFOLIO_VDB_NAME, "PersonalValuations");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbModel model = KomodoJsonMarshaller.unmarshall(entity, RestVdbModel.class);
        assertNotNull(model);

        assertEquals("PersonalValuations", model.getId());
        assertEquals(KomodoType.MODEL, model.getkType());
        assertEquals(true, model.isVisible());
        assertEquals(Type.PHYSICAL, model.getModelType());

        List<RestLink> links = model.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbModelSources() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbModelUri(LinkType.SOURCES, TestUtilities.PORTFOLIO_VDB_NAME, "PersonalValuations");
        this.response = request(uri).get();
        final String entities = this.response.readEntity(String.class);
        assertThat(entities, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entities);

        RestVdbModelSource[] sources = KomodoJsonMarshaller.unmarshallArray(entities, RestVdbModelSource[].class);
        assertEquals(1, sources.length);

        RestVdbModelSource source = sources[0];

        assertEquals("excelconnector", source.getId());
        assertEquals(KomodoType.VDB_MODEL_SOURCE, source.getkType());
        assertEquals("java:/excel-file", source.getJndiName());
        assertEquals("excel", source.getTranslator());

        List<RestLink> links = source.getLinks();
        assertEquals(2, links.size());
    }

    @Test
    public void shouldGetVdbModelSource() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbModelSourceUri(LinkType.SELF, TestUtilities.PORTFOLIO_VDB_NAME, "PersonalValuations", "excelconnector");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbModelSource source = KomodoJsonMarshaller.unmarshall(entity, RestVdbModelSource.class);
        assertNotNull(source);

        assertEquals("excelconnector", source.getId());
        assertEquals(KomodoType.VDB_MODEL_SOURCE, source.getkType());
        assertEquals("java:/excel-file", source.getJndiName());
        assertEquals("excel", source.getTranslator());

        List<RestLink> links = source.getLinks();
        assertEquals(2, links.size());
    }

    @Test
    public void shouldGetVdbTranslators() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbChildGroupUri(TestUtilities.TWEET_EXAMPLE_NAME, LinkType.TRANSLATORS);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbTranslator[] translators = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbTranslator[].class);
        assertNotNull(translators);
        assertEquals(1, translators.length);

        RestVdbTranslator translator = translators[0];

        assertEquals(KomodoType.VDB_TRANSLATOR, translator.getkType());
        assertEquals("ws", translator.getType());
        assertEquals("Rest Web Service translator", translator.getDescription());

        List<RestLink> links = translator.getLinks();
        assertEquals(2, links.size());
    }

    @Test
    public void shouldGetVdbTranslatorsEmptyList() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbChildGroupUri(TestUtilities.PORTFOLIO_VDB_NAME, LinkType.TRANSLATORS);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbTranslator[] translators = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbTranslator[].class);
        assertNotNull(translators);
        assertEquals(0, translators.length);
    }

    @Test
    public void shouldGetVdbTranslator() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbTranslatorUri(LinkType.SELF, TestUtilities.TWEET_EXAMPLE_NAME, "rest");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbTranslator translator = KomodoJsonMarshaller.unmarshall(entity, RestVdbTranslator.class);
        assertNotNull(translator);

        assertEquals(KomodoType.VDB_TRANSLATOR, translator.getkType());
        assertEquals("ws", translator.getType());
        assertEquals("Rest Web Service translator", translator.getDescription());

        List<RestLink> links = translator.getLinks();
        assertEquals(2, links.size());
    }

    @Test
    public void shouldGetVdbImports() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbChildGroupUri(TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME, LinkType.IMPORTS);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbImport[] imports = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbImport[].class);
        assertNotNull(imports);
        assertEquals(1, imports.length);

        RestVdbImport vdbImport = imports[0];

        assertEquals(KomodoType.VDB_IMPORT, vdbImport.getkType());
        assertEquals("x", vdbImport.getName());
        assertEquals(2, vdbImport.getVersion());
        assertEquals(false, vdbImport.isImportDataPolicies());

        List<RestLink> links = vdbImport.getLinks();
        assertEquals(2, links.size());
    }

    @Test
    public void shouldGetVdbImportsEmptyList() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbChildGroupUri(TestUtilities.PORTFOLIO_VDB_NAME, LinkType.IMPORTS);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbImport[] imports = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbImport[].class);
        assertNotNull(imports);
        assertEquals(0, imports.length);
    }

    @Test
    public void shouldGetVdbImport() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbImportUri(LinkType.SELF, TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME, "x");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbImport vdbImport = KomodoJsonMarshaller.unmarshall(entity, RestVdbImport.class);
        assertNotNull(vdbImport);

        assertEquals(KomodoType.VDB_IMPORT, vdbImport.getkType());
        assertEquals("x", vdbImport.getName());
        assertEquals(2, vdbImport.getVersion());
        assertEquals(false, vdbImport.isImportDataPolicies());

        List<RestLink> links = vdbImport.getLinks();
        assertEquals(2, links.size());
    }

    @Test
    public void shouldGetVdbDataRoles() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbChildGroupUri(TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME, LinkType.DATA_ROLES);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbDataRole[] dataRoles = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbDataRole[].class);
        assertNotNull(dataRoles);
        assertEquals(1, dataRoles.length);

        RestVdbDataRole dataRole = dataRoles[0];

        assertEquals(KomodoType.VDB_DATA_ROLE, dataRole.getkType());
        assertEquals("roleOne", dataRole.getName());
        assertEquals("roleOne described", dataRole.getDescription());
        assertEquals(true, dataRole.isAllowCreateTempTables());
        assertEquals(false, dataRole.isAnyAuthenticated());
        assertEquals(true, dataRole.isGrantAll());
        assertEquals(2, dataRole.getMappedRoles().length);
        assertTrue(dataRole.getMappedRoles()[0].equals("ROLE1") || dataRole.getMappedRoles()[0].equals("ROLE2"));
        assertTrue(dataRole.getMappedRoles()[1].equals("ROLE1") || dataRole.getMappedRoles()[1].equals("ROLE2"));

        List<RestLink> links = dataRole.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbDataRolesEmptyList() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbChildGroupUri(TestUtilities.PORTFOLIO_VDB_NAME, LinkType.DATA_ROLES);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbDataRole[] dataRoles = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbDataRole[].class);
        assertNotNull(dataRoles);
        assertEquals(0, dataRoles.length);
    }

    @Test
    public void shouldGetVdbDataRole() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbDataRoleUri(LinkType.SELF, TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME, "roleOne");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbDataRole dataRole = KomodoJsonMarshaller.unmarshall(entity, RestVdbDataRole.class);
        assertNotNull(dataRole);

        assertEquals(KomodoType.VDB_DATA_ROLE, dataRole.getkType());
        assertEquals("roleOne", dataRole.getName());
        assertEquals("roleOne described", dataRole.getDescription());
        assertEquals(true, dataRole.isAllowCreateTempTables());
        assertEquals(false, dataRole.isAnyAuthenticated());
        assertEquals(true, dataRole.isGrantAll());
        assertEquals(2, dataRole.getMappedRoles().length);
        assertTrue(dataRole.getMappedRoles()[0].equals("ROLE1") || dataRole.getMappedRoles()[0].equals("ROLE2"));
        assertTrue(dataRole.getMappedRoles()[1].equals("ROLE1") || dataRole.getMappedRoles()[1].equals("ROLE2"));

        List<RestLink> links = dataRole.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbPermissions() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbDataRoleUri(LinkType.PERMISSIONS, TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME, "roleOne");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbPermission[] permissions = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbPermission[].class);
        assertNotNull(permissions);
        assertEquals(4, permissions.length);

        for (RestVdbPermission permission : permissions) {
            assertEquals(KomodoType.VDB_PERMISSION, permission.getkType());
            List<RestLink> links = permission.getLinks();
            assertEquals(4, links.size());

            if (permission.getId().equals("myTable.T1")) {
                assertFalse(permission.hasChildren());
                assertFalse(permission.isAllowAlter());
                assertFalse(permission.isAllowCreate());
                assertFalse(permission.isAllowDelete());
                assertFalse(permission.isAllowExecute());
                assertFalse(permission.isAllowLanguage());
                assertTrue(permission.isAllowRead());
                assertFalse(permission.isAllowUpdate());

            } else
                if (permission.getId().equals("myTable.T2")) {
                assertTrue(permission.hasChildren());
                assertTrue(permission.isAllowAlter());
                assertTrue(permission.isAllowCreate());
                assertTrue(permission.isAllowDelete());
                assertTrue(permission.isAllowExecute());
                assertFalse(permission.isAllowLanguage());
                assertFalse(permission.isAllowRead());
                assertTrue(permission.isAllowUpdate());

            } else
                    if (permission.getId().equals("myTable.T2.col1")) {
                assertTrue(permission.hasChildren());
                assertFalse(permission.isAllowAlter());
                assertFalse(permission.isAllowCreate());
                assertFalse(permission.isAllowDelete());
                assertFalse(permission.isAllowExecute());
                assertFalse(permission.isAllowLanguage());
                assertFalse(permission.isAllowRead());
                assertFalse(permission.isAllowUpdate());

            } else
                        if (permission.getId().equals("javascript")) {
                assertFalse(permission.hasChildren());
                assertFalse(permission.isAllowAlter());
                assertFalse(permission.isAllowCreate());
                assertFalse(permission.isAllowDelete());
                assertFalse(permission.isAllowExecute());
                assertTrue(permission.isAllowLanguage());
                assertFalse(permission.isAllowRead());
                assertFalse(permission.isAllowUpdate());
            }
        }
    }

    @Test
    public void shouldGetVdbPermission() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbPermissionUri(LinkType.SELF,
                                                    TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME,
                                                    "roleOne", "myTable.T1");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbPermission permission = KomodoJsonMarshaller.unmarshall(entity, RestVdbPermission.class);
        assertNotNull(permission);

        assertEquals(KomodoType.VDB_PERMISSION, permission.getkType());
        assertEquals("myTable.T1", permission.getId());
        assertFalse(permission.hasChildren());
        assertFalse(permission.isAllowAlter());
        assertFalse(permission.isAllowCreate());
        assertFalse(permission.isAllowDelete());
        assertFalse(permission.isAllowExecute());
        assertFalse(permission.isAllowLanguage());
        assertTrue(permission.isAllowRead());
        assertFalse(permission.isAllowUpdate());

        List<RestLink> links = permission.getLinks();
        assertEquals(4, links.size());
    }

    @Test
    public void shouldGetVdbConditions() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbPermissionUri(LinkType.CONDITIONS,
                                                  TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME,
                                                  "roleOne", "myTable.T2");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbCondition[] conditions = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbCondition[].class);
        assertNotNull(conditions);
        assertEquals(1, conditions.length);

        RestVdbCondition condition = conditions[0];
        assertEquals("col1 = user()", condition.getName());
        assertFalse(condition.isConstraint());

        List<RestLink> links = condition.getLinks();
        assertEquals(2, links.size());
    }

    @Test
    public void shouldGetVdbCondition() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbPermissionChildUri(LinkType.SELF,
                                                  TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME,
                                                  "roleOne", "myTable.T2", LinkType.CONDITIONS, "col1 = user()");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbCondition condition = KomodoJsonMarshaller.unmarshall(entity, RestVdbCondition.class);
        assertNotNull(condition);

        assertEquals("col1 = user()", condition.getName());
        assertFalse(condition.isConstraint());

        List<RestLink> links = condition.getLinks();
        assertEquals(2, links.size());
    }

    @Test
    public void shouldGetVdbMasks() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbPermissionUri(LinkType.MASKS,
                                                  TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME,
                                                  "roleOne", "myTable.T2.col1");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbMask[] masks = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbMask[].class);
        assertNotNull(masks);
        assertEquals(1, masks.length);

        RestVdbMask mask = masks[0];
        assertEquals("col2", mask.getName());
        assertEquals("1", mask.getOrder());

        List<RestLink> links = mask.getLinks();
        assertEquals(2, links.size());
    }

    @Test
    public void shouldGetVdbMask() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.buildVdbPermissionChildUri(LinkType.SELF,
                                                  TestUtilities.ALL_ELEMENTS_EXAMPLE_NAME,
                                                  "roleOne", "myTable.T2.col1", LinkType.MASKS, "col2");
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbMask mask = KomodoJsonMarshaller.unmarshall(entity, RestVdbMask.class);
        assertNotNull(mask);

        assertEquals("col2", mask.getName());
        assertEquals("1", mask.getOrder());

        List<RestLink> links = mask.getLinks();
        assertEquals(2, links.size());
    }
}
