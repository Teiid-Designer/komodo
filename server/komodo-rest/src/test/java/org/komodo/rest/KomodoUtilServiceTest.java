/*
* JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.net.URI;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.util.Map;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.core.Response;
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
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.KomodoRestUriBuilder;
import org.komodo.rest.relational.KomodoStatusObject;
import org.komodo.rest.relational.RelationalMessages;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.constants.SystemConstants;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.TestUtilities;

@SuppressWarnings( {"javadoc", "nls"} )
public final class KomodoUtilServiceTest implements StringConstants {

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

    private void loadSamples() throws Exception {
        _restApp.importVdb(KomodoUtilService.getVdbSample("parts_dynamic-vdb.xml"));
        _restApp.importVdb(KomodoUtilService.getVdbSample("portfolio-vdb.xml"));
        _restApp.importVdb(KomodoUtilService.getVdbSample("teiid-vdb-all-elements.xml"));
        _restApp.importVdb(KomodoUtilService.getVdbSample("tweet-example-vdb.xml"));

        Assert.assertEquals(4, _restApp.getVdbs().length);
    }

    protected Invocation.Builder request(final URI uri) {
        return this.client.target(uri.toString()).request();
    }

    @Test
    public void shouldAbout() throws Exception {
        String[] EXPECTED = {
            OPEN_BRACE + NEW_LINE,
            "  \"Information\": " +  OPEN_BRACE + NEW_LINE,
            "    \"Repository Workspace\": \"komodoLocalWorkspace\"," + NEW_LINE,
            "    \"Repository Configuration\"", // Configuration Url contains local file names so impossible to test
            "    \"Repository Vdb Total\": \"4\"" + NEW_LINE,
            "  " + CLOSE_BRACE + NEW_LINE };

        loadSamples();

        // get
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.ABOUT).build();
        this.response = request(uri).get();
        assertTrue(response.hasEntity());

        final String entity = response.readEntity(String.class);
        //System.out.println("Response from uri " + uri + ":\n" + entity);
        for (String expected : EXPECTED) {
            assertTrue(entity.contains(expected));
        }
    }

    @Test
    public void shouldReturnSwaggerSpec() throws Exception {

        // get
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                                    .path("swagger.json").build();
        this.response = request(uri).get();
        assertTrue(response.hasEntity());

        final String entity = response.readEntity(String.class);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        assertTrue(entity.contains("\"swagger\" : \"2.0\""));
        assertTrue(entity.contains("\"/service/about\""));
        assertTrue(entity.contains("\"/service/samples\""));
        assertTrue(entity.contains("\"/service/schema\""));
        assertTrue(entity.contains("\"/workspace/vdbs\""));
        assertTrue(entity.contains("\"/workspace/vdbs/{vdbName}\""));
        assertTrue(entity.contains("\"keng__id\""));
        assertTrue(entity.contains("\"keng__kType\""));
    }

    @Test
    public void shouldLoadSampleData() throws Exception {

        // get
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.SAMPLE_DATA).build();
        this.response = request(uri).post(null);

        final String entity = response.readEntity(String.class);
        System.out.println("Response from uri " + uri + ":\n" + entity);

        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        assertEquals("Sample Vdb Import", status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        for (String sample : KomodoUtilService.SAMPLES) {
            String message = attributes.get(sample);
            assertNotNull(message);
            assertEquals(
                         RelationalMessages.getString(RelationalMessages.Error.VDB_SAMPLE_IMPORT_SUCCESS, sample),
                         message);
        }
    }

    @Test
    public void shouldLoadSamplesDataAlreadyExists() throws Exception {
        loadSamples();

        // get
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.SAMPLE_DATA).build();
        this.response = request(uri).post(null);

        final String entity = response.readEntity(String.class);
        System.out.println("Response from uri " + uri + ":\n" + entity);

        KomodoStatusObject status = KomodoJsonMarshaller.unmarshall(entity, KomodoStatusObject.class);
        assertNotNull(status);

        assertEquals("Sample Vdb Import", status.getTitle());
        Map<String, String> attributes = status.getAttributes();

        for (String sample : KomodoUtilService.SAMPLES) {
            String message = attributes.get(sample);
            assertNotNull(message);

            assertEquals(
                         RelationalMessages.getString(
                                                      RelationalMessages.Error.VDB_SAMPLE_IMPORT_VDB_EXISTS,
                                                      sample), message);
        }
    }

    @Test
    public void shouldReturnTeiidSchema() throws Exception {

        // get
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.SCHEMA_SEGMENT)
                                                    .build();
        this.response = request(uri).get();
        assertTrue(response.hasEntity());

        final String entity = response.readEntity(String.class);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        InputStream schemaStream = getClass().getResourceAsStream("teiid-schema.json");
        String expected = TestUtilities.toString(schemaStream);

        assertEquals(expected, entity);
    }

    @Test
    public void shouldReturnTeiidSchemaForKType() throws Exception {

        // get
        UriBuilder baseBuilder = UriBuilder.fromUri(_uriBuilder.baseUri())
                                                    .path(V1Constants.SERVICE_SEGMENT)
                                                    .path(V1Constants.SCHEMA_SEGMENT);

        // Get model
        URI uri = baseBuilder.clone()
                                         .queryParam(KomodoService.QueryParamKeys.KTYPE, KomodoType.MODEL)
                                         .build();
        this.response = request(uri).get();
        assertTrue(response.hasEntity());

        String entity = response.readEntity(String.class);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        assertFalse(entity.contains("\"schema-1\" : {"));
        assertFalse(entity.contains("\"keng__id\" : \"vdb\""));
        assertFalse(entity.contains("\"keng__id\" : \"importVdb\""));
        assertTrue(entity.contains("\"keng__id\" : \"model\""));
        assertFalse(entity.contains("\"keng__id\" : \"source\""));
        assertFalse(entity.contains("\"keng__id\" : \"metadata\""));
        assertFalse(entity.contains("\"keng__id\" : \"validationError\""));
        assertFalse(entity.contains("\"keng__id\" : \"translator\""));
        assertFalse(entity.contains("\"keng__id\" : \"dataRole\""));
        assertFalse(entity.contains("\"keng__id\" : \"permission\""));
        assertFalse(entity.contains("\"keng__id\" : \"condition\""));
        assertFalse(entity.contains("\"keng__id\" : \"mask\""));
        assertFalse(entity.contains("\"keng__id\" : \"entry\""));

        // Get data role
        uri = baseBuilder.clone()
                                  .queryParam(KomodoService.QueryParamKeys.KTYPE, KomodoType.VDB_DATA_ROLE)
                                  .build();
        this.response = request(uri).get();
        assertTrue(response.hasEntity());

        entity = response.readEntity(String.class);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        assertFalse(entity.contains("\"schema-1\" : {"));
        assertFalse(entity.contains("\"keng__id\" : \"vdb\""));
        assertFalse(entity.contains("\"keng__id\" : \"importVdb\""));
        assertFalse(entity.contains("\"keng__id\" : \"model\""));
        assertFalse(entity.contains("\"keng__id\" : \"source\""));
        assertFalse(entity.contains("\"keng__id\" : \"metadata\""));
        assertFalse(entity.contains("\"keng__id\" : \"validationError\""));
        assertFalse(entity.contains("\"keng__id\" : \"translator\""));
        assertTrue(entity.contains("\"keng__id\" : \"dataRole\""));
        assertFalse(entity.contains("\"keng__id\" : \"permission\""));
        assertFalse(entity.contains("\"keng__id\" : \"condition\""));
        assertFalse(entity.contains("\"keng__id\" : \"mask\""));
        assertFalse(entity.contains("\"keng__id\" : \"entry\""));

        // Get mask
        uri = baseBuilder.clone().
                                   queryParam(KomodoService.QueryParamKeys.KTYPE, KomodoType.VDB_MASK)
                                   .build();
        this.response = request(uri).get();
        assertTrue(response.hasEntity());

        entity = response.readEntity(String.class);
        //System.out.println("Response from uri " + uri + ":\n" + entity);

        assertFalse(entity.contains("\"schema-1\" : {"));
        assertFalse(entity.contains("\"keng__id\" : \"vdb\""));
        assertFalse(entity.contains("\"keng__id\" : \"importVdb\""));
        assertFalse(entity.contains("\"keng__id\" : \"model\""));
        assertFalse(entity.contains("\"keng__id\" : \"source\""));
        assertFalse(entity.contains("\"keng__id\" : \"metadata\""));
        assertFalse(entity.contains("\"keng__id\" : \"validationError\""));
        assertFalse(entity.contains("\"keng__id\" : \"translator\""));
        assertFalse(entity.contains("\"keng__id\" : \"dataRole\""));
        assertFalse(entity.contains("\"keng__id\" : \"permission\""));
        assertFalse(entity.contains("\"keng__id\" : \"condition\""));
        assertTrue(entity.contains("\"keng__id\" : \"mask\""));
        assertFalse(entity.contains("\"keng__id\" : \"entry\""));
    }
}
