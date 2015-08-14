/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.rest;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.junit.Assert.assertThat;
import javax.json.JsonObject;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;
import org.jboss.resteasy.plugins.server.undertow.UndertowJaxrsServer;
import org.jboss.resteasy.test.TestPortProvider;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

@SuppressWarnings( { "javadoc", "nls" } )
public final class KomodoVdbServiceTest {

    private static UndertowJaxrsServer _server;

    @BeforeClass
    public static void beforeAll() throws Exception {
        _server = new UndertowJaxrsServer().start();
        _server.deploy( new KomodoRestV1Application() );
    }

    @AfterClass
    public static void afterAll() throws Exception {
        _server.stop();
    }

    private Client client;

    @Before
    public void beforeEach() {
        this.client = ClientBuilder.newClient();
    }

    @After
    public void afterEach() {
        this.client.close();
    }

    protected WebTarget request( String url ) {
        return this.client.target( TestPortProvider.generateURL( url ) );
    }

    @Test
    public void shouldReturnEmptyResponseWhenNoVdbsInWorkspace() {
        final JsonObject result = request( "/komodo/v1/workspace/vdbs" ).request().get( JsonObject.class );
        assertThat( result, is( notNullValue() ) );
        assertThat( result.isEmpty(), is( true ) );
    }

    @Test
    public void shouldReturnVdbsResponse() throws Exception {
        final JsonObject result = request( "/komodo/v1/workspace/vdbs" ).request().get( JsonObject.class );
        assertThat( result, is( notNullValue() ) );
        assertThat( result.isEmpty(), is( true ) );
    }

}
