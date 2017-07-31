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

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.core.StringStartsWith.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;

import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.relational.model.Model.Type;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbCondition;
import org.komodo.rest.relational.response.RestVdbDataRole;
import org.komodo.rest.relational.response.RestVdbImport;
import org.komodo.rest.relational.response.RestVdbMask;
import org.komodo.rest.relational.response.RestVdbModel;
import org.komodo.rest.relational.response.RestVdbModelSource;
import org.komodo.rest.relational.response.RestVdbModelTable;
import org.komodo.rest.relational.response.RestVdbModelTableColumn;
import org.komodo.rest.relational.response.RestVdbPermission;
import org.komodo.rest.relational.response.RestVdbTranslator;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.TestUtilities;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings( {"javadoc", "nls"} )
public final class KomodoVdbServiceTest extends AbstractKomodoServiceTest {

    @Rule
    public TestName testName = new TestName();

    @Test
    public void shouldGetVdbs() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.workspaceVdbsUri();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        assertNotNull(response.getEntity());

        final String entities = response.getEntity();
        assertThat(entities, is(notNullValue()));

        // System.out.println("Response:\n" + entities);
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

        URI uri = _uriBuilder.workspaceVdbsUri();
        ClientRequest request = request(uri, MediaType.APPLICATION_XML_TYPE);
        ClientResponse<String> response = request.get(String.class);
        assertNotNull(response.getEntity());

        //
        // Internal server error since the server does not support
        // '/vdbs' url returning anything in xml
        //
        assertEquals(500, response.getStatus());

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        assertTrue(entity.contains("No match for accept header"));
    }

    //    @Test
    //    public void shouldDeleteVdb() throws Exception {
    //        final RestVdb restVdb = createVdbs( 1 )[ 0 ];
    //        response = request( _uriBuilder.buildVdbUri( LinkType.DELETE, restVdb.getName() ) ).delete();
    //        assertThat( response.getStatus(), is( Status.NO_CONTENT.getStatusCode() ) );
    //    }

    @Test
	public void shouldCreateModel() throws Exception {
        final String vdbName = "blah";

        { // create VDB first
            final Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
            _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
            final URI uri = _uriBuilder.vdbUri(LinkType.SELF, settings);

            final RestVdb vdb = new RestVdb();
            vdb.setName(vdbName);
            vdb.setDescription("blah VDB description");
            vdb.setOriginalFilePath("/Users/elvis/vdbs/blah.vdb");
            vdb.setPreview(false);
            vdb.setConnectionType("connType");
            vdb.setVersion(1);
            vdb.setId(vdbName);
            vdb.setkType(KomodoType.VDB);
            vdb.setDataPath("/tko:komodo/tko:workspace/user/blah");

            final String json = KomodoJsonMarshaller.marshall(vdb);
            final ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
            addBody(request, json);

            final ClientResponse<String> response = request.post(String.class);
            assertThat(response, is(notNullValue()));
            assertThat(response.getStatus(), is(Response.Status.OK.getStatusCode()));
        }

        final String modelName = "elvis";
        final String dataPath = "/tko:komodo/tko:workspace/user/blah/elvis";
        final String ddl = "CREATE VIEW Tweet AS select * FROM twitterview.getTweets;";
        final String description = "model description goes here";
        final String id = modelName;
        final KomodoType kType = KomodoType.MODEL;
        final String metadataType = "DDL";
        final Type modelType = Type.VIRTUAL;
        final boolean visible = true;

        final Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, modelName);
        final URI uri = _uriBuilder.vdbModelUri(LinkType.SELF, settings);

        { // create model
            final RestVdbModel inModel = new RestVdbModel();
            inModel.setDataPath(dataPath);
            inModel.setDdl(ddl);
            inModel.setDescription(description);
            inModel.setId(id);
            inModel.setkType(kType);
            inModel.setMetadataType(metadataType);
            inModel.setModelType(modelType);
            inModel.setVisible(visible);

            final String json = KomodoJsonMarshaller.marshall(inModel);
            final ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
            addBody(request, json);

            final ClientResponse<String> response = request.post(String.class);
            assertThat(response, is(notNullValue()));
            assertThat(response.getStatus(), is(Response.Status.OK.getStatusCode()));

            final String entity = response.getEntity();
            assertThat(entity, is(notNullValue()));

            final RestVdbModel outModel = KomodoJsonMarshaller.unmarshall(entity, RestVdbModel.class);
            assertNotNull(outModel);
            assertThat(outModel.getDataPath(), is(dataPath));
            assertThat(outModel.getDescription(), is(description));
            // can't check DDL because sequencer has not run yet
            assertThat(outModel.getId(), is(id));
            assertThat(outModel.getkType(), is(kType));
            assertThat(outModel.getMetadataType(), is(metadataType));
            assertThat(outModel.getModelType(), is(modelType));
            assertThat(outModel.isVisible(), is(visible));
        }

        { // get new model
            final ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
            final ClientResponse<String> response = request.get(String.class);
            assertThat(response.getStatus(), is(Status.OK.getStatusCode()));

            final String entity = response.getEntity();
            assertThat(entity, is(notNullValue()));

            final RestVdbModel newModel = KomodoJsonMarshaller.unmarshall(entity, RestVdbModel.class);
            assertNotNull(newModel);
            assertThat(newModel.getDataPath(), is(dataPath));
            assertThat(newModel.getDescription(), is(description));
            assertThat(newModel.getId(), is(id));
            assertThat(newModel.getkType(), is(kType));
            assertThat(newModel.getMetadataType(), is(metadataType));
            assertThat(newModel.getModelType(), is(modelType));
            assertThat(newModel.isVisible(), is(visible));

            // remove any formatting before comparing
            final String actual = newModel.getDdl().toLowerCase().replace('\n', ' ').trim();
            final String expected = ddl.toLowerCase().replace('\n', ' ').trim();
            assertThat(actual, is(expected));
        }
    }

    @Test
    public void shouldCreateModelSource() throws Exception {
        final String vdbName = "blah";

        { // create VDB
            final Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
            _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
            final URI uri = _uriBuilder.vdbUri(LinkType.SELF, settings);

            final RestVdb vdb = new RestVdb();
            vdb.setName(vdbName);
            vdb.setDescription("blah VDB description");
            vdb.setOriginalFilePath("/Users/elvis/vdbs/blah.vdb");
            vdb.setPreview(false);
            vdb.setConnectionType("connType");
            vdb.setVersion(1);
            vdb.setId(vdbName);
            vdb.setkType(KomodoType.VDB);
            vdb.setDataPath("/tko:komodo/tko:workspace/user/blah");

            final String json = KomodoJsonMarshaller.marshall(vdb);
            final ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
            addBody(request, json);

            final ClientResponse<String> response = request.post(String.class);
            assertThat(response, is(notNullValue()));
            assertThat(response.getStatus(), is(Response.Status.OK.getStatusCode()));
        }

        final String modelName = "elvis";

        { // create model
            final RestVdbModel model = new RestVdbModel();
            model.setDataPath("/tko:komodo/tko:workspace/user/blah/elvis");
            model.setDdl("CREATE VIEW Tweet AS select * FROM twitterview.getTweets;");
            model.setDescription("model description goes here");
            model.setId(modelName);
            model.setkType(KomodoType.MODEL);
            model.setMetadataType("DDL");
            model.setModelType(Type.VIRTUAL);
            model.setVisible(true);

            final Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
            _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
            _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, modelName);
            final URI uri = _uriBuilder.vdbModelUri(LinkType.SELF, settings);

            final String json = KomodoJsonMarshaller.marshall(model);
            final ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
            addBody(request, json);

            final ClientResponse<String> response = request.post(String.class);
            assertThat(response, is(notNullValue()));
            assertThat(response.getStatus(), is(Response.Status.OK.getStatusCode()));
        }

        final String modelSourceName = "sledge";
        final String id = modelSourceName;
        final String dataPath = "/tko:komodo/tko:workspace/user/blah/elvis/vdb:sources/sledge";
        final KomodoType kType = KomodoType.VDB_MODEL_SOURCE;
        final String jndiName = "myJndiName";
        final String translator = "myTranslator";

        final Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, modelName);
        _uriBuilder.addSetting(settings, SettingNames.SOURCE_NAME, modelSourceName);
        final URI uri = _uriBuilder.vdbModelSourceUri(LinkType.SELF, settings);

        { // create model source
            final RestVdbModelSource inModelSource = new RestVdbModelSource();
            inModelSource.setDataPath(dataPath);
            inModelSource.setId(id);
            inModelSource.setkType(kType);
            inModelSource.setJndiName(jndiName);
            inModelSource.setTranslator(translator);

            final String json = KomodoJsonMarshaller.marshall(inModelSource);
            final ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
            addBody(request, json);

            final ClientResponse<String> response = request.post(String.class);
            assertThat(response, is(notNullValue()));
            assertThat(response.getStatus(), is(Response.Status.OK.getStatusCode()));

            final String entity = response.getEntity();
            assertThat(entity, is(notNullValue()));

            final RestVdbModelSource outModelSource = KomodoJsonMarshaller.unmarshall(entity, RestVdbModelSource.class);
            assertNotNull(outModelSource);
            assertThat(outModelSource.getDataPath(), is(dataPath));
            assertThat(outModelSource.getId(), is(id));
            assertThat(outModelSource.getkType(), is(kType));
            assertThat(outModelSource.getJndiName(), is(jndiName));
            assertThat(outModelSource.getTranslator(), is(translator));
        }

        { // get new model source
            final ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
            final ClientResponse<String> response = request.get(String.class);
            assertThat(response.getStatus(), is(Status.OK.getStatusCode()));

            final String entity = response.getEntity();
            assertThat(entity, is(notNullValue()));

            final RestVdbModelSource newModelSource = KomodoJsonMarshaller.unmarshall(entity, RestVdbModelSource.class);
            assertNotNull(newModelSource);
            assertThat(newModelSource.getDataPath(), is(dataPath));
            assertThat(newModelSource.getId(), is(id));
            assertThat(newModelSource.getkType(), is(kType));
            assertThat(newModelSource.getJndiName(), is(jndiName));
            assertThat(newModelSource.getTranslator(), is(translator));
        }
    }

    @Test
    public void shouldCreateVdb() throws Exception {
        final String vdbName = "blah";
        final String description = "blah VDB description";
        final String originalFilePath = "/Users/elvis/vdbs/blah.vdb";
        final boolean preview = false;
        final String connectionType = "connType";
        final int version = 5;
        final String id = vdbName;
        final KomodoType kType = KomodoType.VDB;
        final String dataPath = "/tko:komodo/tko:workspace/user/blah";

        final Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        final URI uri = _uriBuilder.vdbUri(LinkType.SELF, settings);

        { // create
            final RestVdb inVdb = new RestVdb();
            inVdb.setName(vdbName);
            inVdb.setDescription(description);
            inVdb.setOriginalFilePath(originalFilePath);
            inVdb.setPreview(preview);
            inVdb.setConnectionType(connectionType);
            inVdb.setVersion(version);
            inVdb.setId(id);
            inVdb.setkType(kType);
            inVdb.setDataPath(dataPath);

            final String json = KomodoJsonMarshaller.marshall(inVdb);
            final ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
            addBody(request, json);

            final ClientResponse<String> response = request.post(String.class);
            assertThat(response, is(notNullValue()));
            assertThat(response.getStatus(), is(Response.Status.OK.getStatusCode()));

            final String entity = response.getEntity();
            assertThat(entity, is(notNullValue()));

            // verify response is new VDB
            final RestVdb outVdb = KomodoJsonMarshaller.unmarshall(entity, RestVdb.class);
            assertNotNull(outVdb);
            assertThat(outVdb.getName(), is(vdbName));
            assertThat(outVdb.getDescription(), is(description));
            assertThat(outVdb.getOriginalFilePath(), is(originalFilePath));
            assertThat(outVdb.isPreview(), is(preview));
            assertThat(outVdb.getConnectionType(), is(connectionType));
            assertThat(outVdb.getVersion(), is(version));
            assertThat(outVdb.getId(), is(id));
            assertThat(outVdb.getkType(), is(kType));
            assertThat(outVdb.getDataPath(), is(dataPath));
        }

        { // get new VDB
            final ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
            final ClientResponse<String> response = request.get(String.class);
            assertThat(response.getStatus(), is(Status.OK.getStatusCode()));

            final String entity = response.getEntity();
            assertThat(entity, is(notNullValue()));

            final RestVdb newVdb = KomodoJsonMarshaller.unmarshall(entity, RestVdb.class);
            assertNotNull(newVdb);
            assertThat(newVdb.getName(), is(vdbName));
            assertThat(newVdb.getDescription(), is(description));
            assertThat(newVdb.getOriginalFilePath(), is(originalFilePath));
            assertThat(newVdb.isPreview(), is(preview));
            assertThat(newVdb.getConnectionType(), is(connectionType));
            assertThat(newVdb.getVersion(), is(version));
            assertThat(newVdb.getId(), is(id));
            assertThat(newVdb.getkType(), is(kType));
            assertThat(newVdb.getDataPath(), is(dataPath));
        }
    }

    @Test
    public void shouldGetVdb() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());

        URI uri = _uriBuilder.vdbUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdb vdb = KomodoJsonMarshaller.unmarshall(entity, RestVdb.class);
        assertNotNull(vdb);

        assertPortfolio(vdb);
    }

    @Test
    public void shouldGetVdbXml() throws Exception {
        loadVdbs();

        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        URI uri = _uriBuilder.vdbUri(LinkType.SELF, settings);

        ClientRequest request = request(uri, MediaType.APPLICATION_XML_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        System.out.println("Response:\n" + entity);
        assertTrue(entity.contains("<?xml version="));
        assertTrue(entity.contains("encoding="));
        assertTrue(entity.contains("<vdb name=\"Portfolio\" version=\"1\">"));
        assertTrue(entity.contains("</vdb>"));
    }

    @Test
    public void shouldGetVdbWhenPatternMatches() throws Exception {
        loadVdbs();

        final String pattern = STAR + "P" + STAR;
        URI uri = UriBuilder.fromUri(_uriBuilder.workspaceVdbsUri())
                                          .queryParam(KomodoVdbService.QueryParamKeys.PATTERN, pattern)
                                          .build();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entities = response.getEntity();
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

        URI uri = UriBuilder.fromUri(_uriBuilder.workspaceVdbsUri())
                                            .queryParam(KomodoVdbService.QueryParamKeys.SIZE, resultSize)
                                            .build();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entities = response.getEntity();
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

        URI uri = UriBuilder.fromUri(_uriBuilder.workspaceVdbsUri())
                                        .queryParam(KomodoVdbService.QueryParamKeys.START, start)
                                        .build();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entities = response.getEntity();
        assertThat(entities, is(notNullValue()));

        //System.out.println("Response:\n" + entities);
        // make sure the VDB JSON document is returned for each vdb
        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entities, RestVdb[].class);
        assertEquals(1, vdbs.length);
    }

    //
    //    @Test
    //    public void shouldNotDeleteVdb() throws Exception {
    //        response = request( _uriBuilder.buildVdbUri( LinkType.DELETE, "vdbDoesNotExist" ) ).delete();
    //        assertThat( response.getStatus(), is( Status.NOT_FOUND.getStatusCode() ) );
    //    }

    @Test
    public void shouldNotFindVdb() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.workspaceVdbsUri()).path("blah").build();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        assertThat(response.getStatus(), is(Status.NOT_FOUND.getStatusCode()));
    }

    @Test
    public void shouldReturnEmptyListWhenNoVdbsInWorkspace() throws Exception {
        URI uri = _uriBuilder.workspaceVdbsUri();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
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
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.PORTFOLIO_VDB_NAME, LinkType.MODELS);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
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

            Collection<RestLink> links = model.getLinks();
            assertEquals(4, links.size());
        }
    }

    @Test
    public void shouldGetVdbModel() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PersonalValuations");
        URI uri = _uriBuilder.vdbModelUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        // System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbModel model = KomodoJsonMarshaller.unmarshall(entity, RestVdbModel.class);
        assertNotNull(model);

        assertEquals("PersonalValuations", model.getId());
        assertEquals(KomodoType.MODEL, model.getkType());
        assertEquals(true, model.isVisible());
        assertEquals(Type.PHYSICAL, model.getModelType());

        Collection<RestLink> links = model.getLinks();
        assertEquals(4, links.size());
    }

    @Test
    public void shouldGetVdbModelSources() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PersonalValuations");
        URI uri = _uriBuilder.vdbModelUri(LinkType.SOURCES, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entities = response.getEntity();
        assertThat(entities, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entities);

        RestVdbModelSource[] sources = KomodoJsonMarshaller.unmarshallArray(entities, RestVdbModelSource[].class);
        assertEquals(1, sources.length);

        RestVdbModelSource source = sources[0];

        assertEquals("excelconnector", source.getId());
        assertEquals(KomodoType.VDB_MODEL_SOURCE, source.getkType());
        assertEquals("java:/excel-file", source.getJndiName());
        assertEquals("excel", source.getTranslator());

        Collection<RestLink> links = source.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbModelTables() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PARTS_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PartsSS");
        URI modelUri = _uriBuilder.vdbModelUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelUri).path(LinkType.TABLES.uriName()).build();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entities = response.getEntity();
        assertThat(entities, is(notNullValue()));

        RestVdbModelTable[] tables = KomodoJsonMarshaller.unmarshallArray(entities, RestVdbModelTable[].class);
        assertEquals(5, tables.length);

        List<String> tableNames = new ArrayList<String>();
        for(RestVdbModelTable table : tables) {
        	tableNames.add(table.getId());
        }
        
        assertTrue(tableNames.contains("PARTS"));
        assertTrue(tableNames.contains("SHIP_VIA"));
        assertTrue(tableNames.contains("STATUS"));
        assertTrue(tableNames.contains("SUPPLIER"));
        assertTrue(tableNames.contains("SUPPLIER_PARTS"));
    }

    @Test
    public void shouldGetVdbModelTableColumns() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PARTS_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PartsSS");
        _uriBuilder.addSetting(settings, SettingNames.TABLE_NAME, "SUPPLIER_PARTS");
        URI modelTableUri = _uriBuilder.vdbModelTableUri(LinkType.SELF, settings);
        URI uri = UriBuilder.fromUri(modelTableUri).path(LinkType.COLUMNS.uriName()).build();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entities = response.getEntity();
        assertThat(entities, is(notNullValue()));

        RestVdbModelTableColumn[] columns = KomodoJsonMarshaller.unmarshallArray(entities, RestVdbModelTableColumn[].class);
        assertEquals(4, columns.length);

        List<String> columnNames = new ArrayList<String>();
        Map<String,String> columnTypeMap = new HashMap<String,String>();
        for(RestVdbModelTableColumn column : columns) {
        	columnNames.add(column.getId());
        	columnTypeMap.put(column.getId(), column.getDatatypeName());
        }
        
        assertTrue(columnNames.contains("SUPPLIER_ID"));
        assertTrue(columnTypeMap.get("SUPPLIER_ID").equalsIgnoreCase("string"));
        assertTrue(columnNames.contains("PART_ID"));
        assertTrue(columnTypeMap.get("PART_ID").equalsIgnoreCase("string"));
        assertTrue(columnNames.contains("QUANTITY"));
        assertTrue(columnTypeMap.get("QUANTITY").equalsIgnoreCase("bigdecimal"));
        assertTrue(columnNames.contains("SHIPPER_ID"));
        assertTrue(columnTypeMap.get("SHIPPER_ID").equalsIgnoreCase("bigdecimal"));
    }

    @Test
    public void shouldGetVdbModelSourcesIncludeReferenceForTranslator() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.TWEET_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "twitter");
        URI uri = _uriBuilder.vdbModelUri(LinkType.SOURCES, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entities = response.getEntity();
        assertThat(entities, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entities);

        RestVdbModelSource[] sources = KomodoJsonMarshaller.unmarshallArray(entities, RestVdbModelSource[].class);
        assertEquals(1, sources.length);

        RestVdbModelSource source = sources[0];

        assertEquals("twitter", source.getId());
        assertEquals(KomodoType.VDB_MODEL_SOURCE, source.getkType());
        assertEquals("java:/twitterDS", source.getJndiName());
        assertEquals("rest", source.getTranslator());

        Collection<RestLink> links = source.getLinks();
        assertEquals(4, links.size());

        RestLink refLink = null;
        for (RestLink link : links) {
            if (LinkType.REFERENCE.equals(link.getRel()))
                refLink = link;
        }
        assertNotNull(refLink);

        URI href = refLink.getHref();
        assertTrue(href.toString().endsWith("workspace/vdbs/twitter/VdbTranslators/rest"));
    }

    @Test
    public void shouldGetVdbModelSource() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PersonalValuations");
        _uriBuilder.addSetting(settings, SettingNames.SOURCE_NAME, "excelconnector");
        URI uri = _uriBuilder.vdbModelSourceUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbModelSource source = KomodoJsonMarshaller.unmarshall(entity, RestVdbModelSource.class);
        assertNotNull(source);

        assertEquals("excelconnector", source.getId());
        assertEquals(KomodoType.VDB_MODEL_SOURCE, source.getkType());
        assertEquals("java:/excel-file", source.getJndiName());
        assertEquals("excel", source.getTranslator());

        Collection<RestLink> links = source.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbTranslators() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.TWEET_EXAMPLE_VDB_NAME, LinkType.TRANSLATORS);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbTranslator[] translators = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbTranslator[].class);
        assertNotNull(translators);
        assertEquals(1, translators.length);

        RestVdbTranslator translator = translators[0];

        assertEquals(KomodoType.VDB_TRANSLATOR, translator.getkType());
        assertEquals("ws", translator.getType());
        assertEquals("Rest Web Service translator", translator.getDescription());

        Collection<RestLink> links = translator.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbTranslatorsEmptyList() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.PORTFOLIO_VDB_NAME, LinkType.TRANSLATORS);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
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
        String vdbName = TestUtilities.TWEET_EXAMPLE_VDB_NAME;
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, vdbName);

        URI parentUri = _uriBuilder.vdbUri(_uriBuilder.workspaceVdbsUri(), vdbName);

        _uriBuilder.addSetting(settings, SettingNames.PARENT_PATH, parentUri);
        _uriBuilder.addSetting(settings, SettingNames.TRANSLATOR_NAME, "rest");
        _uriBuilder.addSetting(settings, SettingNames.ADD_TRANSLATORS_SEGMENT, "true");
        URI uri = _uriBuilder.vdbTranslatorUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbTranslator translator = KomodoJsonMarshaller.unmarshall(entity, RestVdbTranslator.class);
        assertNotNull(translator);

        assertEquals(KomodoType.VDB_TRANSLATOR, translator.getkType());
        assertEquals("ws", translator.getType());
        assertEquals("Rest Web Service translator", translator.getDescription());

        Collection<RestLink> links = translator.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbImports() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME, LinkType.IMPORTS);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
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

        Collection<RestLink> links = vdbImport.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbImportsEmptyList() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.PORTFOLIO_VDB_NAME, LinkType.IMPORTS);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
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
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.IMPORT_NAME, "x");
        URI uri = _uriBuilder.vdbImportUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entity);

        RestVdbImport vdbImport = KomodoJsonMarshaller.unmarshall(entity, RestVdbImport.class);
        assertNotNull(vdbImport);

        assertEquals(KomodoType.VDB_IMPORT, vdbImport.getkType());
        assertEquals("x", vdbImport.getName());
        assertEquals(2, vdbImport.getVersion());
        assertEquals(false, vdbImport.isImportDataPolicies());

        Collection<RestLink> links = vdbImport.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldCreateDataRole() throws Exception {
        loadVdbs();

        final String dataRoleName = "MyDataRole";

        final Properties settings = _uriBuilder.createSettings( SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME );
        _uriBuilder.addSetting( settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri() );
        _uriBuilder.addSetting( settings, SettingNames.DATA_ROLE_ID, dataRoleName );
        final URI uri = _uriBuilder.vdbDataRoleUri( LinkType.SELF, settings );

        final boolean allowCreateTempTables = true;
        final boolean anyAuthenticated = true;
        final String dataPath = "/tko:komodo/tko:workspace/user/"
                                + TestUtilities.PORTFOLIO_VDB_NAME
                                + '/'
                                + VdbLexicon.Vdb.DATA_ROLES
                                + '/'
                                + dataRoleName;
        final String description = "My data role description";
        final boolean grantAll = true;
        final String[] mappedRoles = new String[] { "a", "b", "c", "d", "e" };
        final KomodoType type = KomodoType.VDB_DATA_ROLE;
        final String id = dataRoleName;

        final String permissionName = "MyPermission";
        final String permissionId = permissionName;
        final KomodoType permissionType = KomodoType.VDB_PERMISSION;
        final String permissionDataPath = dataPath + '/' + VdbLexicon.DataRole.PERMISSIONS + '/' + permissionName;
        final boolean allowAlter = true;
        final boolean allowCreate = false;
        final boolean allowDelete = true;
        final boolean allowExecute = false;
        final boolean allowLanguage = true;
        final boolean allowRead = false;
        final boolean allowUpdate = true;
        
        { // create data role
            final RestVdbDataRole inDataRole = new RestVdbDataRole();
            inDataRole.setAllowCreateTempTables( allowCreateTempTables );
            inDataRole.setAnyAuthenticated( anyAuthenticated );
            inDataRole.setDataPath( dataPath );
            inDataRole.setDescription( description );
            inDataRole.setGrantAll( grantAll );
            inDataRole.setId( id );
            inDataRole.setkType( type );
            inDataRole.setMappedRoles( mappedRoles );
            inDataRole.setName( dataRoleName );
            
            // add permission
            final RestVdbPermission permission = new RestVdbPermission();
            permission.setName( permissionName );
            permission.setAllowAlter( allowAlter );
            permission.setAllowCreate( allowCreate );
            permission.setAllowDelete( allowDelete );
            permission.setAllowExecute( allowExecute );
            permission.setAllowLanguage( allowLanguage );
            permission.setAllowRead( allowRead );
            permission.setAllowUpdate( allowUpdate );
            permission.setId( permissionId );
            permission.setkType( permissionType );
            permission.setDataPath( permissionDataPath );
            inDataRole.setPermissions( new RestVdbPermission[] { permission } );

            final String json = KomodoJsonMarshaller.marshall( inDataRole );
            final ClientRequest request = request( uri, MediaType.APPLICATION_JSON_TYPE );
            addBody( request, json );

            final ClientResponse< String > response = request.post( String.class );
            assertThat( response, is( notNullValue() ) );
            assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

            final String entity = response.getEntity();
            assertThat( entity, is( notNullValue() ) );

            final RestVdbDataRole outDataRole = KomodoJsonMarshaller.unmarshall( entity, RestVdbDataRole.class );
            assertNotNull( outDataRole );
            assertThat( outDataRole.isAllowCreateTempTables(), is( allowCreateTempTables ) );
            assertThat( outDataRole.isAnyAuthenticated(), is( anyAuthenticated ) );
            assertThat( outDataRole.getDataPath(), is( dataPath ) );
            assertThat( outDataRole.getDescription(), is( description ) );
            assertThat( outDataRole.isGrantAll(), is( grantAll ) );
            assertThat( outDataRole.getId(), is( id ) );
            assertThat( outDataRole.getkType(), is( type ) );
            assertThat( Arrays.equals( outDataRole.getMappedRoles(), mappedRoles ), is( true ) );
            assertThat( outDataRole.getName(), is( dataRoleName ) );
            
            assertThat( outDataRole.getPermissions().length, is( 1 ) );
            final RestVdbPermission outPermission = outDataRole.getPermissions()[0];
            assertThat( outPermission.getName(), is( permissionName ) );
            assertThat( outPermission.isAllowAlter(), is( allowAlter ) );
            assertThat( outPermission.isAllowCreate(), is( allowCreate ) );
            assertThat( outPermission.isAllowDelete(), is( allowDelete ) );
            assertThat( outPermission.isAllowExecute(), is( allowExecute ) );
            assertThat( outPermission.isAllowLanguage(), is( allowLanguage ) );
            assertThat( outPermission.isAllowRead(), is( allowRead ) );
            assertThat( outPermission.isAllowUpdate(), is( allowUpdate ) );
        }
        
        { // verify new data role exists
            final ClientRequest request = request( uri, MediaType.APPLICATION_JSON_TYPE );
            final ClientResponse< String > response = request.get( String.class );
            final String entity = response.getEntity();
            assertThat( entity, is( notNullValue() ) );

            final RestVdbDataRole dataRole = KomodoJsonMarshaller.unmarshall( entity, RestVdbDataRole.class );
            assertNotNull( dataRole );

            assertThat( dataRole.isAllowCreateTempTables(), is( allowCreateTempTables ) );
            assertThat( dataRole.isAnyAuthenticated(), is( anyAuthenticated ) );
            assertThat( dataRole.getDataPath(), is( dataPath ) );
            assertThat( dataRole.getDescription(), is( description ) );
            assertThat( dataRole.isGrantAll(), is( grantAll ) );
            assertThat( dataRole.getId(), is( id ) );
            assertThat( dataRole.getkType(), is( type ) );
            assertThat( Arrays.equals( dataRole.getMappedRoles(), mappedRoles ), is( true ) );
            assertThat( dataRole.getName(), is( dataRoleName ) );
            assertThat( dataRole.getPermissions().length, is( 1 ) );

            final Collection< RestLink > links = dataRole.getLinks();
            assertEquals( 4, links.size() );
        }
    }
    
    @Test
    public void shouldDeleteDataRole() throws Exception {
        loadVdbs();

        final String dataRoleName = "MyDataRole";

        final Properties settings = _uriBuilder.createSettings( SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME );
        _uriBuilder.addSetting( settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri() );
        _uriBuilder.addSetting( settings, SettingNames.DATA_ROLE_ID, dataRoleName );
        final URI uri = _uriBuilder.vdbDataRoleUri( LinkType.SELF, settings );

        { // create data role
            final boolean allowCreateTempTables = true;
            final boolean anyAuthenticated = true;
            final String dataPath = "/tko:komodo/tko:workspace/user/"
                                    + TestUtilities.PORTFOLIO_VDB_NAME
                                    + '/'
                                    + VdbLexicon.Vdb.DATA_ROLES
                                    + '/'
                                    + dataRoleName;
            final String description = "My data role description";
            final boolean grantAll = true;
            final String[] mappedRoles = new String[] { "a", "b", "c", "d", "e" };
            final KomodoType type = KomodoType.VDB_DATA_ROLE;
            final String id = dataRoleName;

            final RestVdbDataRole dataRole = new RestVdbDataRole();
            dataRole.setAllowCreateTempTables( allowCreateTempTables );
            dataRole.setAnyAuthenticated( anyAuthenticated );
            dataRole.setDataPath( dataPath );
            dataRole.setDescription( description );
            dataRole.setGrantAll( grantAll );
            dataRole.setId( id );
            dataRole.setkType( type );
            dataRole.setMappedRoles( mappedRoles );
            dataRole.setName( dataRoleName );

            final String json = KomodoJsonMarshaller.marshall( dataRole );
            final ClientRequest request = request( uri, MediaType.APPLICATION_JSON_TYPE );
            addBody( request, json );

            final ClientResponse< String > response = request.post( String.class );
            assertThat( response, is( notNullValue() ) );
            assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

            final String entity = response.getEntity();
            assertThat( entity, is( notNullValue() ) );
        }
        
        { // delete data role
            final ClientRequest request = request( uri, MediaType.APPLICATION_JSON_TYPE );
            final ClientResponse< String > response = request.delete( String.class );
            assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );
        }
        
        { // verify data role no longer exists
            final ClientRequest request = request( uri, MediaType.APPLICATION_JSON_TYPE );
            final ClientResponse< String > response = request.get( String.class );
            assertThat( response.getStatus(), is( Response.Status.NOT_FOUND.getStatusCode() ) );
        }
    }
    
    @Test
    public void shouldGetVdbDataRoles() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME, LinkType.DATA_ROLES);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
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

        Collection<RestLink> links = dataRole.getLinks();
        assertEquals(4, links.size());
    }

    @Test
    public void shouldGetVdbDataRolesEmptyList() throws Exception {
        loadVdbs();

        // get
        URI vdbBaseUri = _uriBuilder.workspaceVdbsUri();
        URI uri = _uriBuilder.vdbChildGroupUri(vdbBaseUri, TestUtilities.PORTFOLIO_VDB_NAME, LinkType.DATA_ROLES);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestBasicEntity[] dataRoles = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertNotNull(dataRoles);
        assertEquals(0, dataRoles.length);
    }

    @Test
    public void shouldGetVdbDataRole() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        URI uri = _uriBuilder.vdbDataRoleUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
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

        Collection<RestLink> links = dataRole.getLinks();
        assertEquals(4, links.size());
    }

    @Test
    public void shouldGetVdbPermissions() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        URI uri = _uriBuilder.vdbDataRoleUri(LinkType.PERMISSIONS, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbPermission[] permissions = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbPermission[].class);
        assertNotNull(permissions);
        assertEquals(4, permissions.length);

        for (RestVdbPermission permission : permissions) {
            assertEquals(KomodoType.VDB_PERMISSION, permission.getkType());
            Collection<RestLink> links = permission.getLinks();
            assertEquals(5, links.size());

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
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T1");
        URI uri = _uriBuilder.vdbPermissionUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
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

        Collection<RestLink> links = permission.getLinks();
        assertEquals(5, links.size());
    }

    @Test
    public void shouldGetVdbConditions() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2");
        URI uri = _uriBuilder.vdbPermissionUri(LinkType.CONDITIONS, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbCondition[] conditions = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbCondition[].class);
        assertNotNull(conditions);
        assertEquals(1, conditions.length);

        RestVdbCondition condition = conditions[0];
        assertEquals("col1 = user()", condition.getName());
        assertFalse(condition.isConstraint());

        Collection<RestLink> links = condition.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbCondition() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_TYPE, LinkType.CONDITIONS.uriName());
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_ID, "col1 = user()");
        URI uri = _uriBuilder.vdbPermissionChildUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbCondition condition = KomodoJsonMarshaller.unmarshall(entity, RestVdbCondition.class);
        assertNotNull(condition);

        assertEquals("col1 = user()", condition.getName());
        assertFalse(condition.isConstraint());

        Collection<RestLink> links = condition.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbMasks() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2.col1");
        URI uri = _uriBuilder.vdbPermissionUri(LinkType.MASKS, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbMask[] masks = KomodoJsonMarshaller.unmarshallArray(entity, RestVdbMask[].class);
        assertNotNull(masks);
        assertEquals(1, masks.length);

        RestVdbMask mask = masks[0];
        assertEquals("col2", mask.getName());
        assertEquals("1", mask.getOrder());

        Collection<RestLink> links = mask.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbMask() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.VDB_PARENT_PATH, _uriBuilder.workspaceVdbsUri());
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2.col1");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_TYPE, LinkType.MASKS.uriName());
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_ID, "col2");
        URI uri = _uriBuilder.vdbPermissionChildUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbMask mask = KomodoJsonMarshaller.unmarshall(entity, RestVdbMask.class);
        assertNotNull(mask);

        assertEquals("col2", mask.getName());
        assertEquals("1", mask.getOrder());

        Collection<RestLink> links = mask.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldFailNameValidationWhenNameAlreadyExists() throws Exception {
        _restApp.importVdb( TestUtilities.partsExample(), USER_NAME );

        // try and validate the same name of an existing VDB
        final URI vdbUri = _uriBuilder.workspaceVdbsUri();
        final URI uri = UriBuilder.fromUri( vdbUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( "MyPartsVDB_Dynamic" )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( notNullValue() ) );
    }

    @Test
    public void shouldFailNameValidationWhenConnectionWithSameNameExists() throws Exception {
    	final String sourceName = "elvis";
        _restApp.createConnection( sourceName, USER_NAME );

        // try and validate the same name of an existing VDB
        final URI vdbUri = _uriBuilder.workspaceVdbsUri();
        final URI uri = UriBuilder.fromUri( vdbUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( sourceName )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( notNullValue() ) );
    }

    @Test
    public void shouldFailNameValidationWhenNameHasInvalidCharacters() throws Exception {
        final URI vdbUri = _uriBuilder.workspaceVdbsUri();
        final URI uri = UriBuilder.fromUri( vdbUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( "InvalidN@me" )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( notNullValue() ) );
    }

    @Test
    public void shouldFailNameValidationWhenNameIsEmpty() throws Exception {
        final URI vdbUri = _uriBuilder.workspaceConnectionsUri();
        final URI uri = UriBuilder.fromUri( vdbUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( "" )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.INTERNAL_SERVER_ERROR.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, startsWith( "RESTEASY" ) );
    }

    @Test
    public void shouldFailNameValidationWhenNameHasSpaces() throws Exception {
        final URI vdbUri = _uriBuilder.workspaceVdbsUri();
        final URI uri = UriBuilder.fromUri( vdbUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( "a b c" )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( notNullValue() ) );
    }

    @Test
    public void shouldFailNameValidationWhenMissingNameSegment() throws Exception {
        final URI vdbUri = _uriBuilder.workspaceVdbsUri();
        final URI uri = UriBuilder.fromUri( vdbUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.INTERNAL_SERVER_ERROR.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, startsWith( "RESTEASY" ) );
    }

    @Test
    public void shouldValidateName() throws Exception {
        final URI vdbUri = _uriBuilder.workspaceVdbsUri();
        final URI uri = UriBuilder.fromUri( vdbUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( "ValidName" )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( "" ) ); // no error message since name was valid
    }
}
