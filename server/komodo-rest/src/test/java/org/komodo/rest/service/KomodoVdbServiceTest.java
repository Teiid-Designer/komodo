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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.net.URI;
import java.util.Collection;
import java.util.Properties;
import javax.ws.rs.client.Invocation.Builder;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.core.UriBuilder;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.relational.model.Model.Type;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.RestVdb;
import org.komodo.rest.relational.RestVdbCondition;
import org.komodo.rest.relational.RestVdbDataRole;
import org.komodo.rest.relational.RestVdbImport;
import org.komodo.rest.relational.RestVdbMask;
import org.komodo.rest.relational.RestVdbModel;
import org.komodo.rest.relational.RestVdbModelSource;
import org.komodo.rest.relational.RestVdbPermission;
import org.komodo.rest.relational.RestVdbTranslator;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.service.KomodoVdbService;
import org.komodo.spi.repository.KomodoType;
import org.komodo.test.utils.TestUtilities;

@SuppressWarnings( {"javadoc", "nls"} )
public final class KomodoVdbServiceTest extends AbstractKomodoServiceTest {

    @Rule
    public TestName testName = new TestName();

    @Test
    public void shouldGetVdbs() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbsUri();
        this.response = request(uri).get();
        assertTrue(response.hasEntity());

        final String entities = response.readEntity(String.class);
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
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        this.response = request(_uriBuilder.buildVdbUri(LinkType.SELF, settings)).get();
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

        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        URI uri = _uriBuilder.buildVdbUri(LinkType.SELF, settings);
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

            Collection<RestLink> links = model.getLinks();
            assertEquals(4, links.size());
        }
    }

    @Test
    public void shouldGetVdbModel() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PersonalValuations");
        URI uri = _uriBuilder.buildVdbModelUri(LinkType.SELF, settings);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
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
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PersonalValuations");
        URI uri = _uriBuilder.buildVdbModelUri(LinkType.SOURCES, settings);
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

        Collection<RestLink> links = source.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbModelSourcesIncludeReferenceForTranslator() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.TWEET_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "twitter");
        URI uri = _uriBuilder.buildVdbModelUri(LinkType.SOURCES, settings);
        this.response = request(uri).get();
        final String entities = this.response.readEntity(String.class);
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
        assertTrue(href.toString().endsWith("/v1/workspace/vdbs/twitter/VdbTranslators/rest"));
    }

    @Test
    public void shouldGetVdbModelSource() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.PORTFOLIO_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.MODEL_NAME, "PersonalValuations");
        _uriBuilder.addSetting(settings, SettingNames.SOURCE_NAME, "excelconnector");
        URI uri = _uriBuilder.buildVdbModelSourceUri(LinkType.SELF, settings);
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

        Collection<RestLink> links = source.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbTranslators() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbChildGroupUri(TestUtilities.TWEET_EXAMPLE_VDB_NAME, LinkType.TRANSLATORS);
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

        Collection<RestLink> links = translator.getLinks();
        assertEquals(3, links.size());
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
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.TWEET_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.TRANSLATOR_NAME, "rest");
        URI uri = _uriBuilder.buildVdbTranslatorUri(LinkType.SELF, settings);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response from uri " + uri + ":\n" + entity);

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
        URI uri = _uriBuilder.generateVdbChildGroupUri(TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME, LinkType.IMPORTS);
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

        Collection<RestLink> links = vdbImport.getLinks();
        assertEquals(3, links.size());
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
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.IMPORT_NAME, "x");
        URI uri = _uriBuilder.buildVdbImportUri(LinkType.SELF, settings);
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

        Collection<RestLink> links = vdbImport.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbDataRoles() throws Exception {
        loadVdbs();

        // get
        URI uri = _uriBuilder.generateVdbChildGroupUri(TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME, LinkType.DATA_ROLES);
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

        Collection<RestLink> links = dataRole.getLinks();
        assertEquals(4, links.size());
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

        RestBasicEntity[] dataRoles = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertNotNull(dataRoles);
        assertEquals(0, dataRoles.length);
    }

    @Test
    public void shouldGetVdbDataRole() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        URI uri = _uriBuilder.buildVdbDataRoleUri(LinkType.SELF, settings);
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

        Collection<RestLink> links = dataRole.getLinks();
        assertEquals(4, links.size());
    }

    @Test
    public void shouldGetVdbPermissions() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        URI uri = _uriBuilder.buildVdbDataRoleUri(LinkType.PERMISSIONS, settings);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
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
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T1");
        URI uri = _uriBuilder.buildVdbPermissionUri(LinkType.SELF, settings);
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

        Collection<RestLink> links = permission.getLinks();
        assertEquals(5, links.size());
    }

    @Test
    public void shouldGetVdbConditions() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2");
        URI uri = _uriBuilder.buildVdbPermissionUri(LinkType.CONDITIONS, settings);
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

        Collection<RestLink> links = condition.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbCondition() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_TYPE, LinkType.CONDITIONS.uriName());
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_ID, "col1 = user()");
        URI uri = _uriBuilder.buildVdbPermissionChildUri(LinkType.SELF, settings);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
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
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2.col1");
        URI uri = _uriBuilder.buildVdbPermissionUri(LinkType.MASKS, settings);
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

        Collection<RestLink> links = mask.getLinks();
        assertEquals(3, links.size());
    }

    @Test
    public void shouldGetVdbMask() throws Exception {
        loadVdbs();

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.VDB_NAME, TestUtilities.ALL_ELEMENTS_EXAMPLE_VDB_NAME);
        _uriBuilder.addSetting(settings, SettingNames.DATA_ROLE_ID, "roleOne");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_ID, "myTable.T2.col1");
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_TYPE, LinkType.MASKS.uriName());
        _uriBuilder.addSetting(settings, SettingNames.PERMISSION_CHILD_ID, "col2");
        URI uri = _uriBuilder.buildVdbPermissionChildUri(LinkType.SELF, settings);
        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertThat(entity, is(notNullValue()));

        //System.out.println("Response:\n" + entity);

        RestVdbMask mask = KomodoJsonMarshaller.unmarshall(entity, RestVdbMask.class);
        assertNotNull(mask);

        assertEquals("col2", mask.getName());
        assertEquals("1", mask.getOrder());

        Collection<RestLink> links = mask.getLinks();
        assertEquals(3, links.size());
    }
}
