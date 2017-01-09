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
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.net.URI;
import java.util.List;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.Test;
import org.komodo.rest.RestBasicEntity;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.KomodoProperties;
import org.komodo.rest.relational.RestEntityFactory;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoSearcherAttributes;
import org.komodo.rest.relational.response.KomodoSavedSearcher;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.rest.relational.response.RestVdbModel;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.KomodoType;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlLexicon;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

@SuppressWarnings( {"javadoc", "nls", "deprecation"} )
public final class KomodoSearchServiceTest extends AbstractKomodoServiceTest {

    private final static String PORTFOLIO_DATA_PATH = "/tko:komodo/tko:workspace/" + USER_NAME + "/Portfolio";

    @Test
    public void shouldFailNoParameters() throws Exception {
        // get
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        System.out.println("Response:\n" + entity);

        JsonElement jelement = new JsonParser().parse(entity);
        assertNotNull(jelement);
        JsonObject  jobject = jelement.getAsJsonObject();
        assertEquals("\"The search service requires at least one parameter\"", jobject.get("error").toString());
    }

    @Test
    public void shouldSearchForAnythingContainingView() throws Exception {
        loadVdbs();

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_CONTAINS_PARAMETER, "view");
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(19, entities.length);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertNotEquals(KomodoType.UNKNOWN, e.getkType());
        }
    }

    @Test
    public void shouldSearchForAnyModelContainingView() throws Exception {
        loadVdbs();

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_TYPE_PARAMETER, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        properties.addProperty(SEARCH_CONTAINS_PARAMETER, "view");
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(5, entities.length);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertEquals(KomodoType.MODEL, e.getkType());
        }
    }

    @Test
    public void shouldSearchForAnyModelUnderPortfolioContainingView() throws Exception {
        loadVdbs();

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_TYPE_PARAMETER, VdbLexicon.Vdb.DECLARATIVE_MODEL);
        properties.addProperty(SEARCH_PARENT_PARAMETER, PORTFOLIO_DATA_PATH);
        properties.addProperty(SEARCH_CONTAINS_PARAMETER, "view");
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(2, entities.length);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertEquals(KomodoType.MODEL, e.getkType());
            assertTrue(e.getDataPath().startsWith(PORTFOLIO_DATA_PATH));
        }
    }

    @Test
    public void shouldSearchByPath() throws Exception {
        loadVdbs();

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_PATH_PARAMETER, PORTFOLIO_DATA_PATH);
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(1, entities.length);

        RestBasicEntity basicEntity = entities[0];
        RestVdb vdb = RestEntityFactory.resolve(basicEntity, RestVdb.class);
        assertNotNull(vdb);

        assertPortfolio(vdb);
    }

    @Test
    public void shouldSearchByParent() throws Exception {
        loadVdbs();

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_PARENT_PARAMETER, PORTFOLIO_DATA_PATH);
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(5, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            RestVdbModel model = RestEntityFactory.resolve(basicEntity, RestVdbModel.class);
            assertNotNull(model);
        }
    }

    @Test
    public void shouldSearchByAncestor() throws Exception {
        loadVdbs();

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_ANCESTOR_PARAMETER, PORTFOLIO_DATA_PATH);
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(110, entities.length);
    }

    @Test
    public void shouldSearchByType() throws Exception {
        loadVdbs();

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_TYPE_PARAMETER, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(38, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
        }
    }

    @Test
    public void shouldSearchByKType() throws Exception {
        loadVdbs();

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_TYPE_PARAMETER, KomodoType.COLUMN.getType());
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(38, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
        }
    }

    @Test
    public void shouldSearchByKTypeAndLocalName() throws Exception {
        loadVdbs();

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_TYPE_PARAMETER, KomodoType.COLUMN.getType());
        properties.addProperty(SEARCH_OBJECT_NAME_PARAMETER, "%ID%");
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(12, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
            assertTrue(basicEntity.getId().contains("ID"));
            // System.out.println(basicEntity.getDataPath());
        }
    }

    @Test
    public void shouldExecuteSavedSearch()  throws Exception {
        loadVdbs();
        List<String> searchNames = loadSampleSearches();

        // get
        KomodoProperties properties = new KomodoProperties();
        properties.addProperty(SEARCH_SAVED_NAME_PARAMETER, searchNames.get(0));
        URI uri = _uriBuilder.searchUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(4, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.VDB, kType);
            // System.out.println(basicEntity.getDataPath());
        }
    }

    @Test
    public void shouldReturnSavedSearches() throws Exception {
        loadVdbs();
        List<String> searchNames = loadSampleSearches();

        // get
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.savedSearchCollectionUri(properties);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        KomodoSavedSearcher[] entities = KomodoJsonMarshaller.unmarshallArray(entity, KomodoSavedSearcher[].class);
        assertEquals(searchNames.size(), entities.length);

        for (KomodoSavedSearcher kso : entities) {
            searchNames.contains(kso.getName());
        }
    }

    @Test
    public void shouldSaveSearch() throws Exception {
        loadVdbs();

        // post
        URI uri = _uriBuilder.savedSearchCollectionUri(new KomodoProperties());

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        String searchName = "Vdbs Search";
        searchAttr.setSearchName(searchName);
        searchAttr.setType(VdbLexicon.Vdb.VIRTUAL_DATABASE);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        request.post(String.class);

        Repository repository = getRestApp().getDefaultRepository();
        UnitOfWork uow = repository.createTransaction(USER_NAME,
                                                      getClass().getSimpleName() + COLON + "FindSavedSearches" + COLON + System.currentTimeMillis(),
                                                      true, null);

        KomodoObject searches = repository.komodoSearches(uow);
        KomodoObject[] children = searches.getChildren(uow);
        assertEquals(1, children.length);
        assertEquals(searchName, children[0].getName(uow));
    }

    @Test
    public void shouldSaveSearchWithParameters() throws Exception {
        loadVdbs();

        // post
        URI uri = _uriBuilder.savedSearchCollectionUri(new KomodoProperties());

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        String searchName = "Vdbs Search";
        searchAttr.setSearchName(searchName);
        searchAttr.setType("{fromTypeParam}");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        request.post(String.class);

        Repository repository = getRestApp().getDefaultRepository();
        UnitOfWork uow = repository.createTransaction(USER_NAME,
                                                      getClass().getSimpleName() + COLON + "FindSavedSearches" + COLON + System.currentTimeMillis(),
                                                      true, null);

        KomodoObject searches = repository.komodoSearches(uow);
        KomodoObject[] children = searches.getChildren(uow);
        assertEquals(1, children.length);
        assertEquals(searchName, children[0].getName(uow));
    }

    @Test
    public void shouldDeleteSavedSearch() throws Exception {
        loadVdbs();
        List<String> searchNames = loadSampleSearches();

        // delete
        URI uri = _uriBuilder.savedSearchCollectionUri(new KomodoProperties());
        String searchName = searchNames.get(0);
        uri = UriBuilder.fromUri(uri).path(searchName).build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        request.delete(String.class);

        Repository repository = getRestApp().getDefaultRepository();
        UnitOfWork uow = repository.createTransaction(USER_NAME,
                                                      getClass().getSimpleName() + COLON + "FindSavedSearches" + COLON + System.currentTimeMillis(),
                                                      true, null);

        KomodoObject searches = repository.komodoSearches(uow);
        KomodoObject[] children = searches.getChildren(uow);
        assertEquals(searchNames.size() - 1, children.length);
        for (KomodoObject child : children) {
            assertNotEquals(searchName, child.getName(uow));
        }
    }

    @Test
    public void shouldAdvancedSearchForAnythingContainingView() throws Exception {
        loadVdbs();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setContains("view");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(19, entities.length);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertNotEquals(KomodoType.UNKNOWN, e.getkType());
        }
    }

    @Test
    public void shouldAdvancedSearchForAnyModelContainingView() throws Exception {
        loadVdbs();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setType(VdbLexicon.Vdb.DECLARATIVE_MODEL);
        searchAttr.setContains("view");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(5, entities.length);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertEquals(KomodoType.MODEL, e.getkType());
        }
    }

    @Test
    public void shouldAdvancedSearchForAnyModelUnderPortfolioContainingView() throws Exception {
        loadVdbs();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setType(VdbLexicon.Vdb.DECLARATIVE_MODEL);
        searchAttr.setParent(PORTFOLIO_DATA_PATH);
        searchAttr.setContains("view");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(2, entities.length);

        for (RestBasicEntity e : entities) {
            assertNotNull(e.getId());
            assertNotNull(e.getBaseUri());
            assertNotNull(e.getDataPath());
            assertNotNull(e.getkType());
            assertNotNull(e.hasChildren());
            assertEquals(KomodoType.MODEL, e.getkType());
            assertTrue(e.getDataPath().startsWith(PORTFOLIO_DATA_PATH));
        }
    }

    @Test
    public void shouldAdvancedSearchByPath() throws Exception {
        loadVdbs();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setPath(PORTFOLIO_DATA_PATH);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        System.out.println("Response:\n" + entity);
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(1, entities.length);

        RestBasicEntity basicEntity = entities[0];
        RestVdb vdb = RestEntityFactory.resolve(basicEntity, RestVdb.class);
        assertNotNull(vdb);

        assertPortfolio(vdb);
    }

    @Test
    public void shouldAdvancedSearchByParent() throws Exception {
        loadVdbs();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setParent(PORTFOLIO_DATA_PATH);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(5, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            RestVdbModel model = RestEntityFactory.resolve(basicEntity, RestVdbModel.class);
            assertNotNull(model);
        }
    }

    @Test
    public void shouldAdvancedSearchByAncestor() throws Exception {
        loadVdbs();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setAncestor(PORTFOLIO_DATA_PATH);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(110, entities.length);
    }

    @Test
    public void shouldAdvancedSearchByType() throws Exception {
        loadVdbs();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setType(TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(38, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
        }
    }

    @Test
    public void shouldAdvancedSearchByKType() throws Exception {
        loadVdbs();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setType(KomodoType.COLUMN.getType());

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(38, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
        }
    }

    @Test
    public void shouldAdvancedSearchByKTypeAndLocalName() throws Exception {
        loadVdbs();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setType(KomodoType.COLUMN.getType());
        searchAttr.setObjectName("%ID%");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(12, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
            assertTrue(basicEntity.getId().contains("ID"));
            // System.out.println(basicEntity.getDataPath());
        }
    }

    @Test
    public void shouldAdvancedExecuteSavedSearch()  throws Exception {
        loadVdbs();
        List<String> searchNames = loadSampleSearches();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setSearchName(searchNames.get(0));

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(4, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.VDB, kType);
            // System.out.println(basicEntity.getDataPath());
        }
    }

    @Test
    public void shouldAdvancedExecuteSavedSearchWithParameter()  throws Exception {
        loadVdbs();
        List<String> searchNames = loadSampleSearches();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setSearchName(searchNames.get(2));
        searchAttr.setParameter("valueParam", "%_ID");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(12, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.COLUMN, kType);
            assertTrue(basicEntity.getId().endsWith("_ID"));
        }
    }

    @Test
    public void shouldFailToSavedSearchDueToLackofParameter()  throws Exception {
        loadVdbs();
        List<String> searchNames = loadSampleSearches();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setSearchName(searchNames.get(2));

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        assertEquals(Response.Status.FORBIDDEN.getStatusCode(), response.getStatus());
        final String entity = response.getEntity();
        System.out.println("Response:\n" + entity);
        assertTrue(entity.contains("An error occurred whilst searching the workspace: " +
                            "Search requires the parameter valueParam but has not been provided a value"));
    }

    @Test
    public void shouldAdvancedExecuteSavedSearchWithKTypeParameter()  throws Exception {
        loadVdbs();
        List<String> searchNames = loadSampleSearches();

        // post
        KomodoProperties properties = new KomodoProperties();
        URI uri = _uriBuilder.searchUri(properties);

        KomodoSearcherAttributes searchAttr = new KomodoSearcherAttributes();
        searchAttr.setSearchName(searchNames.get(3));
        searchAttr.setParameter("fromTypeParam", "Vdb");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, searchAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        // System.out.println("Response:\n" + entity);
        RestBasicEntity[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestBasicEntity[].class);
        assertEquals(4, entities.length);

        for (RestBasicEntity basicEntity : entities) {
            KomodoType kType = basicEntity.getkType();
            assertEquals(KomodoType.VDB, kType);
        }
    }
}
