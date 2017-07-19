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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;
import org.komodo.relational.ViewBuilderCriteriaPredicate;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.RestLink;
import org.komodo.rest.RestLink.LinkType;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.KomodoRestUriBuilder.SettingNames;
import org.komodo.rest.relational.connection.RestConnection;
import org.komodo.rest.relational.dataservice.RestDataservice;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.request.KomodoDataserviceUpdateAttributes;
import org.komodo.rest.relational.response.RestConnectionDriver;
import org.komodo.rest.relational.response.RestDataserviceViewInfo;
import org.komodo.rest.relational.response.RestVdb;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.test.utils.TestUtilities;

@SuppressWarnings( {"javadoc", "nls"} )
public final class KomodoDataserviceServiceTest extends AbstractKomodoServiceTest {

    public static final String DATASERVICE_NAME = "MyDataService"; 
    
    @Rule
    public TestName testName = new TestName();

    @Test
    public void shouldGetDataservices() throws Exception {
        createDataservice(DATASERVICE_NAME);

        // get
        URI uri = _uriBuilder.workspaceDataservicesUri();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        assertNotNull(response.getEntity());

        final String entities = response.getEntity();
        assertThat(entities, is(notNullValue()));

        // make sure the Dataservice JSON document is returned for each dataservice
        RestDataservice[] dataservices = KomodoJsonMarshaller.unmarshallArray(entities, RestDataservice[].class);

        assertEquals(1, dataservices.length);
        RestDataservice myService = dataservices[0];
        assertNotNull(myService.getId());
        assertTrue(DATASERVICE_NAME.equals(myService.getId()));
        assertNotNull(myService.getDataPath());
        assertNotNull(myService.getkType());
    }
    
    @Test
    public void shouldReturnEmptyListWhenNoDataservicesInWorkspace() throws Exception {
        URI uri = _uriBuilder.workspaceDataservicesUri();
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        RestDataservice[] dataservices = KomodoJsonMarshaller.unmarshallArray(entity, RestDataservice[].class);
        assertNotNull(dataservices);
        assertEquals(0, dataservices.length);
    }
    
    @Test
    public void shouldGetDataservice() throws Exception {
        createDataservice(DATASERVICE_NAME);

        // get
        Properties settings = _uriBuilder.createSettings(SettingNames.DATA_SERVICE_NAME, DATASERVICE_NAME);
        _uriBuilder.addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, _uriBuilder.workspaceDataservicesUri());

        URI uri = _uriBuilder.dataserviceUri(LinkType.SELF, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        RestDataservice dataservice = KomodoJsonMarshaller.unmarshall(entity, RestDataservice.class);
        assertNotNull(dataservice);
        
        assertEquals(dataservice.getId(), DATASERVICE_NAME);
    }

    @Test
    public void shouldGetDataserviceConnections() throws Exception {
        loadStatesDataService();

        // get
        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        Properties settings = _uriBuilder.createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        _uriBuilder.addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, _uriBuilder.workspaceDataservicesUri());

        URI uri = _uriBuilder.dataserviceUri(LinkType.CONNECTIONS, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        System.out.println("Response:\n" + entity);

        RestConnection[] connections = KomodoJsonMarshaller.unmarshallArray(entity, RestConnection[].class);
        assertNotNull(connections);
        assertEquals(1, connections.length);

        RestConnection connection = connections[0];
        assertEquals("MySqlPool", connection.getId());
        assertEquals("java:/MySqlDS", connection.getJndiName());
        if (getTeiidVersion().isGreaterThanOrEqualTo(Version.TEIID_9_0))
            assertEquals("mysql-connector-java-5.1.39-bin.jar_com.mysql.jdbc.Driver_5_1", connection.getDriverName());
        else
            assertEquals("mysql-connector-java-5.1.39-bin.jarcom.mysql.jdbc.Driver_5_1", connection.getDriverName());

        Collection<RestLink> links = connection.getLinks();
        assertNotNull(links);
        assertEquals(3, links.size());

        for(RestLink link : links) {
            LinkType rel = link.getRel();
            assertTrue(LinkType.SELF.equals(rel) || LinkType.PARENT.equals(rel) || LinkType.CHILDREN.equals(rel));

            if (LinkType.SELF.equals(rel)) {
                String href = _uriBuilder.workspaceConnectionsUri() + FORWARD_SLASH + connection.getId();
                assertEquals(href, link.getHref().toString());
            }
        }
    }
    
    @Test
    public void shouldGetDataserviceDrivers() throws Exception {
        loadStatesDataService();

        // get
        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        Properties settings = _uriBuilder.createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        _uriBuilder.addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, _uriBuilder.workspaceDataservicesUri());

        URI uri = _uriBuilder.dataserviceUri(LinkType.DRIVERS, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        System.out.println("Response:\n" + entity);

        RestConnectionDriver[] drivers = KomodoJsonMarshaller.unmarshallArray(entity, RestConnectionDriver[].class);
        assertNotNull(drivers);
        assertEquals(1, drivers.length);

        RestConnectionDriver connectionDriver = drivers[0];
        assertEquals("mysql-connector-java-5.1.39-bin.jar", connectionDriver.getName());
    }

    @Test
    public void shouldGetViewInfoForSingleSourceDataService() throws Exception {
        loadDsbSingleSourceDataService();

        // get
        String dsName = TestUtilities.PARTS_SINGLE_SOURCE_SERVICE_NAME;
        Properties settings = _uriBuilder.createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        _uriBuilder.addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, _uriBuilder.workspaceDataservicesUri());

        URI uri = _uriBuilder.dataserviceUri(LinkType.SERVICE_VIEW_INFO, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        RestDataserviceViewInfo[] viewInfos = KomodoJsonMarshaller.unmarshallArray(entity, RestDataserviceViewInfo[].class);
        assertEquals(2, viewInfos.length);
        
        for(int i = 0; i < viewInfos.length; i++) {
            String infoType = viewInfos[i].getInfoType();
            if(infoType.equals(RestDataserviceViewInfo.LH_TABLE_INFO)) {
                assertEquals("OracleParts", viewInfos[i].getSourceVdbName());
                assertEquals("SUPPLIER", viewInfos[i].getTableName());
                List<String> colList = Arrays.asList(viewInfos[i].getColumnNames());
                assertTrue(colList.contains("SUPPLIER_ID"));
                assertTrue(colList.contains("SUPPLIER_NAME"));
                assertTrue(colList.contains("SUPPLIER_STATUS"));
                assertTrue(colList.contains("SUPPLIER_CITY"));
                assertTrue(colList.contains("SUPPLIER_STATE"));
            } else if(infoType.equals(RestDataserviceViewInfo.DDL_INFO)) {
                assertEquals(true, viewInfos[i].isViewEditable());
                assertEquals("CREATE VIEW PartsSingleSourceView (\n\tSUPPLIER_ID string,\n\tSUPPLIER_NAME string,\n\tSUPPLIER_STATUS bigdecimal,\n\tSUPPLIER_CITY string,\n\tSUPPLIER_STATE string,\n\tPRIMARY KEY(SUPPLIER_ID)\n)\nAS\nSELECT SUPPLIER_ID, SUPPLIER_NAME, SUPPLIER_STATUS, SUPPLIER_CITY, SUPPLIER_STATE FROM OracleParts.SUPPLIER;\n", viewInfos[i].getViewDdl());
            }
        }
    }
    
    @Test
    public void shouldGetViewInfoForJoinSameTableNamesDataService() throws Exception {
        loadDsbJoinSameTableNamesDataService();

        // get
        String dsName = TestUtilities.JOIN_SAME_TABLE_NAMES_SERVICE_NAME;
        Properties settings = _uriBuilder.createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        _uriBuilder.addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, _uriBuilder.workspaceDataservicesUri());

        URI uri = _uriBuilder.dataserviceUri(LinkType.SERVICE_VIEW_INFO, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        RestDataserviceViewInfo[] viewInfos = KomodoJsonMarshaller.unmarshallArray(entity, RestDataserviceViewInfo[].class);
        assertEquals(4, viewInfos.length);
       
        for(int i = 0; i < viewInfos.length; i++) {
            String infoType = viewInfos[i].getInfoType();
            if(infoType.equals(RestDataserviceViewInfo.LH_TABLE_INFO)) {
                assertEquals("BQTOracle", viewInfos[i].getSourceVdbName());
                assertEquals("SMALLA", viewInfos[i].getTableName());
                List<String> colList = Arrays.asList(viewInfos[i].getColumnNames());
                assertTrue(colList.contains("INTKEY"));
                assertTrue(colList.contains("STRINGKEY"));
            } else if(infoType.equals(RestDataserviceViewInfo.RH_TABLE_INFO)) {
                assertEquals("BQTOracle2", viewInfos[i].getSourceVdbName());
                assertEquals("SMALLA", viewInfos[i].getTableName());
                List<String> colList = Arrays.asList(viewInfos[i].getColumnNames());
                assertTrue(colList.contains("STRINGKEY"));
                assertTrue(colList.contains("INTNUM"));
            } else if(infoType.equals(RestDataserviceViewInfo.CRITERIA_INFO)) {
                assertEquals("INNER", viewInfos[i].getJoinType());
                List<ViewBuilderCriteriaPredicate> colList = viewInfos[i].getCriteriaPredicates();
                assertEquals(1, colList.size());
                ViewBuilderCriteriaPredicate predicate = colList.get(0);
                assertEquals("INTKEY", predicate.getLhColumn());
                assertEquals("INTKEY", predicate.getRhColumn());
                assertEquals("=", predicate.getOperator());
            } else if(infoType.equals(RestDataserviceViewInfo.DDL_INFO)) {
                assertEquals(true, viewInfos[i].isViewEditable());
                assertEquals("CREATE VIEW JoinServiceSameTableNamesView (\n\tRowId integer,\n\tINTKEY bigdecimal,\n\tA_STRINGKEY string,\n\tB_STRINGKEY string,\n\tINTNUM bigdecimal,\n\tPRIMARY KEY(RowId)\n)\nAS\nSELECT ROW_NUMBER() OVER (ORDER BY A.INTKEY), A.INTKEY, A.STRINGKEY, B.STRINGKEY, B.INTNUM FROM BQTOracle.SMALLA AS A INNER JOIN BQTOracle2.SMALLA AS B ON A.INTKEY = B.INTKEY;\n", viewInfos[i].getViewDdl());
            }
        }
    }
    
    @Test
    public void shouldGetViewInfoForJoinDifferentTableNamesDataService() throws Exception {
        loadDsbJoinDifferentTableNamesDataService();

        // get
        String dsName = TestUtilities.JOIN_DIFFERENT_TABLE_NAMES_SERVICE_NAME;
        Properties settings = _uriBuilder.createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        _uriBuilder.addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, _uriBuilder.workspaceDataservicesUri());

        URI uri = _uriBuilder.dataserviceUri(LinkType.SERVICE_VIEW_INFO, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        RestDataserviceViewInfo[] viewInfos = KomodoJsonMarshaller.unmarshallArray(entity, RestDataserviceViewInfo[].class);
        assertEquals(4, viewInfos.length);
        
        for(int i = 0; i < viewInfos.length; i++) {
            String infoType = viewInfos[i].getInfoType();
            if(infoType.equals(RestDataserviceViewInfo.LH_TABLE_INFO)) {
                assertEquals("BQTOracle", viewInfos[i].getSourceVdbName());
                assertEquals("SMALLA", viewInfos[i].getTableName());
                List<String> colList = Arrays.asList(viewInfos[i].getColumnNames());
                assertTrue(colList.contains("INTKEY"));
                assertTrue(colList.contains("STRINGKEY"));
            } else if(infoType.equals(RestDataserviceViewInfo.RH_TABLE_INFO)) {
                assertEquals("BQTOracle2", viewInfos[i].getSourceVdbName());
                assertEquals("SMALLB", viewInfos[i].getTableName());
                List<String> colList = Arrays.asList(viewInfos[i].getColumnNames());
                assertTrue(colList.contains("STRINGKEY"));
                assertTrue(colList.contains("INTNUM"));
            } else if(infoType.equals(RestDataserviceViewInfo.CRITERIA_INFO)) {
                assertEquals("INNER", viewInfos[i].getJoinType());
                List<ViewBuilderCriteriaPredicate> colList = viewInfos[i].getCriteriaPredicates();
                assertEquals(1, colList.size());
                ViewBuilderCriteriaPredicate predicate = colList.get(0);
                assertEquals("INTKEY", predicate.getLhColumn());
                assertEquals("INTKEY", predicate.getRhColumn());
                assertEquals("=", predicate.getOperator());
            } else if(infoType.equals(RestDataserviceViewInfo.DDL_INFO)) {
                assertEquals(true, viewInfos[i].isViewEditable());
                assertEquals("CREATE VIEW JoinServiceDifferentTableNamesView (\n\tRowId integer,\n\tINTKEY bigdecimal,\n\tA_STRINGKEY string,\n\tB_STRINGKEY string,\n\tINTNUM bigdecimal,\n\tPRIMARY KEY(RowId)\n)\nAS\nSELECT ROW_NUMBER() OVER (ORDER BY A.INTKEY), A.INTKEY, A.STRINGKEY, B.STRINGKEY, B.INTNUM FROM BQTOracle.SMALLA AS A INNER JOIN BQTOracle2.SMALLB AS B ON A.INTKEY = B.INTKEY;\n", viewInfos[i].getViewDdl());
            }
        }
    }
    
    @Test
    public void shouldGetWorkspaceSourceVdbForDataService() throws Exception {
        loadServiceSourceVdb();
        loadStatesDataService();
        
        // get
        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        Properties settings = _uriBuilder.createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        _uriBuilder.addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, _uriBuilder.workspaceDataservicesUri());

        URI uri = _uriBuilder.dataserviceUri(LinkType.SOURCE_VDB_MATCHES, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entity, RestVdb[].class);
        assertNotNull(vdbs);
        assertEquals(1, vdbs.length);

        RestVdb vdb = vdbs[0];
        assertEquals("ServiceSource", vdb.getId());
    }
    
    @Test
    public void shouldNotGetWorkspaceSourceVdbForDataService() throws Exception {
        loadStatesDataService();
        
        // get
        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        Properties settings = _uriBuilder.createSettings(SettingNames.DATA_SERVICE_NAME, dsName);
        _uriBuilder.addSetting(settings, SettingNames.DATA_SERVICE_PARENT_PATH, _uriBuilder.workspaceDataservicesUri());

        URI uri = _uriBuilder.dataserviceUri(LinkType.SOURCE_VDB_MATCHES, settings);
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        RestVdb[] vdbs = KomodoJsonMarshaller.unmarshallArray(entity, RestVdb[].class);
        assertNotNull(vdbs);
        assertEquals(0, vdbs.length);
    }

    @Test
    public void shouldNotSetServiceVdbForSingleTableMissingParameter() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_SINGLE_TABLE).build();

        // Required parms (dataserviceName, modelSourcePath, tablePath).
        // fails due to missing modelSourcePath
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("/path/to/table");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("missing one or more required parameters"));
    }

    @Test
    public void shouldNotSetServiceVdbForSingleTableBadTablePath() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_SINGLE_TABLE).build();

        // Required parms (dataserviceName, modelSourcePath, tablePath).
        // fails due to bad table path - (doesnt resolve to a table)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("/path/to/table");
        updateAttr.setModelSourcePath("/path/to/ModelSource");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("specified Table does not exist"));
    }

    @Test
    public void shouldNotSetServiceVdbForSingleTableBadDdl() throws Exception {
        loadVdbs();
        createDataservice(DATASERVICE_NAME);
        
        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_SINGLE_TABLE).build();

        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/PersonalValuations/Sheet1");
        updateAttr.setModelSourcePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/Accounts/vdb:sources/h2-connector");
        updateAttr.setViewDdl("CREATE VIEW blah blah");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("DDL Parsing encountered a problem"));
    }

    @Test
    public void shouldSetServiceVdbForSingleTable() throws Exception {
        loadVdbs();
        createDataservice(DATASERVICE_NAME);
        
        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_SINGLE_TABLE).build();

        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/PersonalValuations/Sheet1");
        updateAttr.setModelSourcePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/Accounts/vdb:sources/h2-connector");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("Successfully updated"));
    }
    
    @Test
    public void shouldNotSetServiceVdbForJoinTablesMissingParameter() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_JOIN_TABLES).build();

        // Required parms (dataserviceName, modelSourcePath, rhModelSourcePath, tablePath, rhTablePath).
        // fails due to missing rhModelSourcePath
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("/path/to/lhTable");
        updateAttr.setRhTablePath("/path/to/rhTable");
        updateAttr.setModelSourcePath("/path/to/lhModelSource");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("missing one or more required parameters"));
    }

    @Test
    public void shouldNotSetServiceVdbForJoinTablesBadTablePath() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_JOIN_TABLES).build();

        // Required parms (dataserviceName, modelSourcePath, rhModelSourcePath, tablePath, rhTablePath).
        // Must also have 'viewDdl' OR 
        // fails due to bad table path
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("/path/to/lhTable");
        updateAttr.setRhTablePath("/path/to/rhTable");
        updateAttr.setModelSourcePath("/path/to/lhModelSource");
        updateAttr.setRhModelSourcePath("/path/to/rhModelSource");
        updateAttr.setViewDdl("CREATE VIEW blah blah");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("specified Table does not exist"));
    }

    @Test
    public void shouldNotSetServiceVdbForJoinTablesBadDdl() throws Exception {
        loadVdbs();
        createDataservice(DATASERVICE_NAME);
        
        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VDB_FOR_JOIN_TABLES).build();

        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/PersonalValuations/Sheet1");
        updateAttr.setRhTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/PersonalValuations/Sheet1");
        updateAttr.setModelSourcePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/Accounts/vdb:sources/h2-connector");
        updateAttr.setRhModelSourcePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/Accounts/vdb:sources/h2-connector");
        updateAttr.setViewDdl("CREATE VIEW blah blah");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        // Bogus DDL results in an error
        final String entity = response.getEntity();
        assertTrue(entity.contains("DDL Parsing encountered a problem"));
    }
    
    @Test
    public void shouldNotGenerateViewDdlForSingleTableMissingParameter() throws Exception {
        loadVdbs();
        createDataservice(DATASERVICE_NAME);
        
        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_SINGLE_TABLE).build();

        // Required parms (dataserviceName, tablePath).
        // fails due to missing table path
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        // Bogus DDL results in an error
        final String entity = response.getEntity();
        assertTrue(entity.contains("missing one or more required parameters"));
    }
    
    @Test
    public void shouldNotGenerateViewDdlForSingleTableBadTablePath() throws Exception {
        loadVdbs();
        createDataservice(DATASERVICE_NAME);
        
        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_SINGLE_TABLE).build();

        // Required parms (dataserviceName, tablePath).
        // fails due to bad table path (does not resolve to table)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("/path/to/table");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        // Bogus DDL results in an error
        final String entity = response.getEntity();
        assertTrue(entity.contains("specified Table does not exist"));
    }
    
    @Test
    public void shouldGenerateViewDdlForSingleTable() throws Exception {
        loadVdbs();
        createDataservice(DATASERVICE_NAME);
        
        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_SINGLE_TABLE).build();

        // Required parms (dataserviceName, tablePath).
        // Valid entries - should generate DDL
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/PersonalValuations/Sheet1");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        
        RestDataserviceViewInfo viewInfo = KomodoJsonMarshaller.unmarshall(entity, RestDataserviceViewInfo.class);
        assertEquals(RestDataserviceViewInfo.DDL_INFO, viewInfo.getInfoType());
        assertEquals("CREATE VIEW MyDataServiceView ( ROW_ID INTEGER, ACCOUNT_ID INTEGER, PRODUCT_TYPE STRING, PRODUCT_VALUE STRING, " +
                     "CONSTRAINT PK0 PRIMARY KEY (ROW_ID)) AS \nSELECT  ROW_ID, ACCOUNT_ID, PRODUCT_TYPE, PRODUCT_VALUE \nFROM Portfolio.Sheet1;", viewInfo.getViewDdl());
        assertEquals(false, viewInfo.isViewEditable());
    }
    
    @Test
    public void shouldNotGenerateViewDdlForJoinTablesMissingParameter() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_JOIN_TABLES).build();

        // Required parms (dataserviceName, tablePath, rhTablePath, joinType).
        // fails due to missing joinType
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("/path/to/lhTable");
        updateAttr.setRhTablePath("/path/to/rhTable");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("missing one or more required parameters"));
    }

    @Test
    public void shouldNotGenerateViewDdlForJoinTablesBadTablePath() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_JOIN_TABLES).build();

        // Required parms (dataserviceName, tablePath, rhTablePath, joinType).
        // fails due to bad table path (does not resolve to a Table)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("/path/to/lhTable");
        updateAttr.setRhTablePath("/path/to/rhTable");
        updateAttr.setJoinType("INNER");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("specified Table does not exist"));
    }
    
    @Test
    public void shouldGenerateViewDdlForJoinTables() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.SERVICE_VIEW_DDL_FOR_JOIN_TABLES).build();

        // Required parms (dataserviceName, tablePath, rhTablePath, joinType).
        // Valid entries - should generate DDL
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setDataserviceName(DATASERVICE_NAME);
        updateAttr.setTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/PersonalValuations/Sheet1");
        updateAttr.setRhTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/Portfolio/PersonalValuations/Sheet1");
        updateAttr.setJoinType("INNER");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        
        RestDataserviceViewInfo viewInfo = KomodoJsonMarshaller.unmarshall(entity, RestDataserviceViewInfo.class);
        assertEquals(RestDataserviceViewInfo.DDL_INFO, viewInfo.getInfoType());
        assertEquals("CREATE VIEW MyDataServiceView (RowId integer PRIMARY KEY,  A_ROW_ID INTEGER, A_ACCOUNT_ID INTEGER, A_PRODUCT_TYPE STRING, A_PRODUCT_VALUE STRING,  " +
                     "B_ROW_ID INTEGER, B_ACCOUNT_ID INTEGER, B_PRODUCT_TYPE STRING, B_PRODUCT_VALUE STRING) AS \n" + 
                     "SELECT ROW_NUMBER() OVER (ORDER BY A.ROW_ID), A.ROW_ID, A.ACCOUNT_ID, A.PRODUCT_TYPE, A.PRODUCT_VALUE, B.ROW_ID, B.ACCOUNT_ID, B.PRODUCT_TYPE, B.PRODUCT_VALUE \n" + 
                     "FROM \nPortfolio.Sheet1 AS A \nINNER JOIN \nPortfolio.Sheet1 AS B ;", viewInfo.getViewDdl());
        assertEquals(false, viewInfo.isViewEditable());
    }
    
    @Test
    public void shouldFailNameValidationWhenNameAlreadyExists() throws Exception {
        // create a data service first
        createDataservice( DATASERVICE_NAME );

        // try and validate the same name of an existing data service
        final URI dsUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dsUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( DATASERVICE_NAME )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( notNullValue() ) );
    }

    @Test
    public void shouldFailNameValidationWhenVdbWithSameNameExists() throws Exception {
        // create a data source first
        createVdb( DATASERVICE_NAME );

        // try and validate the same name of an existing data service
        final URI dsUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dsUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( DATASERVICE_NAME )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( notNullValue() ) );
        assertThat( errorMsg.isEmpty(), is( false ) );
    }

    @Test
    public void shouldFailNameValidationWhenNameHasInvalidCharacters() throws Exception {
        final URI dsUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dsUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( "InvalidN@me" )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( notNullValue() ) );
        assertThat( errorMsg.isEmpty(), is( false ) );
    }

    @Test
    public void shouldFailNameValidationWhenNameIsEmpty() throws Exception {
        final URI dsUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dsUri )
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
        final URI dsUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dsUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( "a b c" )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( notNullValue() ) );
        assertThat( errorMsg.isEmpty(), is( false ) );
    }

    @Test
    public void shouldFailNameValidationWhenNameHasBackslash() throws Exception {
        final URI dsUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dsUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( "\\" )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( notNullValue() ) );
        assertThat( errorMsg.isEmpty(), is( false ) );
    }

    @Test
    public void shouldFailNameValidationWhenNameHasSpecialChars() throws Exception {
        final URI dsUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dsUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( "a#" )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( notNullValue() ) );
        assertThat( errorMsg.isEmpty(), is( false ) );
    }

    @Test
    public void shouldFailNameValidationWhenMissingNameSegment() throws Exception {
        final URI dsUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dsUri )
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
        final URI dsUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dsUri )
                                  .path( V1Constants.NAME_VALIDATION_SEGMENT )
                                  .path( "ValidName" )
                                  .build();
        final ClientRequest request = request( uri, MediaType.TEXT_PLAIN_TYPE );
        final ClientResponse< String > response = request.get( String.class );
        assertThat( response.getStatus(), is( Response.Status.OK.getStatusCode() ) );

        final String errorMsg = response.getEntity();
        assertThat( errorMsg, is( "" ) ); // no error message since name was valid
    }

    @Test
    public void shouldNotGenerateJoinCriteriaMissingParameter() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.CRITERIA_FOR_JOIN_TABLES).build();

        // Required parms (tablePath, rhTablePath).
        // fails due to missing joinType
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setTablePath("/path/to/lhTable");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("missing one or more required parameters"));
    }

    @Test
    public void shouldNotGenerateJoinCriteriaBadTablePath() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.CRITERIA_FOR_JOIN_TABLES).build();

        // Required parms (tablePath, rhTablePath).
        // fails due to bad table path (does not resolve to a Table)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setTablePath("/path/to/lhTable");
        updateAttr.setRhTablePath("/path/to/rhTable");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("specified Table does not exist"));
    }
    
    @Test
    public void shouldGenerateJoinCriteriaEmpty() throws Exception {
        loadVdbs();
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.CRITERIA_FOR_JOIN_TABLES).build();

        // Required parms (tablePath, rhTablePath).
        // Valid entries - should generate join criteria (empty)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/MyPartsVDB_Dynamic/PartsSS/PARTS");
        updateAttr.setRhTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/MyPartsVDB_Dynamic/PartsSS/STATUS");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        
        // Info is returned but the criteria is empty, since there are no table pk-fk relationships
        assertThat(entity, is(notNullValue()));
        RestDataserviceViewInfo viewInfo = KomodoJsonMarshaller.unmarshall(entity, RestDataserviceViewInfo.class);
        
        assertTrue("CRITERIA".equals(viewInfo.getInfoType()));
        assertEquals(viewInfo.getCriteriaPredicates().size(), 0);
    }
    
    @Test
    public void shouldGenerateJoinCriteria() throws Exception {
        loadVdbs();
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        URI uri = UriBuilder.fromUri(dataservicesUri).path(V1Constants.CRITERIA_FOR_JOIN_TABLES).build();

        // Required parms (tablePath, rhTablePath).
        // Valid entries - should generate join criteria (empty)
        KomodoDataserviceUpdateAttributes updateAttr = new KomodoDataserviceUpdateAttributes();
        updateAttr.setTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/MyPartsVDB_Dynamic/PartsSS/PARTS");
        updateAttr.setRhTablePath("tko:komodo/tko:workspace/"+USER_NAME+"/MyPartsVDB_Dynamic/PartsSS/SUPPLIER_PARTS");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, updateAttr);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        
        // Info returned.  Should contain a single criteria predicate (PART_ID = PART_ID).
        assertThat(entity, is(notNullValue()));
        RestDataserviceViewInfo viewInfo = KomodoJsonMarshaller.unmarshall(entity, RestDataserviceViewInfo.class);

        assertTrue("CRITERIA".equals(viewInfo.getInfoType()));
        assertEquals(1, viewInfo.getCriteriaPredicates().size());
        assertEquals("PART_ID", viewInfo.getCriteriaPredicates().get(0).getLhColumn());
        assertEquals("PART_ID", viewInfo.getCriteriaPredicates().get(0).getRhColumn());
        assertEquals("=", viewInfo.getCriteriaPredicates().get(0).getOperator());
    }
    
    @Test
    public void shouldCloneDataservice() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dataservicesUri )
                                  .path( V1Constants.CLONE_SEGMENT )
                                  .path( DATASERVICE_NAME )
                                  .build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, "newDataservice");

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertThat(entity, is(notNullValue()));

        RestDataservice dataservice = KomodoJsonMarshaller.unmarshall(entity, RestDataservice.class);
        assertNotNull(dataservice);
        
        assertEquals(dataservice.getId(), "newDataservice");
    }
    
    @Test
    public void shouldNotCloneDataservice() throws Exception {
        createDataservice(DATASERVICE_NAME);

        URI dataservicesUri = _uriBuilder.workspaceDataservicesUri();
        final URI uri = UriBuilder.fromUri( dataservicesUri )
                                  .path( V1Constants.CLONE_SEGMENT )
                                  .path( DATASERVICE_NAME )
                                  .build();

        // Attempt to clone using the same service name should fail...
        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, DATASERVICE_NAME);

        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertTrue(entity.contains("cannot be the same"));
    }

}
