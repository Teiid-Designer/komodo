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
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.net.URI;
import java.util.Base64;
import java.util.List;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.apache.tika.io.IOUtils;
import org.junit.Test;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.ImportExportStatus;
import org.komodo.rest.relational.KomodoStorageAttributes;
import org.komodo.rest.relational.RestStorageType;
import org.komodo.rest.relational.RestStorageTypeDescriptor;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;

public class KomodoImportExportServiceTest extends AbstractKomodoServiceTest {

    @Test
    public void shouldNotImportVdbBlankPayload() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.IMPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();

        this.response = request(uri, MediaType.APPLICATION_JSON_TYPE).post(Entity.json(storageAttr));
        final String entity = this.response.readEntity(String.class);

        assertTrue(entity.contains("The storage type requested from the import export service is unsupported"));
    }

    @Test
    public void shouldImportVdb() throws Exception {
        Repository repository = getRestApp().getDefaultRepository();
        UnitOfWork uow = repository.createTransaction(
                                                      getClass().getSimpleName() + COLON + "importVdb" + COLON + System.currentTimeMillis(),
                                                      false, null);

        KomodoObject workspace = repository.komodoWorkspace(uow);

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.IMPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        storageAttr.setDocumentType(DocumentType.XML);

        String portfolioCnt = FileUtils.streamToString(TestUtilities.portfolioExample());
        String content = Base64.getEncoder().encodeToString(portfolioCnt.getBytes());
        storageAttr.setContent(content);

        assertFalse(workspace.hasChild(uow, TestUtilities.PORTFOLIO_VDB_NAME));

        this.response = request(uri, MediaType.APPLICATION_JSON_TYPE).post(Entity.json(storageAttr));
        final String entity = this.response.readEntity(String.class);

        assertEquals(Response.Status.OK.getStatusCode(), this.response.getStatus());
        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertFalse(status.hasDownloadable());
        assertEquals(XML, status.getType());

        assertTrue(workspace.hasChild(uow, TestUtilities.PORTFOLIO_VDB_NAME));
    }

    @Test
    public void shouldNotExportVdbInvalidArtifactPath() throws Exception {
        loadVdbs();

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                            .path(V1Constants.EXPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        String artifactPath = "/export/blah";
        storageAttr.setArtifactPath(artifactPath);

        String tmpDirPath = System.getProperty("java.io.tmpdir");
        storageAttr.setParameter("files-home-path-property", tmpDirPath);

        this.response = request(uri, MediaType.APPLICATION_JSON_TYPE).post(Entity.json(storageAttr));
        final String entity = this.response.readEntity(String.class);

        assertTrue(entity.contains("No artifact could be found to export at path"));
    }

    @Test
    public void shouldExportVdb() throws Exception {
        loadVdbs();

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                            .path(V1Constants.EXPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        storageAttr.setArtifactPath("/tko:komodo/tko:workspace/myVDB");

        String tmpDirPath = System.getProperty("java.io.tmpdir");
        storageAttr.setParameter("files-home-path-property", tmpDirPath);

        this.response = request(uri).post(Entity.json(storageAttr));
        final String entity = this.response.readEntity(String.class);
        assertNotNull(entity);

        //
        // Test that the file storage connector really did export the vdb
        //
        File tmpFile = new File(tmpDirPath, "myVDB");
        assertTrue(tmpFile.exists());
        tmpFile.deleteOnExit();

        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertTrue(status.hasDownloadable());
        assertEquals(XML, status.getType());

        String content = status.getContent();
        assertNotNull(content);

        byte[] decBytes = Base64.getDecoder().decode(content);
        String decContent = new String(decBytes) + NEW_LINE;

        FileInputStream stream = null;
        try {
            stream = new FileInputStream(tmpFile);
            String tmpFileContent = FileUtils.streamToString(stream) + NEW_LINE;
            assertEquals(tmpFileContent, decContent);
        } finally {
            IOUtils.closeQuietly(stream);
        }
    }

    @Test
    public void shouldImportDataservice() throws Exception {
        Repository repository = getRestApp().getDefaultRepository();
        UnitOfWork uow = repository.createTransaction(
                                                      getClass().getSimpleName() + COLON + "importDataservice" + COLON + System.currentTimeMillis(),
                                                      false, null);

        KomodoObject workspace = repository.komodoWorkspace(uow);

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.IMPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        storageAttr.setDocumentType(DocumentType.ZIP);

        String dsName = "myService";
        InputStream sampleDsStream = TestUtilities.getResourceAsStream(
                                                                       KomodoImportExportServiceTest.class,
                                                                      "dataservice", "sample-ds.zip");

        byte[] sampleBytes = TestUtilities.streamToBytes(sampleDsStream);
        String content = Base64.getEncoder().encodeToString(sampleBytes);
        storageAttr.setContent(content);

        assertFalse(workspace.hasChild(uow, dsName));

        this.response = request(uri, MediaType.APPLICATION_JSON_TYPE).post(Entity.json(storageAttr));
        final String entity = this.response.readEntity(String.class);
        System.out.println(entity);
        assertEquals(Response.Status.OK.getStatusCode(), this.response.getStatus());

        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertFalse(status.hasDownloadable());
        assertEquals(ZIP, status.getType());

        assertTrue(workspace.hasChild(uow, dsName));
        KomodoObject dataservice = workspace.getChild(uow, dsName);
        assertTrue(dataservice.hasChild(uow, TestUtilities.PORTFOLIO_VDB_NAME));
    }

    @Test
    public void shouldExportDataservice() throws Exception {
        loadVdbs();
        String dsName = "myDataService";

        getRestApp().createDataservice(dsName, true);

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                            .path(V1Constants.EXPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        storageAttr.setArtifactPath("/tko:komodo/tko:workspace/" + dsName);

        String tmpDirPath = System.getProperty("java.io.tmpdir");
        storageAttr.setParameter("files-home-path-property", tmpDirPath);

        this.response = request(uri).post(Entity.json(storageAttr));
        final String entity = this.response.readEntity(String.class);
        assertNotNull(entity);
        assertEquals(Response.Status.OK.getStatusCode(), this.response.getStatus());

        //
        // Test that the file storage connector really did export the data service
        //
        File tmpFile = new File(tmpDirPath, dsName);
        assertTrue(tmpFile.exists());
        tmpFile.deleteOnExit();

        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertTrue(status.hasDownloadable());
        assertEquals(ZIP, status.getType());

        String content = status.getContent();
        assertNotNull(content);

        byte[] decBytes = Base64.getDecoder().decode(content);

        File dsZip = File.createTempFile("DSZip", DOT + ZIP);
        dsZip.deleteOnExit();
        FileUtils.write(decBytes, dsZip);
        TestUtilities.testZipFile(dsZip);
    }

    @Test
    public void shouldGetStorageTypes() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.STORAGE_TYPES).build();

        this.response = request(uri).get();
        final String entity = this.response.readEntity(String.class);
        assertNotNull(entity);
        System.out.println(entity);
        assertEquals(Response.Status.OK.getStatusCode(), this.response.getStatus());

        RestStorageType[] entities = KomodoJsonMarshaller.unmarshallArray(entity, RestStorageType[].class);
        assertNotNull(entities);
        assertEquals(2, entities.length);

        for (RestStorageType type : entities) {
            List<RestStorageTypeDescriptor> descriptors = type.getDescriptors();
            assertNotNull(descriptors);
            assertTrue(descriptors.size() > 0);

            String name = type.getName();
            assertTrue(name.equals("file") || name.equals("git"));
        }
    }
}
