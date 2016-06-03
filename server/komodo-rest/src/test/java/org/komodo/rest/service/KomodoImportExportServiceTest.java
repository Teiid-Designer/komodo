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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.FileInputStream;
import java.net.URI;
import java.util.Base64;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.UriBuilder;
import org.junit.Test;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.ImportExportStatus;
import org.komodo.rest.relational.KomodoStorageAttributes;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.spi.repository.DocumentType;
import org.komodo.utils.FileUtils;

public class KomodoImportExportServiceTest extends AbstractKomodoServiceTest {

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
        System.out.println(entity);

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
        String decContent = new String(decBytes);

        try (FileInputStream stream = new FileInputStream(tmpFile)) {
            String tmpFileContent = FileUtils.streamToString(stream);
            assertEquals(tmpFileContent, decContent);
        }
    }
}
