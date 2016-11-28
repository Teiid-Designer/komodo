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
import java.util.Arrays;
import java.util.Base64;
import java.util.List;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import org.apache.tika.io.IOUtils;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.lib.Constants;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevTree;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.treewalk.TreeWalk;
import org.jboss.resteasy.client.ClientRequest;
import org.jboss.resteasy.client.ClientResponse;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.relational.dataservice.Dataservice;
import org.komodo.relational.model.Model;
import org.komodo.relational.vdb.Vdb;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.rest.KomodoRestV1Application.V1Constants;
import org.komodo.rest.relational.AbstractKomodoServiceTest;
import org.komodo.rest.relational.json.KomodoJsonMarshaller;
import org.komodo.rest.relational.response.ImportExportStatus;
import org.komodo.rest.relational.response.KomodoStorageAttributes;
import org.komodo.rest.relational.response.RestStorageType;
import org.komodo.rest.relational.response.RestStorageTypeDescriptor;
import org.komodo.spi.repository.DocumentType;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.test.utils.TestUtilities;
import org.komodo.utils.FileUtils;
import org.komodo.utils.StringUtils;
import org.teiid.modeshape.sequencer.vdb.lexicon.VdbLexicon;

@SuppressWarnings({"deprecation"})
public class KomodoImportExportServiceTest extends AbstractKomodoServiceTest {

    private File myGitDir;

    private Git myGit;

    private File gitRepoDest;

    private void initGitRepository() throws Exception {
        String tmpDirPath = System.getProperty("java.io.tmpdir");
        File tmpDir = new File(tmpDirPath);

        long timestamp = System.currentTimeMillis();
        myGitDir = new File(tmpDir, "mygit-" + timestamp);
        assertTrue(myGitDir.mkdir());

        myGit = Git.init()
                            .setDirectory(myGitDir)
                            .setBare(true)
                            .call();
        assertNotNull(myGit);

        // Need to initialise the master branch
        // as jgit does not automatically create on
        File seedDir = new File(tmpDir, "seedDir-" + timestamp);
        Git seedGit = Git.cloneRepository()
                                    .setURI(myGitDir.getAbsolutePath())
                                    .setDirectory(seedDir)
                                    .call();

        File tweetVdbFile = new File(seedDir, TestUtilities.TWEET_EXAMPLE_NAME + TestUtilities.TWEET_EXAMPLE_SUFFIX);
        assertTrue(tweetVdbFile.createNewFile());
        FileUtils.write(TestUtilities.tweetExample(), tweetVdbFile);
        assertTrue(tweetVdbFile.length() > 0);

        File usStatesZipFile = new File(tmpDir, TestUtilities.US_STATES_DATA_SERVICE_NAME + ZIP_SUFFIX);
        FileUtils.write(TestUtilities.usStatesDataserviceExample(), usStatesZipFile);
        try (FileInputStream fis = new FileInputStream(usStatesZipFile)) {
            File usStatesDir = new File(seedDir, TestUtilities.US_STATES_DATA_SERVICE_NAME);
            usStatesDir.mkdir();
            FileUtils.zipExtract(fis, usStatesDir);
            usStatesZipFile.delete();
        }

        seedGit.add()
                    .addFilepattern(DOT)
                    .call();
        seedGit.commit().setMessage("First Commit").call();
        seedGit.push().call();

        FileUtils.removeDirectoryAndChildren(seedDir);

        //
        // Local git repository
        //
        String dirName = System.currentTimeMillis() + HYPHEN;
        gitRepoDest = new File(FileUtils.tempDirectory(), dirName);
        gitRepoDest.mkdir();
    }

    private void destroyGitRepository() throws Exception {
        if (gitRepoDest != null)
            FileUtils.removeDirectoryAndChildren(gitRepoDest);

        if (myGit != null)
            myGit.close();

        if (myGitDir != null)
            FileUtils.removeDirectoryAndChildren(myGitDir);
    }

    @Before
    public void setup() throws Exception {
        initGitRepository();
    }

    @After
    public void tearDown() throws Exception {
        destroyGitRepository();
    }

    @Test
    public void shouldNotImportVdbBlankPayload() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.IMPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);
        ClientResponse<String> response = request.post(String.class);
        final String entity = response.getEntity();

        assertTrue(entity.contains("The storage type requested from the import export service is unsupported"));
    }

    @Test
    public void shouldImportVdb() throws Exception {
        Repository repository = getRestApp().getDefaultRepository();
        UnitOfWork uow = repository.createTransaction(USER_NAME,
                                                      getClass().getSimpleName() + COLON + "importVdb" + COLON + System.currentTimeMillis(),
                                                      false, null);

        KomodoObject workspace = repository.komodoWorkspace(uow);

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.IMPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        storageAttr.setDocumentType(DocumentType.VDB_XML);

        String portfolioCnt = FileUtils.streamToString(TestUtilities.portfolioExample());
        String content = Base64.getEncoder().encodeToString(portfolioCnt.getBytes());
        storageAttr.setContent(content);

        assertFalse(workspace.hasChild(uow, TestUtilities.PORTFOLIO_VDB_NAME));

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertFalse(status.hasDownloadable());
        assertEquals(VDB_DEPLOYMENT_SUFFIX, status.getType());

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

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();

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
        storageAttr.setArtifactPath("/tko:komodo/tko:workspace/" + USER_NAME + "/myVDB");

        String tmpDirPath = System.getProperty("java.io.tmpdir");
        storageAttr.setParameter("files-home-path-property", tmpDirPath);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertNotNull(entity);

        //
        // Test that the file storage connector really did export the vdb
        //
        File tmpFile = new File(tmpDirPath, "myVDB" + VDB_DEPLOYMENT_SUFFIX);
        assertTrue(tmpFile.exists());
        tmpFile.deleteOnExit();

        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertTrue(status.hasDownloadable());
        assertEquals(VDB_DEPLOYMENT_SUFFIX, status.getType());

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
        UnitOfWork uow = repository.createTransaction(USER_NAME,
                                                      getClass().getSimpleName() + COLON + "importDataservice" + COLON + System.currentTimeMillis(),
                                                      false, null);

        KomodoObject workspace = repository.komodoWorkspace(uow);

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.IMPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        storageAttr.setDocumentType(DocumentType.ZIP);

        String dsName = "MyDataService";
        InputStream sampleDsStream = TestUtilities.sampleDataserviceExample();

        byte[] sampleBytes = TestUtilities.streamToBytes(sampleDsStream);
        String content = Base64.getEncoder().encodeToString(sampleBytes);
        storageAttr.setContent(content);

        assertFalse(workspace.hasChild(uow, dsName));

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        System.out.println(entity);
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertFalse(status.hasDownloadable());
        assertEquals(ZIP, status.getType());

        assertTrue(workspace.hasChild(uow, dsName));
        KomodoObject dataservice = workspace.getChild(uow, dsName);
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Dataservice ds = mgr.resolve(uow, dataservice, Dataservice.class);
        assertNotNull(ds);

        //
        // Due to a hiccup, the portfolio vdb file entry in the dataservice zip is actually capitalized
        //
        Vdb[] vdbs = ds.getVdbs(uow, StringUtils.toCamelCase(TestUtilities.PORTFOLIO_VDB_FILE));
        assertTrue(vdbs.length == 1);

        String vdbName = ds.getServiceVdb(uow).getVdbName( uow );
        assertEquals("DynamicProducts", vdbName);
    }

    @Test
    public void shouldImportUSDataservice() throws Exception {
        Repository repository = getRestApp().getDefaultRepository();
        UnitOfWork uow = repository.createTransaction(USER_NAME,
                                                      getClass().getSimpleName() + COLON + "importDataservice" + COLON + System.currentTimeMillis(),
                                                      false, null);

        KomodoObject workspace = repository.komodoWorkspace(uow);

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.IMPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        storageAttr.setDocumentType(DocumentType.ZIP);

        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        InputStream usDsStream = TestUtilities.usStatesDataserviceExample();

        byte[] usBytes = TestUtilities.streamToBytes(usDsStream);
        String content = Base64.getEncoder().encodeToString(usBytes);
        storageAttr.setContent(content);

        assertFalse(workspace.hasChild(uow, dsName));

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        System.out.println(entity);
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertFalse(status.hasDownloadable());
        assertEquals(ZIP, status.getType());

        assertTrue(workspace.hasChild(uow, dsName));
        KomodoObject dataservice = workspace.getChild(uow, dsName);
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Dataservice ds = mgr.resolve(uow, dataservice, Dataservice.class);
        assertNotNull(ds);

        Vdb vdb = ds.getServiceVdb(uow);
        assertNotNull(vdb);

        String vdbName = vdb.getVdbName( uow );
        assertEquals("usstates", vdbName);
    }

    @Test
    public void shouldImportUSDataserviceFromGit() throws Exception {
        String dsName = TestUtilities.US_STATES_DATA_SERVICE_NAME;
        Repository repository = getRestApp().getDefaultRepository();
        UnitOfWork uow = repository.createTransaction(USER_NAME,
                                                      getClass().getSimpleName() + COLON + "importDataservice" + COLON + System.currentTimeMillis(),
                                                      false, null);

        KomodoObject workspace = repository.komodoWorkspace(uow);

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.IMPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("git");
        storageAttr.setDocumentType(DocumentType.ZIP);
        storageAttr.setParameter("repo-path-property", "file://" + myGitDir);
        storageAttr.setParameter("file-path-property", dsName);
        storageAttr.setParameter("author-name-property", "user");
        storageAttr.setParameter("author-email-property", "user@user.com");

        assertFalse(workspace.hasChild(uow, dsName));

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        System.out.println(entity);
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertFalse(status.hasDownloadable());
        assertEquals(ZIP, status.getType());

        assertTrue(workspace.hasChild(uow, dsName));
        KomodoObject dataservice = workspace.getChild(uow, dsName);
        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        Dataservice ds = mgr.resolve(uow, dataservice, Dataservice.class);
        assertNotNull(ds);

        Vdb vdb = ds.getServiceVdb(uow);
        assertNotNull(vdb);

        String vdbName = vdb.getVdbName( uow );
        assertEquals("usstates", vdbName);
    }

    @Test
    public void shouldExportDataserviceToFile() throws Exception {
        loadVdbs();
        String dsName = "MyDataService";

        getRestApp().createDataservice(dsName, true, USER_NAME);

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                            .path(V1Constants.EXPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        storageAttr.setArtifactPath("/tko:komodo/tko:workspace/" + USER_NAME + FORWARD_SLASH + dsName);

        String tmpDirPath = System.getProperty("java.io.tmpdir");
        storageAttr.setParameter("files-home-path-property", tmpDirPath);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertNotNull(entity);
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        //
        // Test that the file storage connector really did export the data service
        //
        File tmpFile = new File(tmpDirPath, dsName + ZIP_SUFFIX);
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

        File dsZip = File.createTempFile("DSZip", ZIP_SUFFIX);
        dsZip.deleteOnExit();
        FileUtils.write(decBytes, dsZip);
        TestUtilities.testZipFile(dsZip);
    }

    @Test
    public void shouldExportDataserviceToGit() throws Exception {
        loadStatesDataService();

        String dsName = "UsStatesService";
        List<String> zipEntries = TestUtilities.zipEntries(dsName, TestUtilities.usStatesDataserviceExample());
        System.out.println(zipEntries);

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                            .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                            .path(V1Constants.EXPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("git");
        storageAttr.setArtifactPath("/tko:komodo/tko:workspace/" + USER_NAME + FORWARD_SLASH + dsName);

        storageAttr.setParameter("repo-path-property", "file://" + myGitDir);
        storageAttr.setParameter("repo-dest-property", gitRepoDest.getAbsolutePath());
        storageAttr.setParameter("author-name-property", "user");
        storageAttr.setParameter("author-email-property", "user@user.com");

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();
        assertNotNull(entity);
        System.out.println(entity);
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertFalse(status.hasDownloadable());
        assertEquals(ZIP, status.getType());

        //
        // Test that the git storage connector really did export the data service
        //
        org.eclipse.jgit.lib.Repository repository = myGit.getRepository();
        ObjectId commitId = repository.resolve(Constants.HEAD);
        try (RevWalk revWalk = new RevWalk(repository)) {
            RevCommit commit = revWalk.parseCommit(commitId);
            RevTree tree = commit.getTree();

            try (TreeWalk treeWalk = new TreeWalk(repository)) {
                treeWalk.addTree(tree);
                treeWalk.setRecursive(false);
                while (treeWalk.next()) {
                    zipEntries.remove(treeWalk.getPathString());

                    if (treeWalk.isSubtree())
                        treeWalk.enterSubtree();
                }
            }

            //
            // All entries in the original zip have been extracted
            // and pushed to the git repository
            //
            assertTrue("Remaining entries: " + Arrays.toString(zipEntries.toArray(new String[0])), zipEntries.isEmpty());
        }
    }

    @Test
    public void shouldGetStorageTypes() throws Exception {
        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.STORAGE_TYPES).build();

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        ClientResponse<String> response = request.get(String.class);
        final String entity = response.getEntity();
        assertNotNull(entity);
        System.out.println(entity);
        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());

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
    
    @Test
    public void shouldImportDdl() throws Exception {
        String VDB_NAME = "testVDB";
        String MODEL_NAME = "testModel";
        createVdbModel(VDB_NAME, MODEL_NAME);
        
        Repository repository = getRestApp().getDefaultRepository();
        UnitOfWork uow = repository.createTransaction(USER_NAME,
                                                      getClass().getSimpleName() + COLON + "importVdb" + COLON + System.currentTimeMillis(),
                                                      false, null);

        WorkspaceManager mgr = WorkspaceManager.getInstance(repository, uow);
        KomodoObject kObj = mgr.getChild(uow, VDB_NAME, VdbLexicon.Vdb.VIRTUAL_DATABASE);
        Vdb testVdb = Vdb.RESOLVER.resolve(uow, kObj);
        Model[] models = testVdb.getModels(uow, MODEL_NAME);
        Model testModel = models[0];

        URI uri = UriBuilder.fromUri(_uriBuilder.baseUri())
                                        .path(V1Constants.IMPORT_EXPORT_SEGMENT)
                                        .path(V1Constants.IMPORT).build();

        KomodoStorageAttributes storageAttr = new KomodoStorageAttributes();
        storageAttr.setStorageType("file");
        storageAttr.setDocumentType(DocumentType.DDL);
        String modelPath = testModel.getAbsolutePath();
        storageAttr.setArtifactPath(modelPath);

        String patientsDdlCnt = FileUtils.streamToString(TestUtilities.patientsDdl());
        String content = Base64.getEncoder().encodeToString(patientsDdlCnt.getBytes());
        storageAttr.setContent(content);

        ClientRequest request = request(uri, MediaType.APPLICATION_JSON_TYPE);
        addJsonConsumeContentType(request);
        addBody(request, storageAttr);
        ClientResponse<String> response = request.post(String.class);

        final String entity = response.getEntity();

        assertEquals(Response.Status.OK.getStatusCode(), response.getStatus());
        ImportExportStatus status = KomodoJsonMarshaller.unmarshall(entity, ImportExportStatus.class);
        assertNotNull(status);

        assertTrue(status.isSuccess());
        assertFalse(status.hasDownloadable());
        assertEquals(DDL, status.getType());
        
        assertTrue(testModel.hasChild(uow, "vdbwebtest.ER_VISIT"));
    }

}
