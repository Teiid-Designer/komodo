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
package org.komodo.relational.importer.connection;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;

import org.junit.Test;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.relational.AbstractImporterTest;
import org.komodo.relational.connection.Connection;
import org.komodo.relational.importer.connection.ConnectionImporter;
import org.komodo.relational.workspace.WorkspaceManager;
import org.komodo.repository.SynchronousCallback;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork.State;
import org.komodo.test.utils.TestUtilities;
import org.teiid.modeshape.sequencer.dataservice.lexicon.DataVirtLexicon;

@SuppressWarnings( { "javadoc",
                     "nls" } )
public class TestConnectionImporter  extends AbstractImporterTest {

    private static final String MYSQL_USSTATES_TDS = "mysql-usstates-connection.xml";

    private static final String TDS_NAME = "MySqlPool";

    private static final String MYSQL_DRIVER_NAME = "mysql-connector-java-5.1.39-bin.jarcom.mysql.jdbc.Driver_5_1";

    @Override
    protected void runImporter(Repository repository,
                                                                 File file, KomodoObject parentObject, ImportOptions importOptions,
                                                                 ImportMessages importMessages) throws Exception {
        ConnectionImporter importer = new ConnectionImporter(_repo);
        importer.importDS(getTransaction(), file, parentObject, importOptions, importMessages);
    }

    @Override
    protected void runImporter(Repository repository,
                                                                 InputStream inputStream, KomodoObject parentObject,
                                                                 ImportOptions importOptions,
                                                                 ImportMessages importMessages) throws Exception {
        ConnectionImporter importer = new ConnectionImporter(_repo);
        importer.importDS(getTransaction(), inputStream, parentObject, importOptions, importMessages);
    }

    // Commit Transaction and handle Importer errors, adding to import messages.  Then start a new transaction.
    private void commitHandleErrors(ImportMessages importMessages, State expectedState) throws Exception {
        // cache current callback as a new one will be created when the commit occurs
        final SynchronousCallback testCallback = this.callback;

        if (! importMessages.getErrorMessages().isEmpty()) {
            fail("Import has error messages prior to commit: " + NEW_LINE + importMessages.errorMessagesToString());
        }

        // Commit the transaction and handle any import exceptions
        commit(expectedState);

        if ( testCallback.hasError() ) {
            importMessages.addErrorMessage( testCallback.error() );
        }
    }

    // Commit Transaction and handle Importer errors, adding to import messages.  Then start a new transaction.
    private void commitHandleErrors(ImportMessages importMessages) throws Exception {
        commitHandleErrors(importMessages, State.COMMITTED);
    }

    /**
     * Test Error condition - bad CONNECTION file name supplied
     * Expected Outcome - Error message saying that the supplied file is not found
     */
    @Test
    public void testBadTdsFile() throws Exception {
        ImportMessages importMessages = new ImportMessages();
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        executeImporter(new File("unknown.tds"), workspace, new ImportOptions(), importMessages);

        // Verify no children created
        KomodoObject[] children = workspace.getChildren(getTransaction());
        assertEquals(0,children.length);

        // Should have 1 error message
        assertEquals(1, importMessages.getErrorMessages().size());

        String msg = importMessages.getErrorMessages().get(0);
        assertEquals("The specified File \"unknown.tds\" was not found",msg);
    }

    /**
     * Test Error condition - unreadable CONNECTION file supplied.
     * Expected Outcome - Error Message saying that the supplied file is not readable
     */
    @Test
    public void testUnreadableTDSFile() throws Exception {
        InputStream tdsStream = TestUtilities.getResourceAsStream(getClass(),
                                                                  CONNECTION_DIRECTORY, MYSQL_USSTATES_TDS);

        File tmpFile = File.createTempFile("unreadableFile", ".tds");
        Files.copy(tdsStream, tmpFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
        tmpFile.deleteOnExit();

        assertTrue(tmpFile.exists());
        assertTrue(tmpFile.length() > 0);

        // Make file unreadable
        if (! tmpFile.setReadable(false)) {
            //
            // Cannot set the permissions on the file.
            // Some platforms do not allow this, eg. Windows
            // Abort the test
            //
            return;
        }

        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        executeImporter(tmpFile, workspace, new ImportOptions(), importMessages);

        // Set back to readable
        tmpFile.setReadable(true);

        // Verify no children created
        KomodoObject[] children = workspace.getChildren(getTransaction());
        assertEquals(0,children.length);

        // Should have 1 error message
        assertEquals(1, importMessages.getErrorMessages().size());

        String msg = importMessages.getErrorMessages().get(0);
        assertEquals("The specified File \"" + tmpFile.getName() + "\" is not readable", msg);
    }

    /**
     * Test Error condition - empty CONNECTION string supplied
     * Expected Outcome - Error Message saying that the supplied CONNECTION string is empty
     */
    @Test
    public void testEmptyTDSString() throws Exception {
        File tmpFile = File.createTempFile("emptyFile", ".tds");
        tmpFile.deleteOnExit();

        assertTrue(tmpFile.exists());
        assertEquals(0, tmpFile.length());

        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        executeImporter(tmpFile, workspace, new ImportOptions(), importMessages);

        // Verify no children created
        KomodoObject[] children = workspace.getChildren(getTransaction());
        assertEquals(0,children.length);

        // Should have 1 error message
        assertEquals(1, importMessages.getErrorMessages().size());

        String msg = importMessages.getErrorMessages().get(0);
        assertEquals("The supplied content string is empty", msg);
    }

    // Verifies a MySQL connection node
    private void verifyMySQLUSStatesTDS(KomodoObject dsNode) throws Exception {
        verifyProperty(dsNode, DataVirtLexicon.Connection.DRIVER_NAME, MYSQL_DRIVER_NAME);
        verifyProperty(dsNode, DataVirtLexicon.Connection.JNDI_NAME, "java:/MySqlDS");
        verifyProperty(dsNode, "connection-url", "jdbc:mysql://db4free.net:3306/usstates");
        verifyProperty(dsNode, "user-name", "komodo");
        verifyProperty(dsNode, "password", "XUMz4vBKuA2v");
        verifyProperty(dsNode, 
                       DataVirtLexicon.Connection.TYPE, 
                       org.teiid.modeshape.sequencer.dataservice.Connection.Type.JDBC.name());
    }

    /**
     * Test import of mysql-usstates-connection.xml
     * Expected outcome - successful creation
     */
    @Test
    public void testTdsImport_MySQLUSStates() throws Exception {
        InputStream tdsStream = TestUtilities.getResourceAsStream(getClass(),
                                                                  CONNECTION_DIRECTORY, MYSQL_USSTATES_TDS);

        ImportOptions importOptions = new ImportOptions();
        ImportMessages importMessages = new ImportMessages();
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        executeImporter(tdsStream, workspace, importOptions, importMessages);

        // Commit the transaction and handle any import exceptions
        commitHandleErrors(importMessages);

        // Retrieve connection after import
        WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Connection[] connections = mgr.findConnections(getTransaction());
        assertEquals(1, connections.length);

        Connection connection = connections[0];
        // Test connection name
        String connectionName = connection.getName(getTransaction());
        assertEquals(TDS_NAME, connectionName);

        verifyMySQLUSStatesTDS(connection);
    }

    /**
     * Imports MySQL USStates CONNECTION, then re-imports.  import of CONNECTION into a parent
     * does a full replace of the existing content...
     * Expected outcome - successful creation with replacement of first import content
     */
    @Test
    public void testTdsImportModelThenReimport() throws Exception {
        // Import the original connection from tds
        testTdsImport_MySQLUSStates();
        commit();

        testTdsImport_MySQLUSStates();
        commit();

        WorkspaceManager mgr = WorkspaceManager.getInstance(_repo, getTransaction());
        Connection[] connections = mgr.findConnections(getTransaction());
        assertEquals(1, connections.length);

        Connection connection = connections[0];
        // Test connection name
        String connectionName = connection.getName(getTransaction());
        assertEquals(TDS_NAME, connectionName);

        verifyMySQLUSStatesTDS(connection);
    }
}
