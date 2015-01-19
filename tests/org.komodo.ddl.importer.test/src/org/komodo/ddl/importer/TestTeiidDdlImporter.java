/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.ddl.importer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.ddl.importer.ImportOptions.ImportType;
import org.komodo.ddl.importer.ImportOptions.OptionKeys;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;

/**
 * Test Class to test Teiid DDL import
 *
 */
@SuppressWarnings({"nls", "javadoc"})
public class TestTeiidDdlImporter extends AbstractDdlImporterTest {

    private static final String SEQUENCE_DDL_PATH = ".*\\/ddl:statements";

	private static final String TEIID_MYSQL_ACCTS = "Teiid-MySQLAccounts.ddl"; 

	private static final String TEIID_FLATFILE = "Teiid-FlatFile.ddl";

    private UnitOfWork uow; 

	@Before
	public void setup() throws Exception {
	    uow = _repo.createTransaction("test-importer", false, null);
	}

	@After
	public void cleanup() {
	    uow = null;
	}

    private KomodoObject executeImporter(InputStream ddlStream, String name,
                                                                     ImportOptions importOptions,
                                                                     ImportMessages importMessages) throws Exception {
        assertNotNull(_repo);
        assertNotNull(uow);
        assertNotNull(ddlStream);
        assertNotNull(name);
        assertNotNull(importOptions);
        assertNotNull(importMessages);

        CountDownLatch updateLatch = addSequencePathListener(uow, 1, SEQUENCE_DDL_PATH);

        DdlImporter importer = new DefaultDdlImporter(_repo, uow);
        KomodoObject modelNode = importer.importDdl(ddlStream, name, importOptions, importMessages);
        if (importMessages.hasError()) {
            fail(importMessages.errorMessagesToString());
        }

        // Wait for the starting of the repository or timeout of 3 minutes
        updateLatch.await(3, TimeUnit.MINUTES);

        traverse(modelNode);

        return modelNode;
    }

    private KomodoObject executeImporter(File ddlFile, ImportOptions importOptions, ImportMessages importMessages) throws Exception {
        assertNotNull(_repo);
        assertNotNull(uow);
        assertNotNull(ddlFile);
        assertNotNull(importOptions);
        assertNotNull(importMessages);

        CountDownLatch updateLatch = addSequencePathListener(uow, 1, SEQUENCE_DDL_PATH);

        DdlImporter importer = new DefaultDdlImporter(_repo, uow);
        KomodoObject modelNode = importer.importDdl(ddlFile, importOptions, importMessages);
        if (importMessages.hasError()) {
            return modelNode; // test should deal with consequences
        }

        traverse(modelNode);

        // Wait for the starting of the repository or timeout of 3 minutes
        // The timeout will mean the assertion fails
        assertTrue(updateLatch.await(3, TimeUnit.MINUTES));

        return modelNode;
    }

	/**
     * Test Error condition - bad DDL file name supplied
     * Expected Outcome - Error message saying that the supplied file is not found
     */
    @Test
    public void testBadDdlFile() throws Exception {
    	ImportOptions importOptions = new ImportOptions();
    	ImportMessages importMessages = new ImportMessages();

    	KomodoObject modelNode = executeImporter(new File("unknown.ddl"), importOptions, importMessages);

    	// No model created
    	assertNull("Failed - expected null model ", modelNode);

    	// Should have 1 error message
    	assertEquals(1, importMessages.getErrorMessages().size());

    	String msg = importMessages.getErrorMessages().get(0);
    	assertEquals("The specified DDL File \"unknown.ddl\" was not found",msg);
    }

	/**
     * Test Error condition - unreadable DDL file supplied.
     * Expected Outcome - Error Message saying that the supplied file is not readable 
     */
    @Test
    public void testUnreadableDdlFile() throws Exception {
    	InputStream ddlStream = setup(TEIID_MYSQL_ACCTS);

    	File tmpFile = File.createTempFile("unreadableFile", ".ddl");
    	Files.copy(ddlStream, tmpFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
    	tmpFile.deleteOnExit();

    	assertTrue(tmpFile.exists());
    	assertTrue(tmpFile.length() > 0);

    	// Make file unreadable
    	tmpFile.setReadable(false);

    	// Options for the import (default)
    	ImportOptions importOptions = new ImportOptions();
    	// Saves Messages during import
    	ImportMessages importMessages = new ImportMessages();

    	KomodoObject modelNode = executeImporter(tmpFile, importOptions, importMessages);

    	// Set back to readable
    	tmpFile.setReadable(true);

    	// No model created
    	assertNull("Failed - expected null model ", modelNode);

    	// Should have 1 error message
    	assertEquals(1, importMessages.getErrorMessages().size());

    	String msg = importMessages.getErrorMessages().get(0);
    	assertEquals("The specified DDL File \"" + tmpFile.getName() + "\" is not readable", msg);
    }

    /**
     * Test Error condition - empty DDL string supplied
     * Expected Outcome - Error Message saying that the supplied DDL string is empty
     */
    @Test
    public void testEmptyDdlString() throws Exception {
        File tmpFile = File.createTempFile("emptyFile", ".ddl");
        tmpFile.deleteOnExit();

        assertTrue(tmpFile.exists());
        assertEquals(0, tmpFile.length());

        // Options for the import (default)
        ImportOptions importOptions = new ImportOptions();
        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject modelNode = executeImporter(tmpFile, importOptions, importMessages);

        // No model created
        assertNull("Failed - expected null model ", modelNode);

        // Should have 1 error message
        assertEquals(1, importMessages.getErrorMessages().size());

        String msg = importMessages.getErrorMessages().get(0);
        assertEquals("The supplied DDL string is empty", msg);
    }

    private void verifyMySQLAcctsDdl(KomodoObject modelNode) throws Exception {
        KomodoObject ddlStmtsNode = verify(uow, modelNode, StandardDdlLexicon.STATEMENTS_CONTAINER);

        // ----------------------------------
        // Test expected tables exist
        // ----------------------------------
        KomodoObject accountTableNode = verify(uow, ddlStmtsNode, "accounts.ACCOUNT", TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject customerTableNode = verify(uow, ddlStmtsNode, "accounts.CUSTOMER", TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject holdingsTableNode = verify(uow, ddlStmtsNode, "accounts.HOLDINGS", TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject productTableNode = verify(uow, ddlStmtsNode, "accounts.PRODUCT", TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject subsTableNode = verify(uow, ddlStmtsNode, "accounts.SUBSCRIPTIONS", TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);

        // ----------------------------------------
        // Test expected columns for ACCOUNT table
        // ----------------------------------------
        verify(uow, accountTableNode, "ACCOUNT_ID", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, accountTableNode, "SSN", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, accountTableNode, "STATUS", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, accountTableNode, "TYPE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, accountTableNode, "DATEOPENED", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, accountTableNode, "DATECLOSED", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for CUSTOMER table
        // ------------------------------------------
        verify(uow, customerTableNode, "SSN", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, customerTableNode, "FIRSTNAME", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, customerTableNode, "LASTNAME", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, customerTableNode, "ST_ADDRESS", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, customerTableNode, "APT_NUMBER", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, customerTableNode, "CITY", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, customerTableNode, "STATE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, customerTableNode, "ZIPCODE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, customerTableNode, "PHONE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for HOLDINGS table
        // ------------------------------------------
        verify(uow, holdingsTableNode, "TRANSACTION_ID", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, holdingsTableNode, "ACCOUNT_ID", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, holdingsTableNode, "PRODUCT_ID", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, holdingsTableNode, "PURCHASE_DATE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, holdingsTableNode, "SHARES_COUNT", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for PRODUCT table
        // ------------------------------------------
        verify(uow, productTableNode, "ID", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, productTableNode, "SYMBOL", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, productTableNode, "COMPANY_NAME", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for SUBSCRIPTIONS table
        // ------------------------------------------
        verify(uow, subsTableNode, "value", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, subsTableNode, "type", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(uow, subsTableNode, "end_date", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // --------------------------------------------
        // Test expected properties on HOLDINGS table
        // --------------------------------------------
        // Expected properties
        KomodoObject nameInSource = verify(uow, holdingsTableNode, "NAMEINSOURCE", StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(nameInSource, StandardDdlLexicon.VALUE, "`accounts`.`HOLDINGS`");

        KomodoObject updateable = verify(uow, holdingsTableNode, "UPDATABLE", StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(updateable, StandardDdlLexicon.VALUE, "TRUE");

        // -------------------------------------------------------------
        // Test expected properties on HOLDINGS.PURCHASE_DATE column
        // -------------------------------------------------------------
        KomodoObject purcharseDateNode = verify(uow, holdingsTableNode, "PURCHASE_DATE", TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // Expected properties
        KomodoObject nativeType = verify(uow, purcharseDateNode, "NATIVE_TYPE", StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(nativeType, StandardDdlLexicon.VALUE, "TIMESTAMP");

        nameInSource = verify(uow, purcharseDateNode, "NAMEINSOURCE", StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(nameInSource, StandardDdlLexicon.VALUE, "`PURCHASE_DATE`");

        verifyProperty(purcharseDateNode, StandardDdlLexicon.DEFAULT_VALUE, "CURRENT_TIMESTAMP");
        verifyProperty(purcharseDateNode, StandardDdlLexicon.NULLABLE, "NOT NULL");
        verifyProperty(purcharseDateNode, StandardDdlLexicon.DEFAULT_OPTION, "DATETIME");
    }

	/**
     * Test import of Teiid-MySQLAccounts.ddl
     * Expected outcome - successful creation
     */
    @Test
    public void testDdlImport_MySQLAcctsAsModel() throws Exception {
        InputStream ddlStream = setup(TEIID_MYSQL_ACCTS);

    	// Options for the import (default)
    	ImportOptions importOptions = new ImportOptions();
    	// Saves Messages during import
    	ImportMessages importMessages = new ImportMessages();

    	KomodoObject modelNode = executeImporter(ddlStream, TEIID_MYSQL_ACCTS, importOptions, importMessages);

    	// Test that a Model was created
    	assertNotNull("Failed - No Model Created ", modelNode);

    	// Test Model name
    	String modelName = modelNode.getName(null);
    	assertEquals(importOptions.getOption(OptionKeys.MODEL_NAME), modelName);

    	verifyMySQLAcctsDdl(modelNode);
    }

    @Test
    public void testDdlImport_MySQLAcctsAsSchema() throws Exception {
        InputStream ddlStream = setup(TEIID_MYSQL_ACCTS);

        ImportOptions importOptions = new ImportOptions();
        importOptions.setImportType(ImportType.SCHEMA);

        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject schemaNode = executeImporter(ddlStream, TEIID_MYSQL_ACCTS, importOptions, importMessages);

        // Test that a schema fragment was created
        assertNotNull("Failed - No Schema Created ", schemaNode);
        verifyPrimaryType(schemaNode, KomodoLexicon.Schema.NODE_TYPE);

        // Test Model name
        String schemaName = schemaNode.getName(null);
        assertEquals(importOptions.getOption(OptionKeys.SCHEMA_NAME), schemaName);

        verifyMySQLAcctsDdl(schemaNode);
    }

    private void verifyFlatFileDdl(KomodoObject schemaNode) throws Exception {
        KomodoObject ddlStmtsNode = verify(uow, schemaNode, StandardDdlLexicon.STATEMENTS_CONTAINER);

        // ----------------------------------
        // Test expected procedures exist
        // ----------------------------------
        KomodoObject getFilesProcNode = verify(uow, ddlStmtsNode, "getFiles", TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);
        verify(uow, ddlStmtsNode, "getTextFiles", TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);
        verify(uow, ddlStmtsNode, "saveFile", TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);

        // --------------------------------------------
        // Test getFiles procedure has expected param
        // --------------------------------------------
        verify(uow, getFilesProcNode, "pathAndPattern", TeiidDdlLexicon.CreateProcedure.PARAMETER);

        // --------------------------------------------
        // Test getFiles procedure properties
        // --------------------------------------------
        // Expected properties
        KomodoObject description = verify(uow, getFilesProcNode, "ANNOTATION", StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(description, StandardDdlLexicon.VALUE, "Returns files that match the given path and pattern as BLOBs");

        // ------------------------------------------------
        // Test getFiles procedure has expected resultSet
        // ------------------------------------------------
        KomodoObject resultSet = verify(uow, getFilesProcNode, "resultSet", TeiidDdlLexicon.CreateProcedure.RESULT_COLUMNS);

        // -------------------------------------------------------------
        // Test resultSet has expected columns
        // -------------------------------------------------------------
        verify(uow, resultSet, "file", TeiidDdlLexicon.CreateProcedure.RESULT_COLUMN);
        verify(uow, resultSet, "filePath", TeiidDdlLexicon.CreateProcedure.RESULT_COLUMN);
    }

	/**
     * Test import of Teiid-FlatFile.ddl
     * Expected outcome - successful creation
     */
    @Test
    public void testDdlImport_FlatFileAsModel() throws Exception {
    	InputStream ddlStream = setup(TEIID_FLATFILE);

    	// Options for the import (default)
    	ImportOptions importOptions = new ImportOptions();
    	ImportMessages importMessages = new ImportMessages();

        KomodoObject modelNode = executeImporter(ddlStream, TEIID_FLATFILE, importOptions, importMessages);

    	// Test that a Model was created
    	assertNotNull("Failed - No Model Created ", modelNode);
    	verifyPrimaryType(modelNode, KomodoLexicon.VdbModel.NODE_TYPE);

    	// Test Model name
    	String modelName = modelNode.getName(null);
    	assertEquals(importOptions.getOption(OptionKeys.MODEL_NAME), modelName);

        verifyFlatFileDdl(modelNode);
    }

    @Test
    public void testDdlImport_FlatFileAsSchema() throws Exception {
        InputStream ddlStream = setup(TEIID_FLATFILE);

        // Options for the import
        ImportOptions importOptions = new ImportOptions();
        importOptions.setImportType(ImportType.SCHEMA);

        ImportMessages importMessages = new ImportMessages();

        KomodoObject schemaNode = executeImporter(ddlStream, TEIID_FLATFILE, importOptions, importMessages);

        // Test that a Model was created
        assertNotNull("Failed - No Schema fragment Created ", schemaNode);
        verifyPrimaryType(schemaNode, KomodoLexicon.Schema.NODE_TYPE);

        // Test Schema name
        String schemaName = schemaNode.getName(null);
        assertEquals(importOptions.getOption(OptionKeys.SCHEMA_NAME), schemaName);

        verifyFlatFileDdl(schemaNode);
    }
}
