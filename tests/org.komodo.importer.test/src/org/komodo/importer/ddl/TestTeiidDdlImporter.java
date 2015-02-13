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
package org.komodo.importer.ddl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.importer.AbstractImporterTest;
import org.komodo.importer.ImportMessages;
import org.komodo.importer.ImportOptions;
import org.komodo.importer.ImportOptions.ImportType;
import org.komodo.importer.ImportOptions.OptionKeys;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;

/**
 * Test Class to test Teiid DDL import
 *
 */
@SuppressWarnings({"nls", "javadoc"})
public class TestTeiidDdlImporter extends AbstractImporterTest {

    private static final String SEQUENCE_DDL_PATH = ".*\\/ddl:statements";

	private static final String TEIID_MYSQL_ACCTS = "Teiid-MySQLAccounts.ddl";

	private static final String TEIID_FLATFILE = "Teiid-FlatFile.ddl";

	@Override
	protected KomodoObject runImporter(Repository repository, UnitOfWork uow,
	                                                             File file, ImportOptions importOptions,
	                                                             ImportMessages importMessages) {
        DdlImporter importer = new DdlImporter(_repo, uow);
        return importer.importDdl(file, importOptions, importMessages);
	}

	@Override
	protected KomodoObject runImporter(Repository repository, UnitOfWork uow,
	                                                             InputStream inputStream, ImportOptions importOptions,
	                                                             ImportMessages importMessages) {
	    DdlImporter importer = new DdlImporter(_repo, uow);
        return importer.importDdl(inputStream, importOptions, importMessages);
	}

	/**
     * Test Error condition - bad DDL file name supplied
     * Expected Outcome - Error message saying that the supplied file is not found
     */
    @Test
    public void testBadDdlFile() throws Exception {
    	ImportOptions importOptions = new ImportOptions();
    	importOptions.setImportType(ImportType.MODEL);

    	ImportMessages importMessages = new ImportMessages();

    	KomodoObject modelNode = executeImporter(new File("unknown.ddl"), importOptions, importMessages);

    	// No model created
    	assertNull("Failed - expected null model ", modelNode);

    	// Should have 1 error message
    	assertEquals(1, importMessages.getErrorMessages().size());

    	String msg = importMessages.getErrorMessages().get(0);
    	assertEquals("The specified File \"unknown.ddl\" was not found",msg);
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
        importOptions.setImportType(ImportType.MODEL);

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
    	assertEquals("The specified File \"" + tmpFile.getName() + "\" is not readable", msg);
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
        importOptions.setImportType(ImportType.MODEL);

        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject modelNode = executeImporter(tmpFile, importOptions, importMessages);

        // No model created
        assertNull("Failed - expected null model ", modelNode);

        // Should have 1 error message
        assertEquals(1, importMessages.getErrorMessages().size());

        String msg = importMessages.getErrorMessages().get(0);
        assertEquals("The supplied content string is empty", msg);
    }

    private void verifyMySQLAcctsDdl(KomodoObject modelNode) throws Exception {
        KomodoObject ddlStmtsNode = verify(modelNode, StandardDdlLexicon.STATEMENTS_CONTAINER);

        // ----------------------------------
        // Test expected tables exist
        // ----------------------------------
        KomodoObject accountTableNode = verify(ddlStmtsNode, "accounts.ACCOUNT", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject customerTableNode = verify(ddlStmtsNode, "accounts.CUSTOMER", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject holdingsTableNode = verify(ddlStmtsNode, "accounts.HOLDINGS", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject productTableNode = verify(ddlStmtsNode, "accounts.PRODUCT", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);
        KomodoObject subsTableNode = verify(ddlStmtsNode, "accounts.SUBSCRIPTIONS", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_STATEMENT);

        // ----------------------------------------
        // Test expected columns for ACCOUNT table
        // ----------------------------------------
        verify(accountTableNode, "ACCOUNT_ID", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(accountTableNode, "SSN", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(accountTableNode, "STATUS", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(accountTableNode, "TYPE", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(accountTableNode, "DATEOPENED", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(accountTableNode, "DATECLOSED", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for CUSTOMER table
        // ------------------------------------------
        verify(customerTableNode, "SSN", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(customerTableNode, "FIRSTNAME", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(customerTableNode, "LASTNAME", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(customerTableNode, "ST_ADDRESS", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(customerTableNode, "APT_NUMBER", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(customerTableNode, "CITY", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(customerTableNode, "STATE", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(customerTableNode, "ZIPCODE", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(customerTableNode, "PHONE", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for HOLDINGS table
        // ------------------------------------------
        verify(holdingsTableNode, "TRANSACTION_ID", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(holdingsTableNode, "ACCOUNT_ID", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(holdingsTableNode, "PRODUCT_ID", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(holdingsTableNode, "PURCHASE_DATE", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(holdingsTableNode, "SHARES_COUNT", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for PRODUCT table
        // ------------------------------------------
        verify(productTableNode, "ID", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(productTableNode, "SYMBOL", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(productTableNode, "COMPANY_NAME", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // ------------------------------------------
        // Test expected columns for SUBSCRIPTIONS table
        // ------------------------------------------
        verify(subsTableNode, "value", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(subsTableNode, "type", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);
        verify(subsTableNode, "end_date", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // --------------------------------------------
        // Test expected properties on HOLDINGS table
        // --------------------------------------------
        // Expected properties
        KomodoObject nameInSource = verify(holdingsTableNode, "NAMEINSOURCE", JcrConstants.NT_UNSTRUCTURED, StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(nameInSource, StandardDdlLexicon.VALUE, "`accounts`.`HOLDINGS`");

        KomodoObject updateable = verify(holdingsTableNode, "UPDATABLE", JcrConstants.NT_UNSTRUCTURED, StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(updateable, StandardDdlLexicon.VALUE, "TRUE");

        // -------------------------------------------------------------
        // Test expected properties on HOLDINGS.PURCHASE_DATE column
        // -------------------------------------------------------------
        KomodoObject purcharseDateNode = verify(holdingsTableNode, "PURCHASE_DATE", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateTable.TABLE_ELEMENT);

        // Expected properties
        KomodoObject nativeType = verify(purcharseDateNode, "NATIVE_TYPE", JcrConstants.NT_UNSTRUCTURED, StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(nativeType, StandardDdlLexicon.VALUE, "TIMESTAMP");

        nameInSource = verify(purcharseDateNode, "NAMEINSOURCE", JcrConstants.NT_UNSTRUCTURED, StandardDdlLexicon.TYPE_STATEMENT_OPTION);
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
        importOptions.setImportType(ImportType.MODEL);
    	importOptions.setOption(OptionKeys.NAME, TEIID_MYSQL_ACCTS);

    	// Saves Messages during import
    	ImportMessages importMessages = new ImportMessages();

    	KomodoObject modelNode = executeImporter(ddlStream, importOptions,
    	                                                                       importMessages, SEQUENCE_DDL_PATH);

    	// Test that a Model was created
    	assertNotNull("Failed - No Model Created ", modelNode);

    	// Test Model name
    	String modelName = modelNode.getName(null);
    	assertEquals(importOptions.getOption(OptionKeys.NAME), modelName);

    	verifyMySQLAcctsDdl(modelNode);
    }

    @Test
    public void testDdlImport_MySQLAcctsAsSchema() throws Exception {
        InputStream ddlStream = setup(TEIID_MYSQL_ACCTS);

        ImportOptions importOptions = new ImportOptions();
        importOptions.setImportType(ImportType.SCHEMA);
        importOptions.setOption(OptionKeys.NAME, TEIID_MYSQL_ACCTS);

        // Saves Messages during import
        ImportMessages importMessages = new ImportMessages();

        KomodoObject schemaNode = executeImporter(ddlStream, importOptions,
                                                                                 importMessages, SEQUENCE_DDL_PATH);

        // Test that a schema fragment was created
        assertNotNull("Failed - No Schema Created ", schemaNode);
        verifyPrimaryType(schemaNode, KomodoLexicon.Schema.NODE_TYPE);

        // Test Model name
        String schemaName = schemaNode.getName(null);
        assertEquals(importOptions.getOption(OptionKeys.NAME), schemaName);

        verifyMySQLAcctsDdl(schemaNode);
    }

    private void verifyFlatFileDdl(KomodoObject schemaNode) throws Exception {
        KomodoObject ddlStmtsNode = verify(schemaNode, StandardDdlLexicon.STATEMENTS_CONTAINER);

        // ----------------------------------
        // Test expected procedures exist
        // ----------------------------------
        KomodoObject getFilesProcNode = verify(ddlStmtsNode, "getFiles", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);
        verify(ddlStmtsNode, "getTextFiles", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);
        verify(ddlStmtsNode, "saveFile", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT);

        // --------------------------------------------
        // Test getFiles procedure has expected param
        // --------------------------------------------
        verify(getFilesProcNode, "pathAndPattern", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateProcedure.PARAMETER);

        // --------------------------------------------
        // Test getFiles procedure properties
        // --------------------------------------------
        // Expected properties
        KomodoObject description = verify(getFilesProcNode, "ANNOTATION", JcrConstants.NT_UNSTRUCTURED, StandardDdlLexicon.TYPE_STATEMENT_OPTION);
        verifyProperty(description, StandardDdlLexicon.VALUE, "Returns files that match the given path and pattern as BLOBs");

        // ------------------------------------------------
        // Test getFiles procedure has expected resultSet
        // ------------------------------------------------
        KomodoObject resultSet = verify(getFilesProcNode, "resultSet", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateProcedure.RESULT_COLUMNS);

        // -------------------------------------------------------------
        // Test resultSet has expected columns
        // -------------------------------------------------------------
        verify(resultSet, "file", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateProcedure.RESULT_COLUMN);
        verify(resultSet, "filePath", JcrConstants.NT_UNSTRUCTURED, TeiidDdlLexicon.CreateProcedure.RESULT_COLUMN);
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
        importOptions.setImportType(ImportType.MODEL);

    	importOptions.setOption(OptionKeys.NAME, TEIID_FLATFILE);

    	ImportMessages importMessages = new ImportMessages();

        KomodoObject modelNode = executeImporter(ddlStream, importOptions,
                                                                               importMessages, SEQUENCE_DDL_PATH);

    	// Test that a Model was created
    	assertNotNull("Failed - No Model Created ", modelNode);
    	verifyPrimaryType(modelNode, VdbLexicon.Vdb.DECLARATIVE_MODEL);

    	// Test Model name
    	String modelName = modelNode.getName(null);
    	assertEquals(importOptions.getOption(OptionKeys.NAME), modelName);

        verifyFlatFileDdl(modelNode);
    }

    @Test
    public void testDdlImport_FlatFileAsSchema() throws Exception {
        InputStream ddlStream = setup(TEIID_FLATFILE);

        // Options for the import
        ImportOptions importOptions = new ImportOptions();
        importOptions.setImportType(ImportType.SCHEMA);
        importOptions.setOption(OptionKeys.NAME, TEIID_FLATFILE);

        ImportMessages importMessages = new ImportMessages();

        KomodoObject schemaNode = executeImporter(ddlStream, importOptions,
                                                                                  importMessages, SEQUENCE_DDL_PATH);

        // Test that a Model was created
        assertNotNull("Failed - No Schema fragment Created ", schemaNode);
        verifyPrimaryType(schemaNode, KomodoLexicon.Schema.NODE_TYPE);

        // Test Schema name
        String schemaName = schemaNode.getName(null);
        assertEquals(importOptions.getOption(OptionKeys.NAME), schemaName);

        verifyFlatFileDdl(schemaNode);
    }
}
