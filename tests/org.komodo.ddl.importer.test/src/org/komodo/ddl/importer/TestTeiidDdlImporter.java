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

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;
import org.komodo.relational.constants.RelationalConstants;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.RelationalObject;

/**
 * Test Class to test Teiid DDL import
 * 
 */
public class TestTeiidDdlImporter extends AbstractDdlImporterTest {
	
	private static final String TEIID_MYSQL_ACCTS = "Teiid-MySQLAccounts.ddl";  //$NON-NLS-1$
	private static final String TEIID_FLATFILE = "Teiid-FlatFile.ddl";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestTeiidDdlImporter( ) {
		super();
	}	
	
	/**
     * Test Error condition - bad DDL file name supplied
     * Expected Outcome - Error message saying that the supplied file is not found
     */
    @Test
    public void testBadDdlFile() {   	
    	// Options for the import (default)
    	ImportOptions importOptions = new ImportOptions();
    	// Saves Messages during import
    	ImportMessages importMessages = new ImportMessages();
    	
    	DdlImportService importer = DefaultDdlImportService.getInstance();
    	Model relationalModel = importer.importDdl(new File("unknown.ddl"),importOptions,importMessages); //$NON-NLS-1$
    	
    	// No model created
    	Assert.assertNull("Failed - expected null model ", relationalModel); //$NON-NLS-1$
    	
    	// Should have 1 error message
    	Assert.assertEquals(1, importMessages.getErrorMessages().size());
    	
    	String msg = importMessages.getErrorMessages().get(0);
    	Assert.assertEquals("The specified DDL File \"unknown.ddl\" was not found",msg); //$NON-NLS-1$
    }
    
	/**
     * Test Error condition - unreadable DDL file supplied.
     * Expected Outcome - Error Message saying that the supplied file is not readable
     */
    @Test
    public void testUnreadableDdlFile() {   	
    	File ddlFile = setup(TEIID_MYSQL_ACCTS);
    	ddlFile.setReadable(false);
    	
    	// Options for the import (default)
    	ImportOptions importOptions = new ImportOptions();
    	// Saves Messages during import
    	ImportMessages importMessages = new ImportMessages();
    	
    	DdlImportService importer = DefaultDdlImportService.getInstance();
    	Model relationalModel = importer.importDdl(ddlFile,importOptions,importMessages);
    	
    	// Set back to readable
    	ddlFile.setReadable(true);
    	
    	// No model created
    	Assert.assertNull("Failed - expected null model ", relationalModel); //$NON-NLS-1$
    	
    	// Should have 1 error message
    	Assert.assertEquals(1, importMessages.getErrorMessages().size());

    	String msg = importMessages.getErrorMessages().get(0);
    	Assert.assertEquals("The specified DDL File \"Teiid-MySQLAccounts.ddl\" is not readable",msg); //$NON-NLS-1$
    }
    
	/**
     * Test Error condition - empty DDL string supplied
     * Expected Outcome - Error Message saying that the supplied DDL string is empty
     */
    @Test
    public void testEmptyDdlString() {   	
    	// Options for the import (default)
    	ImportOptions importOptions = new ImportOptions();
    	// Saves Messages during import
    	ImportMessages importMessages = new ImportMessages();
    	
    	DdlImportService importer = DefaultDdlImportService.getInstance();
    	Model relationalModel = importer.importDdl("",importOptions,importMessages); //$NON-NLS-1$
    	
    	// No model created
    	Assert.assertNull("Failed - expected null model ", relationalModel); //$NON-NLS-1$
    	
    	// Should have 1 error message
    	Assert.assertEquals(1, importMessages.getErrorMessages().size());
    	
    	String msg = importMessages.getErrorMessages().get(0);
    	Assert.assertEquals("The supplied DDL string is empty",msg); //$NON-NLS-1$
    }
	
	/**
     * Test import of Teiid-MySQLAccounts.ddl
     * Expected outcome - successful creation
     */
    @Test
    public void testDdlImport_MySQLAccts() {
    	File ddlFile = setup(TEIID_MYSQL_ACCTS);
    	
    	// Options for the import (default)
    	ImportOptions importOptions = new ImportOptions();
    	// Saves Messages during import
    	ImportMessages importMessages = new ImportMessages();
    	
    	DdlImportService importer = DefaultDdlImportService.getInstance();
    	Model relationalModel = importer.importDdl(ddlFile,importOptions,importMessages);
    	
    	// Test that a Model was created
    	Assert.assertNotNull("Failed - No Model Created ", relationalModel); //$NON-NLS-1$
    	
    	// Test Model name
    	String modelName = relationalModel.getName();
    	Assert.assertEquals(importOptions.getModelName(), modelName);
    	
    	// ----------------------------------
    	// Test expected tables exist
    	// ----------------------------------
    	List<String> tableList = new ArrayList<String>();
    	tableList.add("ACCOUNT"); //$NON-NLS-1$
    	tableList.add("CUSTOMER"); //$NON-NLS-1$
    	tableList.add("HOLDINGS"); //$NON-NLS-1$
    	tableList.add("PRODUCT"); //$NON-NLS-1$
    	tableList.add("SUBSCRIPTIONS"); //$NON-NLS-1$
    	boolean hasTables = TestUtil.childrenMatch(relationalModel,tableList, RelationalConstants.TYPES.TABLE);
    	if(!hasTables) {
    		Assert.fail("expected tables do not match");   //$NON-NLS-1$
    	}
    	
    	// ----------------------------------------
    	// Test expected columns for ACCOUNT table
    	// ----------------------------------------
    	RelationalObject table = relationalModel.getChildWithName("ACCOUNT"); //$NON-NLS-1$
    	List<String> colList = new ArrayList<String>();
    	colList.add("ACCOUNT_ID"); //$NON-NLS-1$
    	colList.add("SSN"); //$NON-NLS-1$
    	colList.add("STATUS"); //$NON-NLS-1$
    	colList.add("TYPE"); //$NON-NLS-1$
    	colList.add("DATEOPENED"); //$NON-NLS-1$
    	colList.add("DATECLOSED"); //$NON-NLS-1$
    	boolean hasCols = TestUtil.childrenMatch(table, colList, RelationalConstants.TYPES.COLUMN);
    	if(!hasCols) {
    		Assert.fail("expected columns do not match");   //$NON-NLS-1$
    	}
    	
    	// ------------------------------------------
    	// Test expected columns for CUSTOMER table
    	// ------------------------------------------
    	table = relationalModel.getChildWithName("CUSTOMER"); //$NON-NLS-1$
    	colList = new ArrayList<String>();
    	colList.add("SSN"); //$NON-NLS-1$
    	colList.add("FIRSTNAME"); //$NON-NLS-1$
    	colList.add("LASTNAME"); //$NON-NLS-1$
    	colList.add("ST_ADDRESS"); //$NON-NLS-1$
    	colList.add("APT_NUMBER"); //$NON-NLS-1$
    	colList.add("CITY"); //$NON-NLS-1$
    	colList.add("STATE"); //$NON-NLS-1$
    	colList.add("ZIPCODE"); //$NON-NLS-1$
    	colList.add("PHONE"); //$NON-NLS-1$
    	hasCols = TestUtil.childrenMatch(table, colList, RelationalConstants.TYPES.COLUMN);
    	if(!hasCols) {
    		Assert.fail("expected columns do not match");   //$NON-NLS-1$
    	}
    	
    	// ------------------------------------------
    	// Test expected columns for HOLDINGS table
    	// ------------------------------------------
    	table = relationalModel.getChildWithName("HOLDINGS"); //$NON-NLS-1$
    	colList = new ArrayList<String>();
    	colList.add("TRANSACTION_ID"); //$NON-NLS-1$
    	colList.add("ACCOUNT_ID"); //$NON-NLS-1$
    	colList.add("PRODUCT_ID"); //$NON-NLS-1$
    	colList.add("PURCHASE_DATE"); //$NON-NLS-1$
    	colList.add("SHARES_COUNT"); //$NON-NLS-1$
    	hasCols = TestUtil.childrenMatch(table, colList, RelationalConstants.TYPES.COLUMN);
    	if(!hasCols) {
    		Assert.fail("expected columns do not match");   //$NON-NLS-1$
    	}
    	
    	// ------------------------------------------
    	// Test expected columns for PRODUCT table
    	// ------------------------------------------
    	table = relationalModel.getChildWithName("PRODUCT"); //$NON-NLS-1$
    	colList = new ArrayList<String>();
    	colList.add("ID"); //$NON-NLS-1$
    	colList.add("SYMBOL"); //$NON-NLS-1$
    	colList.add("COMPANY_NAME"); //$NON-NLS-1$
    	hasCols = TestUtil.childrenMatch(table, colList, RelationalConstants.TYPES.COLUMN);
    	if(!hasCols) {
    		Assert.fail("expected columns do not match");   //$NON-NLS-1$
    	}
    	
    	// ------------------------------------------
    	// Test expected columns for SUBSCRIPTIONS table
    	// ------------------------------------------
    	table = relationalModel.getChildWithName("SUBSCRIPTIONS"); //$NON-NLS-1$
    	colList = new ArrayList<String>();
    	colList.add("value"); //$NON-NLS-1$
    	colList.add("type"); //$NON-NLS-1$
    	colList.add("end_date"); //$NON-NLS-1$
    	hasCols = TestUtil.childrenMatch(table, colList, RelationalConstants.TYPES.COLUMN);
    	if(!hasCols) {
    		Assert.fail("expected columns do not match");   //$NON-NLS-1$
    	}
    	    	
    	// --------------------------------------------
    	// Test expected properties on HOLDINGS table
    	// --------------------------------------------
    	// Expected properties
    	Map<String,String> expectedProps = new HashMap<String,String>();
    	expectedProps.putAll(TestUtil.TABLE_PROPERTY_DEFAULTS);
    	expectedProps.put("NAME", "HOLDINGS"); //$NON-NLS-1$ //$NON-NLS-2$
    	expectedProps.put("DESCRIPTION", null); //$NON-NLS-1$ 
    	expectedProps.put("SUPPORTSUPDATE", "true"); //$NON-NLS-1$ //$NON-NLS-2$
    	expectedProps.put("NAMEINSOURCE", "`accounts`.`HOLDINGS`"); //$NON-NLS-1$ //$NON-NLS-2$
    	
    	// Compare object properties to expected
    	table = relationalModel.getChildWithName("HOLDINGS"); //$NON-NLS-1$
    	String result = TestUtil.compareProperties(table, expectedProps);
    	if(!result.equals("OK")) { //$NON-NLS-1$
    		Assert.fail(result);  
    	}
    	    	
    	// -------------------------------------------------------------
    	// Test expected properties on HOLDINGS.PURCHASE_DATE column
    	// -------------------------------------------------------------
    	List<String> path = new ArrayList<String>();
    	path.add("PURCHASE_DATE"); //$NON-NLS-1$
    	RelationalObject column = table.getChildAtPath(path, RelationalConstants.TYPES.COLUMN);
    	Assert.assertNotNull(column);
    	
    	// Expected properties
    	expectedProps = new HashMap<String,String>();
    	expectedProps.putAll(TestUtil.COLUMN_PROPERTY_DEFAULTS);
    	expectedProps.put("NAME", "PURCHASE_DATE"); //$NON-NLS-1$ //$NON-NLS-2$
    	expectedProps.put("NATIVETYPE", "TIMESTAMP"); //$NON-NLS-1$ //$NON-NLS-2$
    	expectedProps.put("NULLABLE", "NO_NULLS"); //$NON-NLS-1$ //$NON-NLS-2$
    	expectedProps.put("NAMEINSOURCE", "`PURCHASE_DATE`"); //$NON-NLS-1$ //$NON-NLS-2$
    	expectedProps.put("DEFAULTVALUE", "CURRENT_TIMESTAMP"); //$NON-NLS-1$ //$NON-NLS-2$

    	// Compare object properties to expected
    	result = TestUtil.compareProperties(column, expectedProps);
    	if(!result.equals("OK")) { //$NON-NLS-1$ 
    		Assert.fail(result);  
    	}
    }
    
	/**
     * Test import of Teiid-FlatFile.ddl
     * Expected outcome - successful creation
     */
    @Test
    public void testDdlImport_FlatFile() {
    	File ddlFile = setup(TEIID_FLATFILE);
    	
    	// Options for the import (default)
    	ImportOptions importOptions = new ImportOptions();
    	// Saves Messages during import
    	ImportMessages importMessages = new ImportMessages();
    	
    	DdlImportService importer = DefaultDdlImportService.getInstance();
    	Model relationalModel = importer.importDdl(ddlFile,importOptions,importMessages);
    	
    	// Test that a Model was created
    	Assert.assertNotNull("Failed - No Model Created ", relationalModel); //$NON-NLS-1$
    	
    	// Test Model name
    	String modelName = relationalModel.getName();
    	Assert.assertEquals(importOptions.getModelName(), modelName);
    	
    	// ----------------------------------
    	// Test expected procedures exist
    	// ----------------------------------
    	List<String> procList = new ArrayList<String>();
    	procList.add("getFiles"); //$NON-NLS-1$
    	procList.add("getTextFiles"); //$NON-NLS-1$
    	procList.add("saveFile"); //$NON-NLS-1$
    	boolean hasProcs = TestUtil.childrenMatch(relationalModel,procList, RelationalConstants.TYPES.PROCEDURE);
    	if(!hasProcs) {
    		Assert.fail("expected procedures do not match");   //$NON-NLS-1$
    	}
    	
    	// --------------------------------------------
    	// Test getFiles procedure has expected param 
    	// --------------------------------------------
    	RelationalObject proc = relationalModel.getChildWithName("getFiles"); //$NON-NLS-1$
    	List<String> itemList = new ArrayList<String>();
    	itemList.add("pathAndPattern"); //$NON-NLS-1$
    	boolean hasParams = TestUtil.childrenMatch(proc, itemList, RelationalConstants.TYPES.PARAMETER);
    	if(!hasParams) {
    		Assert.fail("expected parameters do not match");   //$NON-NLS-1$
    	}
    	
    	// --------------------------------------------
    	// Test getFiles procedure properties
    	// --------------------------------------------
    	// Expected properties
    	Map<String,String> expectedProps = new HashMap<String,String>();
    	expectedProps.putAll(TestUtil.PROCEDURE_PROPERTY_DEFAULTS);
    	expectedProps.put("NAME", "getFiles"); //$NON-NLS-1$ //$NON-NLS-2$
    	expectedProps.put("NAMEINSOURCE", "getFiles"); //$NON-NLS-1$ //$NON-NLS-2$
    	expectedProps.put("DESCRIPTION", "Returns files that match the given path and pattern as BLOBs"); //$NON-NLS-1$ //$NON-NLS-2$
    	// Compare object properties to expected
    	String result = TestUtil.compareProperties(proc, expectedProps);
    	if(!result.equals("OK")) { //$NON-NLS-1$ 
    		Assert.fail(result);  
    	}

    	// ------------------------------------------------
    	// Test getFiles procedure has expected resultSet
    	// ------------------------------------------------
    	itemList.clear();
    	itemList.add("getFiles"); //$NON-NLS-1$
    	boolean hasRS = TestUtil.childrenMatch(proc, itemList, RelationalConstants.TYPES.RESULT_SET);
    	if(!hasRS) {
    		Assert.fail("expected result set does not match");   //$NON-NLS-1$
    	}
    	
    	// -------------------------------------------------------------
    	// Test resultSet has expected columns
    	// -------------------------------------------------------------
    	List<String> path = new ArrayList<String>();
    	path.add("getFiles"); //$NON-NLS-1$
    	RelationalObject resultSet = proc.getChildAtPath(path, RelationalConstants.TYPES.RESULT_SET);
    	Assert.assertNotNull(resultSet);
    	
    	itemList.clear();
    	itemList.add("file"); //$NON-NLS-1$
    	itemList.add("filePath"); //$NON-NLS-1$
    	boolean hasCols = TestUtil.childrenMatch(resultSet, itemList, RelationalConstants.TYPES.COLUMN);
    	if(!hasCols) {
    		Assert.fail("expected columns do not match");   //$NON-NLS-1$
    	}
    	
    	// --------------------------------------------
    	// Test procedure resultSet properties
    	// --------------------------------------------
    	// Expected properties
    	expectedProps.clear();
    	expectedProps.putAll(TestUtil.RESULTSET_PROPERTY_DEFAULTS);
    	expectedProps.put("NAME", "getFiles"); //$NON-NLS-1$ //$NON-NLS-2$
    	expectedProps.put("NAMEINSOURCE", "getFiles"); //$NON-NLS-1$ //$NON-NLS-2$
    	expectedProps.put("DESCRIPTION", "Returns files that match the given path and pattern as BLOBs"); //$NON-NLS-1$ //$NON-NLS-2$
    	// Compare object properties to expected
    	result = TestUtil.compareProperties(resultSet, expectedProps);
    	if(!result.equals("OK")) { //$NON-NLS-1$ 
    		Assert.fail(result);  
    	}
    	
    }
}
