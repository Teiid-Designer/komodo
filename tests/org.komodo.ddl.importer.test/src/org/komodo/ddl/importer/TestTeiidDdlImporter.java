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
import java.util.List;

import org.junit.Assert;
import org.junit.Test;
import org.komodo.relational.model.Model;

/**
 * Test Class to test Table
 * 
 */
public class TestTeiidDdlImporter extends AbstractDdlImporterTest {
	
	private static final String TEIID_MYSQL_ACCTS = "Teiid-MySQLAccounts.ddl";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestTeiidDdlImporter( ) {
		super();
	}	
	
	/**
     * Test Error - bad DDL file supplied
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
     * Test Error - bad DDL file supplied
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
     * Test Error - empty DDL string supplied
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
     * Test simple creation
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
    	
    	// Test Model child details
    	// Ensure that all of the expected tables exist
    	List<String> tableList = new ArrayList<String>();
    	tableList.add("ACCOUNT"); //$NON-NLS-1$
    	tableList.add("CUSTOMER"); //$NON-NLS-1$
    	tableList.add("HOLDINGS"); //$NON-NLS-1$
    	tableList.add("PRODUCT"); //$NON-NLS-1$
    	tableList.add("SUBSCRIPTIONS"); //$NON-NLS-1$
    	boolean hasTables = TestUtil.hasTables(relationalModel,tableList);
    	if(!hasTables) {
    		Assert.fail("expected tables do not match");   //$NON-NLS-1$
    	}
    	
    	// Do additional property checks on tables
    }
    
}
