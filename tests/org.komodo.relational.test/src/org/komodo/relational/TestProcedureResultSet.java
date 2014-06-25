/*
 * Copyright 2014 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.relational;

import org.komodo.core.IStatus;
import org.komodo.relational.constants.RelationalConstants;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ProcedureResultSet;

/**
 * Test Class to test Table
 * 
 */
public class TestProcedureResultSet {
	
	private static final String RESULT_SET_NAME = "myResultSet";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestProcedureResultSet( ) {
		super();
	}
	
	/**
     * Test simple creation
     */
    @Test
    public void testCreate() {
    	ProcedureResultSet rs = RelationalUtil.createProcedureResultSet(RESULT_SET_NAME);
    	String tName = rs.getName();
    	int type = rs.getType();
    	
    	assertEquals(RESULT_SET_NAME, tName);
    	assertEquals(type, RelationalConstants.TYPES.RESULT_SET);
    }
    
	/**
     * Test adding columns
     */
    @Test
    public void testAddColumns() {
    	ProcedureResultSet rs = RelationalUtil.createProcedureResultSet(RESULT_SET_NAME);
    	
    	rs.addColumn(RelationalUtil.createColumn("column1")); //$NON-NLS-1$
    	
    	assertEquals(1, rs.getColumns().size());
    }

	/**
     * Test adding columns
     */
    @Test
    public void testAddRemoveColumns() {
    	ProcedureResultSet rs = RelationalUtil.createProcedureResultSet(RESULT_SET_NAME);

    	Column col1 = RelationalUtil.createColumn("col1"); //$NON-NLS-1$
    	
    	rs.addColumn(col1);
    	
    	rs.removeColumn(col1);
    	
    	assertEquals(0, rs.getColumns().size());
    }
    
	/**
     * Test validation.  expected result - warning : no columns defined
     */
    @Test
    public void testValidate1() {
    	ProcedureResultSet rs = RelationalUtil.createProcedureResultSet(RESULT_SET_NAME);
    	
    	IStatus status = rs.validate();
    	
    	assertEquals(IStatus.WARNING, status.getSeverity());
    	assertEquals("No columns defined for table", status.getMessage()); //$NON-NLS-1$
    }
        
}
