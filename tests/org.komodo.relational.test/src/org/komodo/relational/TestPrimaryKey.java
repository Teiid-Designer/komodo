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
import org.komodo.relational.model.PrimaryKey;

/**
 * Test Class to test Table
 * 
 */
public class TestPrimaryKey {
	
	private static final String PK_NAME = "myPK";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestPrimaryKey( ) {
		super();
	}
	
	/**
     * Test simple creation
     */
    @Test
    public void testCreate() {
    	PrimaryKey pk = RelationalUtil.createPrimaryKey(PK_NAME);
    	String tName = pk.getName();
    	int type = pk.getType();
    	
    	assertEquals(PK_NAME, tName);
    	assertEquals(type, RelationalConstants.TYPES.PK);
    }
    
	/**
     * Test adding columns
     */
    @Test
    public void testAddColumns() {
    	PrimaryKey pk = RelationalUtil.createPrimaryKey(PK_NAME);

    	pk.addColumn(RelationalUtil.createColumn("column1")); //$NON-NLS-1$
    	
    	assertEquals(1, pk.getColumns().size());
    }

	/**
     * Test adding columns
     */
    @Test
    public void testAddRemoveColumns() {
    	PrimaryKey pk = RelationalUtil.createPrimaryKey(PK_NAME);

    	Column col1 = RelationalUtil.createColumn("col1"); //$NON-NLS-1$
    	
    	pk.addColumn(col1);
    	
    	pk.removeColumn(col1);
    	
    	assertEquals(0, pk.getColumns().size());
    }
    
	/**
     * Test validation.  expected result - warning : no columns defined
     */
    @Test
    public void testValidate1() {
    	PrimaryKey pk = RelationalUtil.createPrimaryKey(PK_NAME);
    	
    	IStatus status = pk.validate();
    	
    	assertEquals(IStatus.ERROR, status.getSeverity());
    	if(!status.getMessage().startsWith("No columns defined for primary key")) { //$NON-NLS-1$
    		fail("unexpected message"); //$NON-NLS-1$
    	}
    }
        
}
