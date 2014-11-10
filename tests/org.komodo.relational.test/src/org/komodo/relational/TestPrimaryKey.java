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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;
import org.komodo.relational.constants.RelationalConstants;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.RelationalObjectFactory;
import org.komodo.spi.outcome.Outcome;

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
    	PrimaryKey pk = RelationalObjectFactory.INSTANCE.createPrimaryKey(PK_NAME);
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
    	PrimaryKey pk = RelationalObjectFactory.INSTANCE.createPrimaryKey(PK_NAME);

    	pk.addColumn(RelationalObjectFactory.INSTANCE.createColumn("column1")); //$NON-NLS-1$
    	
    	assertEquals(1, pk.getColumns().size());
    }

	/**
     * Test adding columns
     */
    @Test
    public void testAddRemoveColumns() {
    	PrimaryKey pk = RelationalObjectFactory.INSTANCE.createPrimaryKey(PK_NAME);

    	Column col1 = RelationalObjectFactory.INSTANCE.createColumn("col1"); //$NON-NLS-1$
    	
    	pk.addColumn(col1);
    	
    	pk.removeColumn(col1);
    	
    	assertEquals(0, pk.getColumns().size());
    }
    
	/**
     * Test validation.  expected result - warning : no columns defined
     */
    @Test
    public void testValidateDefaultPK() {
    	PrimaryKey pk = RelationalObjectFactory.INSTANCE.createPrimaryKey(PK_NAME);
    	
    	Outcome outcome = pk.validate();
    	
    	assertEquals(Outcome.Level.ERROR, outcome.getLevel());
    	if(!outcome.getMessage().startsWith("No columns defined for primary key")) { //$NON-NLS-1$
    		fail("unexpected message"); //$NON-NLS-1$
    	}
    }
        
}
