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
import org.komodo.relational.model.RelationalObjectFactory;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.spi.outcome.IOutcome;

/**
 * Test Class to test Table
 * 
 */
public class TestUniqueConstraint {
	
	private static final String UC_NAME = "myUC";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestUniqueConstraint( ) {
		super();
	}
	
	/**
     * Test simple creation
     */
    @Test
    public void testCreate() {
    	UniqueConstraint uc = RelationalObjectFactory.INSTANCE.createUniqueConstraint(UC_NAME);
    	String tName = uc.getName();
    	int type = uc.getType();
    	
    	assertEquals(UC_NAME, tName);
    	assertEquals(type, RelationalConstants.TYPES.UC);
    }
    
	/**
     * Test adding columns
     */
    @Test
    public void testAddColumns() {
    	UniqueConstraint uc = RelationalObjectFactory.INSTANCE.createUniqueConstraint(UC_NAME);
    	
    	uc.addColumn(RelationalObjectFactory.INSTANCE.createColumn("column1")); //$NON-NLS-1$
    	
    	assertEquals(1, uc.getColumns().size());
    }

	/**
     * Test adding columns
     */
    @Test
    public void testAddRemoveColumns() {
    	UniqueConstraint uc = RelationalObjectFactory.INSTANCE.createUniqueConstraint(UC_NAME);

    	Column col1 = RelationalObjectFactory.INSTANCE.createColumn("col1"); //$NON-NLS-1$
    	
    	uc.addColumn(col1);
    	
    	uc.removeColumn(col1);
    	
    	assertEquals(0, uc.getColumns().size());
    }
    
	/**
     * Test validation.  expected result - warning : no columns defined
     */
    @Test
    public void testValidateDefaultUC() {
    	UniqueConstraint uc = RelationalObjectFactory.INSTANCE.createUniqueConstraint(UC_NAME);
    	
    	IOutcome outcome = uc.validate();
    	
    	assertEquals(IOutcome.Level.ERROR, outcome.getLevel());
    	if(!outcome.getMessage().startsWith("No columns defined for unique constraint")) { //$NON-NLS-1$
    		fail("unexpected message"); //$NON-NLS-1$
    	}
    }
        
}
