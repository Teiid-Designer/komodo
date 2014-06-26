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
import org.komodo.relational.model.ForeignKey;
import org.komodo.spi.outcome.IOutcome;

/**
 * Test Class to test Table
 * 
 */
public class TestForeignKey {
	
	private static final String FK_NAME = "myFK";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestForeignKey( ) {
		super();
	}
	
	/**
     * Test simple creation
     */
    @Test
    public void testCreate() {
    	ForeignKey fk = RelationalUtil.createForeignKey(FK_NAME);
    	String fkName = fk.getName();
    	int type = fk.getType();
    	
    	assertEquals(FK_NAME, fkName);
    	assertEquals(type, RelationalConstants.TYPES.FK);
    }
    
	/**
     * Test adding columns
     */
    @Test
    public void testAddColumns() {
    	ForeignKey fk = RelationalUtil.createForeignKey(FK_NAME);

    	fk.addColumn(RelationalUtil.createColumn("column1")); //$NON-NLS-1$
    	
    	assertEquals(1, fk.getColumns().size());
    }

	/**
     * Test adding columns
     */
    @Test
    public void testAddRemoveColumns() {
    	ForeignKey fk = RelationalUtil.createForeignKey(FK_NAME);

    	Column col1 = RelationalUtil.createColumn("col1"); //$NON-NLS-1$
    	
    	fk.addColumn(col1);
    	
    	fk.removeColumn(col1);
    	
    	assertEquals(0, fk.getColumns().size());
    }
    
	/**
     * Test validation.  expected result - warning : no columns defined
     */
    @Test
    public void testValidate1() {
    	ForeignKey fk = RelationalUtil.createForeignKey(FK_NAME);
    	
    	IOutcome outcome = fk.validate();
    	
    	assertEquals(IOutcome.Level.ERROR, outcome.getLevel());
    	if(!outcome.getMessage().startsWith("No columns defined for foreign key")) { //$NON-NLS-1$
    		fail("unexpected message"); //$NON-NLS-1$
    	}
    }
        
}
