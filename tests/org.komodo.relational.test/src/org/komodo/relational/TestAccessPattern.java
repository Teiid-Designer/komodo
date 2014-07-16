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

import org.junit.Test;
import org.komodo.relational.constants.RelationalConstants;
import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.RelationalObjectFactory;
import org.komodo.spi.outcome.IOutcome;

/**
 * Test Class to test Table
 * 
 */
public class TestAccessPattern {
	
	private static final String AP_NAME = "myAP";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestAccessPattern( ) {
		super();
	}
	
	/**
     * Test simple creation
     */
    @Test
    public void testCreate() {
    	AccessPattern ap = RelationalObjectFactory.INSTANCE.createAccessPattern(AP_NAME);
    	String tName = ap.getName();
    	int type = ap.getType();
    	
    	assertEquals(AP_NAME, tName);
    	assertEquals(type, RelationalConstants.TYPES.AP);
    }
    
	/**
     * Test adding columns
     */
    @Test
    public void testAddColumns() {
    	AccessPattern ap = RelationalObjectFactory.INSTANCE.createAccessPattern(AP_NAME);
    	
    	ap.addColumn(RelationalObjectFactory.INSTANCE.createColumn("column1")); //$NON-NLS-1$
    	
    	assertEquals(1, ap.getColumns().size());
    }

	/**
     * Test adding columns
     */
    @Test
    public void testAddRemoveColumns() {
    	AccessPattern ap = RelationalObjectFactory.INSTANCE.createAccessPattern(AP_NAME);

    	Column col1 = RelationalObjectFactory.INSTANCE.createColumn("col1"); //$NON-NLS-1$
    	
    	ap.addColumn(col1);
    	
    	ap.removeColumn(col1);
    	
    	assertEquals(0, ap.getColumns().size());
    }
    
	/**
     * Test validation.  expected result - OK
     */
    @Test
    public void testValidate1() {
    	AccessPattern ap = RelationalObjectFactory.INSTANCE.createAccessPattern(AP_NAME);
    	
    	IOutcome outcome = ap.validate();
    	
    	assertEquals(IOutcome.Level.OK, outcome.getLevel());
    }
        
}
