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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Properties;

import org.junit.Test;
import org.komodo.relational.constants.RelationalConstants;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.RelationalObjectFactory;
import org.komodo.relational.model.Table;
import org.komodo.spi.outcome.IOutcome;

/**
 * Test Class to test Table
 * 
 */
public class TestTable {
	
	private static final String TABLE_NAME = "myTable";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestTable( ) {
		super();
	}
	
	/**
     * Test simple creation
     */
    @Test
    public void testCreate() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);
    	String tName = table.getName();
    	int type = table.getType();
    	
    	assertEquals(TABLE_NAME, tName);
    	assertEquals(type, RelationalConstants.TYPES.TABLE);
    }
    
	/**
     * Test adding columns
     */
    @Test
    public void testAddColumns() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);

    	table.addColumn(RelationalObjectFactory.INSTANCE.createColumn("column1")); //$NON-NLS-1$
    	
    	assertEquals(1, table.getColumns().size());
    }

	/**
     * Test adding columns
     */
    @Test
    public void testAddRemoveColumns() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);

    	Column col1 = RelationalObjectFactory.INSTANCE.createColumn("col1"); //$NON-NLS-1$
    	
    	table.addColumn(col1);
    	
    	table.removeColumn(col1);
    	
    	assertEquals(0, table.getColumns().size());
    }
    
	/**
     * Test Set and Get PK
     */
    @Test
    public void testSetGetPK() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);

    	PrimaryKey pk = RelationalObjectFactory.INSTANCE.createPrimaryKey("pk1"); //$NON-NLS-1$
    	table.setPrimaryKey(pk);
    	
    	assertEquals(pk, table.getPrimaryKey());
    }

    /**
     * Test Add FK
     */
    @Test
    public void testAddFK() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);

    	ForeignKey fk = RelationalObjectFactory.INSTANCE.createForeignKey("fk1"); //$NON-NLS-1$
    	table.addForeignKey(fk);
    	
    	assertEquals(1, table.getForeignKeys().size());
    }

    /**
     * Test Add Remove FK
     */
    @Test
    public void testAddRemoveFK() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);

    	ForeignKey fk = RelationalObjectFactory.INSTANCE.createForeignKey("fk1"); //$NON-NLS-1$
    	table.addForeignKey(fk);
    	
    	table.removeForeignKey(fk);
    	
    	assertEquals(0, table.getForeignKeys().size());
    }

    /**
     * Test Default properties
     */
    @Test
    public void testDefaultProperties() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);
    	
    	String name = table.getName();
    	assertEquals(TABLE_NAME, name);
    	
    	int cardinality = table.getCardinality();
    	boolean isMaterialized = table.isMaterialized();
    	boolean isSystem = table.isSystem();
    	boolean isChecked = table.isChecked();
    	boolean supportsUpdate = table.getSupportsUpdate();
    	String description = table.getDescription();
    	String displayName = table.getDisplayName();
    	String nis = table.getNameInSource();
    	String query = table.getNativeQuery();
    	
    	assertEquals(-1,cardinality);
    	assertEquals(false,isMaterialized);
    	assertEquals(false,isSystem);
    	assertEquals(true,isChecked);
    	assertEquals(true,supportsUpdate);
    	assertEquals(null,description);
    	assertEquals("Table",displayName); //$NON-NLS-1$
    	assertEquals(null,nis);
    	assertEquals(null,query);
    	
    	RelationalObject matTable = table.getMaterializedTable();
    	assertEquals(null,matTable);
    	
    	RelationalObject parent = table.getParent();
    	assertEquals(null,parent);

    	assertEquals(0, table.getExtensionProperties().size());
    	assertEquals(0,table.getAccessPatterns().size());
    	assertEquals(0,table.getColumns().size());
    	assertEquals(0,table.getForeignKeys().size());
    	assertEquals(0,table.getIndexes().size());
    }
    
	/**
     * Test create a single column via createColumn method
     */
    @Test
    public void testCreateSingleColumn() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);
    	
    	table.createColumn();
    	
    	// Expect one column
    	assertEquals(1, table.getColumns().size());
    	
    	Column col = table.getColumns().get(0);
    	String colName = col.getName();
    	String dType = col.getDatatypeName();
    	
    	// Default name and type
    	assertEquals("newColumn_1",colName); //$NON-NLS-1$
    	assertEquals("string",dType); //$NON-NLS-1$
    	
    	boolean canMoveUp = table.canMoveColumnUp(col);
    	boolean canMoveDown = table.canMoveColumnDown(col);
    	
    	assertFalse(canMoveUp);
    	assertFalse(canMoveDown);
    }

    /**
     * Test create multiple columns via createColumn method
     */
    @Test
    public void testCreateMultiColumns() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);
    	
    	table.createColumn();
    	table.createColumn();
    	table.createColumn();
    	
    	// Expect three column
    	assertEquals(3, table.getColumns().size());
    	
    	Column col1 = table.getColumns().get(0);
    	Column col2 = table.getColumns().get(1);
    	Column col3 = table.getColumns().get(2);

    	// Check expected names
    	String col1Name = col1.getName();
    	String col2Name = col2.getName();
    	String col3Name = col3.getName();
    	assertEquals("newColumn_1",col1Name); //$NON-NLS-1$
    	assertEquals("newColumn_2",col2Name); //$NON-NLS-1$
    	assertEquals("newColumn_3",col3Name); //$NON-NLS-1$
    	
    	// Check expected types
    	String dType1 = col1.getDatatypeName();
    	String dType2 = col2.getDatatypeName();
    	String dType3 = col3.getDatatypeName();
    	assertEquals("string",dType1); //$NON-NLS-1$
    	assertEquals("string",dType2); //$NON-NLS-1$
    	assertEquals("string",dType3); //$NON-NLS-1$
    	
    	// Check can move up
    	boolean canMoveUp1 = table.canMoveColumnUp(col1);
    	boolean canMoveUp2 = table.canMoveColumnUp(col2);
    	boolean canMoveUp3 = table.canMoveColumnUp(col3);
    	assertFalse(canMoveUp1);
    	assertTrue(canMoveUp2);
    	assertTrue(canMoveUp3);

    	// Check can move down
    	boolean canMoveDown1 = table.canMoveColumnDown(col1);
    	boolean canMoveDown2 = table.canMoveColumnDown(col2);
    	boolean canMoveDown3 = table.canMoveColumnDown(col3);
    	assertTrue(canMoveDown1);
    	assertTrue(canMoveDown2);
    	assertFalse(canMoveDown3);
    }
    
    /**
     * Test moving a column up and down
     */
    @Test
    public void testMoveColumn() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);
    	
    	table.createColumn();
    	table.createColumn();
    	table.createColumn();
    	
    	// Expect three column
    	assertEquals(3, table.getColumns().size());
    	
    	Column col1 = table.getColumns().get(0);
    	Column col2 = table.getColumns().get(1);
    	Column col3 = table.getColumns().get(2);

    	// Check expected names
    	String col1Name = col1.getName();
    	String col2Name = col2.getName();
    	String col3Name = col3.getName();
    	assertEquals("newColumn_1",col1Name); //$NON-NLS-1$
    	assertEquals("newColumn_2",col2Name); //$NON-NLS-1$
    	assertEquals("newColumn_3",col3Name); //$NON-NLS-1$
    	
    	// Move bottom column to top
    	table.moveColumnUp(col3);
    	table.moveColumnUp(col3);
    	
    	// Check re-ordered columns
    	col1 = table.getColumns().get(0);
    	col2 = table.getColumns().get(1);
    	col3 = table.getColumns().get(2);
    	col1Name = col1.getName();
    	col2Name = col2.getName();
    	col3Name = col3.getName();
    	assertEquals("newColumn_3",col1Name); //$NON-NLS-1$
    	assertEquals("newColumn_1",col2Name); //$NON-NLS-1$
    	assertEquals("newColumn_2",col3Name); //$NON-NLS-1$
    	
    	// Move top column down one position
    	table.moveColumnDown(col1);
    	
    	// Check re-ordered columns
    	col1 = table.getColumns().get(0);
    	col2 = table.getColumns().get(1);
    	col3 = table.getColumns().get(2);
    	col1Name = col1.getName();
    	col2Name = col2.getName();
    	col3Name = col3.getName();
    	assertEquals("newColumn_1",col1Name); //$NON-NLS-1$
    	assertEquals("newColumn_3",col2Name); //$NON-NLS-1$
    	assertEquals("newColumn_2",col3Name); //$NON-NLS-1$
    }
    
    /**
     * Test the set properties method
     */
    @Test
    public void testSetProperties() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);

    	String testName = "TestName"; //$NON-NLS-1$
    	String testNIS = "TestNIS"; //$NON-NLS-1$
    	String testDesc = "My Description"; //$NON-NLS-1$
    	
    	Properties props = new Properties();
    	props.put(RelationalObject.KEY_NAME, testName);
    	props.put(RelationalObject.KEY_NAME_IN_SOURCE, testNIS);
    	props.put(RelationalObject.KEY_DESCRIPTION, testDesc);
    	
    	props.put(Table.KEY_CARDINALITY, "100"); //$NON-NLS-1$
    	props.put(Table.KEY_MATERIALIZED, "true"); //$NON-NLS-1$
    	props.put(Table.KEY_SUPPORTS_UPDATE, "false"); //$NON-NLS-1$
    	props.put(Table.KEY_SYSTEM, "true"); //$NON-NLS-1$

    	table.setProperties(props);
    	
    	assertEquals(testName, table.getName());
    	assertEquals(testNIS, table.getNameInSource());
    	assertEquals(testDesc, table.getDescription());
    	
    	assertEquals(100,table.getCardinality());
    	assertEquals(true,table.isMaterialized());
    	assertEquals(true,table.isSystem());
    	assertEquals(false,table.getSupportsUpdate());
    }
    
	/**
     * Test validation.  expected result - warning : no columns defined
     */
    @Test
    public void testValidateDefaultCreate() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);
    	
    	IOutcome outcome = table.validate();
    	
    	assertEquals(IOutcome.Level.WARNING, outcome.getLevel());
    	assertEquals("No columns defined for table", outcome.getMessage()); //$NON-NLS-1$
    }

    /**
     * Test validation.  expected result - ok
     */
    @Test
    public void testValidateTableWithOneColumn() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable(TABLE_NAME);
    	table.createColumn();
    	
    	IOutcome outcome = table.validate();
    	
    	assertEquals(IOutcome.Level.OK, outcome.getLevel());
    }

    /**
     * Test validation.  expected result - error : invalid name
     */
    @Test
    public void testValidateTableWithBadName() {
    	Table table = RelationalObjectFactory.INSTANCE.createTable("Crap ?"); //$NON-NLS-1$
    	table.createColumn();
    	
    	IOutcome outcome = table.validate();
    	
    	assertEquals(IOutcome.Level.ERROR, outcome.getLevel());
    	
    	if(!outcome.getMessage().startsWith("The name is invalid.")) { //$NON-NLS-1$
    		fail("unexpected message"); //$NON-NLS-1$
    	}
    }
        
}
