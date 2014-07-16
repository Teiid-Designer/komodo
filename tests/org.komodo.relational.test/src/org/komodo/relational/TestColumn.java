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

import java.util.Properties;

import org.junit.Test;
import org.komodo.relational.constants.RelationalConstants;
import org.komodo.relational.core.DataType;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.RelationalObjectFactory;
import org.komodo.spi.outcome.IOutcome;

/**
 * Test Class to test Column
 * 
 */
public class TestColumn {

	private static final String COLUMN_NAME = "myCol";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestColumn( ) {
		super();
	}
	
	/**
     * Test simple creation
     */
    @Test
    public void testCreate() {
    	Column col = RelationalObjectFactory.INSTANCE.createColumn(COLUMN_NAME);
    	String cName = col.getName();
    	int type = col.getType();
    	
    	assertEquals(COLUMN_NAME, cName);
    	assertEquals(type, RelationalConstants.TYPES.COLUMN);
    }
    
    /**
     * Test Default properties
     */
    @Test
    public void testDefaultProperties() {
    	Column column = RelationalObjectFactory.INSTANCE.createColumn(COLUMN_NAME);
    	
    	String name = column.getName();
    	assertEquals(COLUMN_NAME, name);
    	
    	int octetLength = column.getCharacterOctetLength();
    	String charSetName = column.getCharacterSetName();
    	String collationName = column.getCollationName();
    	String dataTypeName = column.getDatatypeName();
    	String defaultValue = column.getDefaultValue();
    	String description = column.getDescription();
    	String displayName = column.getDisplayName();
    	int distinctValueCount = column.getDistinctValueCount();
    	Properties extProps = column.getExtensionProperties();
    	String format = column.getFormat();
    	long length = column.getLength();
    	String maxValue = column.getMaximumValue();
    	String minValue = column.getMinimumValue();
    	String nativeType = column.getNativeType();
    	String nullable = column.getNullable();
    	int nullValueCount = column.getNullValueCount();
    	int precision = column.getPrecision();
    	int radix = column.getRadix();
    	int processType = column.getProcessType();
    	int scal = column.getScale();
    	String searchability = column.getSearchability();
    	boolean isAutoIncremented = column.isAutoIncremented();
    	boolean isCaseSensitive = column.isCaseSensitive();
    	boolean isCurrency = column.isCurrency();
    	boolean isLengthFixed = column.isLengthFixed();
    	boolean isSelectable = column.isSelectable();
    	boolean isSigned = column.isSigned();
    	boolean isUpdateable = column.isUpdateable();

    	assertEquals(null,charSetName);
    	assertEquals(null,collationName);
    	assertEquals(DataType.DEFAULT_NAME,dataTypeName);
    	assertEquals(null,defaultValue);
    	assertEquals(null,format);
    	assertEquals(null,maxValue);
    	assertEquals(null,minValue);
    	assertEquals(null,nativeType);
    	assertEquals(0,precision);
    	assertEquals(0,radix);
    	assertEquals(RelationalObject.CREATE_ANYWAY,processType);
    	assertEquals(0,scal);
    	assertEquals(false,isAutoIncremented);
    	assertEquals(false,isCurrency);
    	assertEquals(false,isLengthFixed);
    	
        assertEquals(-1,distinctValueCount);
        assertEquals(-1,nullValueCount);
        assertEquals(0,octetLength);
        assertEquals(0,length);
        
        assertEquals(RelationalConstants.NULLABLE.NULLABLE,nullable);
        assertEquals(RelationalConstants.SEARCHABILITY.SEARCHABLE,searchability);
    	assertEquals(true,isCaseSensitive);
    	assertEquals(true,isSigned);
    	assertEquals(true,isSelectable);
    	assertEquals(true,isUpdateable);
    	
    	assertEquals(0,extProps.size());
    	
    	assertEquals(null,description);
    	assertEquals("Column",displayName); //$NON-NLS-1$
    }
    
    /**
     * Test the set properties method
     */
    @Test
    public void testSetProperties() {
    	Column column = RelationalObjectFactory.INSTANCE.createColumn(COLUMN_NAME);

    	String testName = "TestName"; //$NON-NLS-1$
    	String testNIS = "TestNIS"; //$NON-NLS-1$
    	String testDesc = "My Description"; //$NON-NLS-1$
    	
    	Properties props = new Properties();
    	props.put(RelationalObject.KEY_NAME, testName);
    	props.put(RelationalObject.KEY_NAME_IN_SOURCE, testNIS);
    	props.put(RelationalObject.KEY_DESCRIPTION, testDesc);

    	props.put(Column.KEY_DISTINCT_VALUE_COUNT, "5"); //$NON-NLS-1$
    	props.put(Column.KEY_NULL_VALUE_COUNT, "6"); //$NON-NLS-1$
    	props.put(Column.KEY_DATATYPE, "string"); //$NON-NLS-1$
    	props.put(Column.KEY_NATIVE_TYPE, "numeric"); //$NON-NLS-1$
    	props.put(Column.KEY_NULLABLE, RelationalConstants.NULLABLE.NO_NULLS); 
    	props.put(Column.KEY_AUTO_INCREMENTED, "true"); //$NON-NLS-1$
    	props.put(Column.KEY_CASE_SENSITIVE, "false"); //$NON-NLS-1$
    	props.put(Column.KEY_CHARACTER_SET_NAME, "charSet"); //$NON-NLS-1$
    	props.put(Column.KEY_CHARACTER_OCTET_LENGTH, "10"); //$NON-NLS-1$
    	props.put(Column.KEY_COLLATION_NAME, "collation"); //$NON-NLS-1$
    	props.put(Column.KEY_CURRENCY, "true"); //$NON-NLS-1$
    	props.put(Column.KEY_DEFAULT_VALUE, "xxx"); //$NON-NLS-1$
    	props.put(Column.KEY_FORMAT, "format"); //$NON-NLS-1$
    	props.put(Column.KEY_LENGTH, "99"); //$NON-NLS-1$
    	props.put(Column.KEY_LENGTH_FIXED, "true"); //$NON-NLS-1$
    	props.put(Column.KEY_MAXIMUM_VALUE, "maxVal"); //$NON-NLS-1$
    	props.put(Column.KEY_MINIMUM_VALUE, "minVal"); //$NON-NLS-1$
    	props.put(Column.KEY_PRECISION, "3"); //$NON-NLS-1$
    	props.put(Column.KEY_SCALE, "5"); //$NON-NLS-1$
    	props.put(Column.KEY_RADIX, "4"); //$NON-NLS-1$
    	props.put(Column.KEY_SIGNED, "false"); //$NON-NLS-1$
    	props.put(Column.KEY_SEARCHABILITY, RelationalConstants.SEARCHABILITY.LIKE_ONLY);
    	props.put(Column.KEY_SELECTABLE, "false"); //$NON-NLS-1$
    	props.put(Column.KEY_UPDATEABLE, "false"); //$NON-NLS-1$
    	
    	column.setProperties(props);
    	
    	assertEquals(testName, column.getName());
    	assertEquals(testNIS, column.getNameInSource());
    	assertEquals(testDesc, column.getDescription());
    	assertEquals("Column", column.getDisplayName()); //$NON-NLS-1$

    	assertEquals(10,column.getCharacterOctetLength());
    	assertEquals("charSet", column.getCharacterSetName()); //$NON-NLS-1$
    	assertEquals("collation", column.getCollationName()); //$NON-NLS-1$
    	assertEquals("string", column.getDatatypeName()); //$NON-NLS-1$
    	assertEquals("xxx", column.getDefaultValue()); //$NON-NLS-1$
    	assertEquals(5, column.getDistinctValueCount());
    	assertEquals(0, column.getExtensionProperties().size());
    	assertEquals("format", column.getFormat()); //$NON-NLS-1$
    	assertEquals(99, column.getLength());
    	assertEquals("maxVal", column.getMaximumValue()); //$NON-NLS-1$
    	assertEquals("minVal", column.getMinimumValue()); //$NON-NLS-1$
    	assertEquals("numeric", column.getNativeType()); //$NON-NLS-1$
    	assertEquals(RelationalConstants.NULLABLE.NO_NULLS, column.getNullable());
    	assertEquals(6, column.getNullValueCount());
    	assertEquals(3, column.getPrecision());
    	assertEquals(4, column.getRadix());
    	assertEquals(0, column.getProcessType());
    	assertEquals(5, column.getScale());
    	assertEquals(RelationalConstants.SEARCHABILITY.LIKE_ONLY,column.getSearchability());
    	assertEquals(true, column.isAutoIncremented());
    	assertEquals(false, column.isCaseSensitive());
    	assertEquals(true, column.isCurrency());
    	assertEquals(true, column.isLengthFixed());
    	assertEquals(false, column.isSelectable());
    	assertEquals(false, column.isSigned());
    	assertEquals(false, column.isUpdateable());
    }
  
	/**
     * Test set datatype.  This also sets precision if type is numeric
     */
    @Test
    public void testDatatype() {
    	Column column = RelationalObjectFactory.INSTANCE.createColumn(COLUMN_NAME);
    	
    	column.setDatatypeName("STRING"); //$NON-NLS-1$
    	
    	assertEquals("STRING", column.getDatatypeName()); //$NON-NLS-1$
    	assertEquals(0, column.getPrecision());

    	column.setDatatypeName("INTEGER"); //$NON-NLS-1$
    	
    	assertEquals("INTEGER", column.getDatatypeName()); //$NON-NLS-1$
    	assertEquals(1, column.getPrecision());
    }

    /**
     * Test validation.  expected result - OK
     */
    @Test
    public void testValidate1() {
    	Column column = RelationalObjectFactory.INSTANCE.createColumn(COLUMN_NAME);
    	
    	IOutcome outcome = column.validate();
    	
    	assertEquals(IOutcome.Level.OK, outcome.getLevel());
    }
        
    /**
     * Test validation.  expected result - ERROR - no spaces in unquoted name
     */
    @Test
    public void testValidate2() {
    	Column column = RelationalObjectFactory.INSTANCE.createColumn("Crap ?"); //$NON-NLS-1$
    	
    	IOutcome outcome = column.validate();
    	
    	assertEquals(IOutcome.Level.ERROR, outcome.getLevel());
    	
    	if(!outcome.getMessage().startsWith("The name is invalid.")) { //$NON-NLS-1$
    		fail("unexpected message"); //$NON-NLS-1$
    	}
    }

    /**
     * Test validation.  expected result - OK - special char is allowed
     */
    @Test
    public void testValidate3() {
    	Column column = RelationalObjectFactory.INSTANCE.createColumn("Crap?"); //$NON-NLS-1$
    	
    	IOutcome outcome = column.validate();
    	
    	assertEquals(IOutcome.Level.OK, outcome.getLevel());
    }
    
    /**
     * Test validation.  expected result - ERROR - invalid name, first char must be alpha
     */
    @Test
    public void testValidate4() {
    	Column column = RelationalObjectFactory.INSTANCE.createColumn("?Crap"); //$NON-NLS-1$
    	
    	IOutcome outcome = column.validate();
    	
    	assertEquals(IOutcome.Level.ERROR, outcome.getLevel());
    	
    	if(!outcome.getMessage().startsWith("The first character of the name")) { //$NON-NLS-1$
    		fail("unexpected message"); //$NON-NLS-1$
    	}
    }
    
}
