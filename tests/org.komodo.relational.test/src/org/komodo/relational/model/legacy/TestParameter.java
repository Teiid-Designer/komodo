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
package org.komodo.relational.model.legacy;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;
import java.util.Properties;
import org.junit.Test;
import org.komodo.relational.model.legacy.Parameter;
import org.komodo.relational.model.legacy.RelationalConstants;
import org.komodo.relational.model.legacy.RelationalObject;
import org.komodo.relational.model.legacy.RelationalObjectFactory;
import org.komodo.spi.outcome.Outcome;

/**
 * Test Class to test Parameter
 * 
 */
public class TestParameter {

	private static final String PARAM_NAME = "myParam";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestParameter( ) {
		super();
	}
	
	/**
     * Test simple creation
     */
    @Test
    public void testCreate() {
    	Parameter param = RelationalObjectFactory.INSTANCE.createParameter(PARAM_NAME);
    	String cName = param.getName();
    	int type = param.getType();
    	
    	assertEquals(PARAM_NAME, cName);
    	assertEquals(type, RelationalConstants.TYPES.PARAMETER);
    }
    
    /**
     * Test Default properties
     */
    @Test
    public void testDefaultProperties() {
    	Parameter param = RelationalObjectFactory.INSTANCE.createParameter(PARAM_NAME);
    	
    	String name = param.getName();
    	String description = param.getDescription();
    	String displayName = param.getDisplayName();
    	String nis = param.getNameInSource();
    	assertEquals(PARAM_NAME, name);
    	assertEquals(null,description);
    	assertEquals("Parameter",displayName); //$NON-NLS-1$
    	assertEquals(null,nis);
    	
    	
    	String dataType = param.getDatatypeName();
    	String defaultValue = param.getDefaultValue();
    	long length = param.getLength();
    	String nativeType = param.getNativeType();
    	String nullable = param.getNullable();
    	int precision = param.getPrecision();
    	int radix = param.getRadix();
    	int processType = param.getProcessType();
    	int scale = param.getScale();
    	String direction = param.getDirection();
    	Properties extProps = param.getExtensionProperties();
    	
    	assertEquals(DataType.DEFAULT_NAME,dataType);
    	assertEquals(null,defaultValue);
    	assertEquals(null,nativeType);
        assertEquals(null,nullable);
    	assertEquals(RelationalConstants.DIRECTION.IN,direction);
        assertEquals(0,length);
    	assertEquals(0,precision);
    	assertEquals(0,radix);
    	assertEquals(RelationalObject.CREATE_ANYWAY,processType);
    	assertEquals(0,scale);
     	
    	assertEquals(0,extProps.size());
    	
    }
    
    /**
     * Test the set properties method
     */
    @Test
    public void testSetProperties() {
    	Parameter param = RelationalObjectFactory.INSTANCE.createParameter(PARAM_NAME);

    	String testName = "TestName"; //$NON-NLS-1$
    	String testNIS = "TestNIS"; //$NON-NLS-1$
    	String testDesc = "My Description"; //$NON-NLS-1$
    	
    	Properties props = new Properties();
    	props.put(RelationalObject.KEY_NAME, testName);
    	props.put(RelationalObject.KEY_NAME_IN_SOURCE, testNIS);
    	props.put(RelationalObject.KEY_DESCRIPTION, testDesc);

    	props.put(Parameter.KEY_DATATYPE, "string"); //$NON-NLS-1$
    	props.put(Parameter.KEY_NATIVE_TYPE, "numeric"); //$NON-NLS-1$
    	props.put(Parameter.KEY_NULLABLE, RelationalConstants.NULLABLE.NO_NULLS); 
    	props.put(Parameter.KEY_DIRECTION, RelationalConstants.DIRECTION.IN_OUT);
    	props.put(Parameter.KEY_DEFAULT_VALUE, "xxx"); //$NON-NLS-1$
    	props.put(Parameter.KEY_LENGTH, "99"); //$NON-NLS-1$
    	props.put(Parameter.KEY_PRECISION, "3"); //$NON-NLS-1$
    	props.put(Parameter.KEY_SCALE, "5"); //$NON-NLS-1$
    	props.put(Parameter.KEY_RADIX, "4"); //$NON-NLS-1$
    	        
    	param.setProperties(props);
    	
    	assertEquals(testName, param.getName());
    	assertEquals(testNIS, param.getNameInSource());
    	assertEquals(testDesc, param.getDescription());
    	assertEquals("Parameter", param.getDisplayName()); //$NON-NLS-1$

    	assertEquals("string", param.getDatatypeName()); //$NON-NLS-1$
    	assertEquals("xxx", param.getDefaultValue()); //$NON-NLS-1$
    	assertEquals(0, param.getExtensionProperties().size());
    	assertEquals(99, param.getLength());
    	assertEquals("numeric", param.getNativeType()); //$NON-NLS-1$
    	assertEquals(RelationalConstants.DIRECTION.IN_OUT, param.getDirection());
    	assertEquals(RelationalConstants.NULLABLE.NO_NULLS, param.getNullable());
    	assertEquals(3, param.getPrecision());
    	assertEquals(4, param.getRadix());
    	assertEquals(0, param.getProcessType());
    	assertEquals(5, param.getScale());
    }
  
	/**
     * Test set datatype.  This also sets precision if type is numeric
     */
    @Test
    public void testDatatype() {
    	Parameter param = RelationalObjectFactory.INSTANCE.createParameter(PARAM_NAME);
    	
    	param.setDatatypeName("STRING"); //$NON-NLS-1$
    	
    	assertEquals("STRING", param.getDatatypeName()); //$NON-NLS-1$
    	assertEquals(0, param.getPrecision());

    	param.setDatatypeName("INTEGER"); //$NON-NLS-1$
    	
    	assertEquals("INTEGER", param.getDatatypeName()); //$NON-NLS-1$
    }

    /**
     * Test validation.  expected result - OK
     */
    @Test
    public void testValidateDefaultParam() {
    	Parameter param = RelationalObjectFactory.INSTANCE.createParameter(PARAM_NAME);
    	
    	Outcome outcome = param.validate();
    	
    	assertEquals(Outcome.Level.OK, outcome.getLevel());
    }
        
    /**
     * Test validation.  expected result - OK - special char is allowed
     */
    @Test
    public void testValidateParamWithGoodName() {
    	Parameter param = RelationalObjectFactory.INSTANCE.createParameter("Crap?"); //$NON-NLS-1$
    	
    	Outcome outcome = param.validate();
    	
    	assertEquals(Outcome.Level.OK, outcome.getLevel());
    }
    
    /**
     * Test validation.  expected result - ERROR - no spaces in unquoted name
     */
    @Test
    public void testValidateParamWithBadName1() {
    	Parameter param = RelationalObjectFactory.INSTANCE.createParameter("Crap ?"); //$NON-NLS-1$
    	
    	Outcome outcome = param.validate();
    	
    	assertEquals(Outcome.Level.ERROR, outcome.getLevel());
    	
    	if(!outcome.getMessage().startsWith("The name is invalid.")) { //$NON-NLS-1$
    		fail("unexpected message"); //$NON-NLS-1$
    	}
    }

    /**
     * Test validation.  expected result - ERROR - invalid name, first char must be alpha
     */
    @Test
    public void testParamWithBadName2() {
    	Parameter param = RelationalObjectFactory.INSTANCE.createParameter("?Crap"); //$NON-NLS-1$
    	
    	Outcome outcome = param.validate();
    	
    	assertEquals(Outcome.Level.ERROR, outcome.getLevel());
    	
    	if(!outcome.getMessage().startsWith("The first character of the name")) { //$NON-NLS-1$
    		fail("unexpected message"); //$NON-NLS-1$
    	}
    }
    
}
