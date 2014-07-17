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
import org.komodo.relational.extension.RelationalModelExtensionConstants.PropertyKeysNoPrefix;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.RelationalObject;
import org.komodo.relational.model.RelationalObjectFactory;
import org.komodo.spi.outcome.IOutcome;

/**
 * Test Class to test Procedure
 * 
 */
public class TestProcedure {
	
	private static final String PROC_NAME = "myProc";  //$NON-NLS-1$
	
	/**
	 * Constructor
	 */
	public TestProcedure( ) {
		super();
	}
	
	/**
     * Test simple creation
     */
    @Test
    public void testCreate() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);
    	String pName = proc.getName();
    	int type = proc.getType();
    	
    	assertEquals(PROC_NAME, pName);
    	assertEquals(type, RelationalConstants.TYPES.PROCEDURE);
    }
    
	/**
     * Test adding columns
     */
    @Test
    public void testAddParameters() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);

    	Parameter param1 = RelationalObjectFactory.INSTANCE.createParameter("param1"); //$NON-NLS-1$
    	proc.addParameter(param1); 
    	
    	assertEquals(1, proc.getParameters().size());
    }

	/**
     * Test adding columns
     */
    @Test
    public void testAddRemoveParameters() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);

    	Parameter param1 = RelationalObjectFactory.INSTANCE.createParameter("param1"); //$NON-NLS-1$
    	
    	proc.addParameter(param1);
    	
    	proc.removeParameter(param1);
    	
    	assertEquals(0, proc.getParameters().size());
    }
    
    /**
     * Test Default properties
     */
    @Test
    public void testDefaultProperties() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);
    	
    	String name = proc.getName();
    	assertEquals(PROC_NAME, name);
    	
    	String description = proc.getDescription();
    	String displayName = proc.getDisplayName();
    	String nis = proc.getNameInSource();
    	String query = proc.getNativeQuery();
    	assertEquals(null,description);
    	assertEquals("Procedure",displayName); //$NON-NLS-1$
    	assertEquals(null,nis);
    	assertEquals(null,query);

    	RelationalObject parent = proc.getParent();
    	assertEquals(null,parent);

    	ProcedureResultSet resultSet = proc.getResultSet();
    	assertEquals(null,resultSet);

    	String functionCategory = proc.getFunctionCategory();
    	String javaClassName = proc.getJavaClassName();
    	String javaMethodName = proc.getJavaMethodName();
    	String udfJarPath = proc.getUdfJarPath();
    	String updateCount = proc.getUpdateCount();
    	assertEquals(null,functionCategory);
    	assertEquals(null,javaClassName);
    	assertEquals(null,javaMethodName);
    	assertEquals(null,udfJarPath);
    	assertEquals(null,updateCount);
    	
    	assertEquals(0, proc.getExtensionProperties().size());
    }
    
	/**
     * Test create a single column via createColumn method
     */
    @Test
    public void testCreateSingleParameter() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);
    	
    	proc.createParameter();
    	
    	// Expect one column
    	assertEquals(1, proc.getParameters().size());
    	
    	Parameter param = proc.getParameters().get(0);
    	String name = param.getName();
    	String dType = param.getDatatypeName();
    	
    	// Default name and type
    	assertEquals("newParameter_1",name); //$NON-NLS-1$
    	assertEquals("string",dType); //$NON-NLS-1$
    	
    	boolean canMoveUp = proc.canMoveParameterUp(param);
    	boolean canMoveDown = proc.canMoveParameterUp(param);
    	
    	assertFalse(canMoveUp);
    	assertFalse(canMoveDown);
    }

    /**
     * Test create multiple columns via createColumn method
     */
    @Test
    public void testCreateMultiParameters() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);
    	
    	proc.createParameter();
    	proc.createParameter();
    	proc.createParameter();
    	
    	// Expect three params
    	assertEquals(3, proc.getParameters().size());
    	
    	Parameter param1 = proc.getParameters().get(0);
    	Parameter param2 = proc.getParameters().get(1);
    	Parameter param3 = proc.getParameters().get(2);

    	// Check expected names
    	String p1Name = param1.getName();
    	String p2Name = param2.getName();
    	String p3Name = param3.getName();
    	assertEquals("newParameter_1",p1Name); //$NON-NLS-1$
    	assertEquals("newParameter_2",p2Name); //$NON-NLS-1$
    	assertEquals("newParameter_3",p3Name); //$NON-NLS-1$
    	
    	// Check expected types
    	String dType1 = param1.getDatatypeName();
    	String dType2 = param2.getDatatypeName();
    	String dType3 = param3.getDatatypeName();
    	assertEquals("string",dType1); //$NON-NLS-1$
    	assertEquals("string",dType2); //$NON-NLS-1$
    	assertEquals("string",dType3); //$NON-NLS-1$
    	
    	// Check can move up
    	boolean canMoveUp1 = proc.canMoveParameterUp(param1);
    	boolean canMoveUp2 = proc.canMoveParameterUp(param2);
    	boolean canMoveUp3 = proc.canMoveParameterUp(param3);
    	assertFalse(canMoveUp1);
    	assertTrue(canMoveUp2);
    	assertTrue(canMoveUp3);

    	// Check can move down
    	boolean canMoveDown1 = proc.canMoveParameterDown(param1);
    	boolean canMoveDown2 = proc.canMoveParameterDown(param2);
    	boolean canMoveDown3 = proc.canMoveParameterDown(param3);
    	assertTrue(canMoveDown1);
    	assertTrue(canMoveDown2);
    	assertFalse(canMoveDown3);
    }
    
    /**
     * Test moving a column up and down
     */
    @Test
    public void testMoveParameter() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);
    	
    	proc.createParameter();
    	proc.createParameter();
    	proc.createParameter();
    	
    	// Expect three column
    	assertEquals(3, proc.getParameters().size());
    	
    	Parameter param1 = proc.getParameters().get(0);
    	Parameter param2 = proc.getParameters().get(1);
    	Parameter param3 = proc.getParameters().get(2);

    	// Check expected names
    	String param1Name = param1.getName();
    	String param2Name = param2.getName();
    	String param3Name = param3.getName();
    	assertEquals("newParameter_1",param1Name); //$NON-NLS-1$
    	assertEquals("newParameter_2",param2Name); //$NON-NLS-1$
    	assertEquals("newParameter_3",param3Name); //$NON-NLS-1$
    	
    	// Move bottom param to top
    	proc.moveParameterUp(param3);
    	proc.moveParameterUp(param3);
    	
    	// Check re-ordered params
    	param1 = proc.getParameters().get(0);
    	param2 = proc.getParameters().get(1);
    	param3 = proc.getParameters().get(2);
    	param1Name = param1.getName();
    	param2Name = param2.getName();
    	param3Name = param3.getName();
    	assertEquals("newParameter_3",param1Name); //$NON-NLS-1$
    	assertEquals("newParameter_1",param2Name); //$NON-NLS-1$
    	assertEquals("newParameter_2",param3Name); //$NON-NLS-1$
    	
    	// Move top param down one position
    	proc.moveParameterDown(param1);
    	
    	// Check re-ordered params
    	param1 = proc.getParameters().get(0);
    	param2 = proc.getParameters().get(1);
    	param3 = proc.getParameters().get(2);
    	param1Name = param1.getName();
    	param2Name = param2.getName();
    	param3Name = param3.getName();
    	assertEquals("newParameter_1",param1Name); //$NON-NLS-1$
    	assertEquals("newParameter_3",param2Name); //$NON-NLS-1$
    	assertEquals("newParameter_2",param3Name); //$NON-NLS-1$
    }
    
    /**
     * Test the set properties method
     */
    @Test
    public void testSetProperties() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);

    	String testName = "TestName"; //$NON-NLS-1$
    	String testNIS = "TestNIS"; //$NON-NLS-1$
    	String testDesc = "My Description"; //$NON-NLS-1$
    	
    	Properties props = new Properties();
    	props.put(RelationalObject.KEY_NAME, testName);
    	props.put(RelationalObject.KEY_NAME_IN_SOURCE, testNIS);
    	props.put(RelationalObject.KEY_DESCRIPTION, testDesc);
    	
    	props.put(Procedure.KEY_FUNCTION, "false"); //$NON-NLS-1$
    	props.put(Procedure.KEY_UPDATE_COUNT, "updCount"); //$NON-NLS-1$

    	proc.setProperties(props);
    	
    	assertEquals(testName, proc.getName());
    	assertEquals(testNIS, proc.getNameInSource());
    	assertEquals(testDesc, proc.getDescription());
    	
    	assertEquals(false,proc.isFunction());
    	assertEquals("updCount",proc.getUpdateCount()); //$NON-NLS-1$
    	
    	// Expected extension properties
    	Properties expectedProps = new Properties();
    	expectedProps.put(PropertyKeysNoPrefix.NON_PREPARED, false);
    	
    	// Compare expected to actual extension properties
    	Properties actualProps = proc.getExtensionProperties();
    	String result = RelationalUtil.compareExtensionProperties(expectedProps,actualProps);
    	if(result.trim().length()!=0) {
    		fail(result);
    	}
    }
    
    /**
     * Test the set properties method
     */
    @Test
    public void testCreateFunction() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);

    	String testName = "Function"; //$NON-NLS-1$
    	String testNIS = "FunctionNIS"; //$NON-NLS-1$
    	String testDesc = "Function Description"; //$NON-NLS-1$
    	String javaClassName = "java.class.name"; //$NON-NLS-1$
    	String javaMethodName = "method"; //$NON-NLS-1$
    	String category = "unknown"; //$NON-NLS-1$
    	String udfJarPath = "the/jar/path"; //$NON-NLS-1$
    	
    	proc.setName(testName);
    	proc.setNameInSource(testNIS);
    	proc.setDescription(testDesc);
    	
    	proc.setFunction(true);
    	proc.setJavaClassName(javaClassName);
    	proc.setJavaMethodName(javaMethodName);
    	proc.setFunctionCategory(category);
    	proc.setUdfJarPath(udfJarPath);
    	
    	assertEquals(testName, proc.getName());
    	assertEquals(testNIS, proc.getNameInSource());
    	assertEquals(testDesc, proc.getDescription());
    	
    	assertEquals(true,proc.isFunction());
    	assertEquals(javaClassName,proc.getJavaClassName());
    	assertEquals(javaMethodName,proc.getJavaMethodName());
    	assertEquals(category,proc.getFunctionCategory());
    	assertEquals(udfJarPath,proc.getUdfJarPath());
    	
    	// Expected extension properties
    	Properties expectedProps = new Properties();
    	expectedProps.put(PropertyKeysNoPrefix.NON_PREPARED, false);
    	expectedProps.put(PropertyKeysNoPrefix.FUNCTION_CATEGORY, "unknown"); //$NON-NLS-1$
    	expectedProps.put(PropertyKeysNoPrefix.JAVA_METHOD, "method"); //$NON-NLS-1$
    	expectedProps.put(PropertyKeysNoPrefix.JAVA_CLASS, "java.class.name"); //$NON-NLS-1$
    	expectedProps.put(PropertyKeysNoPrefix.DECOMPOSABLE, false);
    	expectedProps.put(PropertyKeysNoPrefix.AGGREGATE, false);
    	expectedProps.put(PropertyKeysNoPrefix.NULL_ON_NULL, false);
    	expectedProps.put(PropertyKeysNoPrefix.UDF_JAR_PATH, "the/jar/path"); //$NON-NLS-1$
    	expectedProps.put(PropertyKeysNoPrefix.ALLOWS_DISTINCT, false);
    	expectedProps.put(PropertyKeysNoPrefix.ANALYTIC, false);
    	expectedProps.put(PropertyKeysNoPrefix.ALLOWS_ORDER_BY, false);
    	expectedProps.put(PropertyKeysNoPrefix.DETERMINISTIC, false);
    	expectedProps.put(PropertyKeysNoPrefix.VARARGS, false);
    	expectedProps.put(PropertyKeysNoPrefix.USES_DISTINCT_ROWS, false);
    	
    	// Compare expected to actual extension properties
    	Properties actualProps = proc.getExtensionProperties();
    	String result = RelationalUtil.compareExtensionProperties(expectedProps,actualProps);
    	if(result.trim().length()!=0) {
    		fail(result);
    	}
    }

    /**
     * Test the set properties method
     */
    @Test
    public void testCreateAggregateFunction() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);

    	String testName = "Function"; //$NON-NLS-1$
    	String testNIS = "FunctionNIS"; //$NON-NLS-1$
    	String testDesc = "Function Description"; //$NON-NLS-1$
    	String javaClassName = "java.class.name"; //$NON-NLS-1$
    	String javaMethodName = "method"; //$NON-NLS-1$
    	String category = "unknown"; //$NON-NLS-1$
    	String udfJarPath = "the/jar/path"; //$NON-NLS-1$
    	
    	proc.setName(testName);
    	proc.setNameInSource(testNIS);
    	proc.setDescription(testDesc);
    	
    	proc.setFunction(true);
    	proc.setJavaClassName(javaClassName);
    	proc.setJavaMethodName(javaMethodName);
    	proc.setFunctionCategory(category);
    	proc.setUdfJarPath(udfJarPath);
    	proc.setAggregate(true);
    	
    	assertEquals(testName, proc.getName());
    	assertEquals(testNIS, proc.getNameInSource());
    	assertEquals(testDesc, proc.getDescription());
    	
    	assertEquals(true,proc.isFunction());
    	assertEquals(true,proc.isAggregate());
    	assertEquals(javaClassName,proc.getJavaClassName());
    	assertEquals(javaMethodName,proc.getJavaMethodName());
    	assertEquals(category,proc.getFunctionCategory());
    	assertEquals(udfJarPath,proc.getUdfJarPath());
    	
    	// Expected extension properties
    	Properties expectedProps = new Properties();
    	expectedProps.put(PropertyKeysNoPrefix.NON_PREPARED, false);
    	expectedProps.put(PropertyKeysNoPrefix.FUNCTION_CATEGORY, "unknown"); //$NON-NLS-1$
    	expectedProps.put(PropertyKeysNoPrefix.JAVA_METHOD, "method"); //$NON-NLS-1$
    	expectedProps.put(PropertyKeysNoPrefix.JAVA_CLASS, "java.class.name"); //$NON-NLS-1$
    	expectedProps.put(PropertyKeysNoPrefix.AGGREGATE, true);
    	expectedProps.put(PropertyKeysNoPrefix.NULL_ON_NULL, false);
    	expectedProps.put(PropertyKeysNoPrefix.UDF_JAR_PATH, "the/jar/path"); //$NON-NLS-1$
    	expectedProps.put(PropertyKeysNoPrefix.DETERMINISTIC, false);
    	expectedProps.put(PropertyKeysNoPrefix.VARARGS, false);
    	
    	// Compare expected to actual extension properties
    	Properties actualProps = proc.getExtensionProperties();
    	String result = RelationalUtil.compareExtensionProperties(expectedProps,actualProps);
    	if(result.trim().length()!=0) {
    		fail(result);
    	}
    }
    
	/**
     * Test validation.  expected result - warning : no columns defined
     */
    @Test
    public void testValidateDefaultProc() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);
    	
    	IOutcome outcome = proc.validate();
    	
    	assertEquals(IOutcome.Level.WARNING, outcome.getLevel());
    	assertEquals("No parameters defined for procedure", outcome.getMessage()); //$NON-NLS-1$
    }

    /**
     * Test validation.  expected result - error : invalid name
     */
    @Test
    public void testValidateProcWithBadName() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure("Crap ?"); //$NON-NLS-1$
    	proc.createParameter();
    	
    	IOutcome outcome = proc.validate();
    	
    	assertEquals(IOutcome.Level.ERROR, outcome.getLevel());
    	
    	if(!outcome.getMessage().startsWith("The character ' ' (at position 5) is not allowed")) { //$NON-NLS-1$
    		fail("unexpected message"); //$NON-NLS-1$
    	}
    }
        
    /**
     * Test validation.  expected result - ok
     */
    @Test
    public void testValidateProcWithOneParam() {
    	Procedure proc = RelationalObjectFactory.INSTANCE.createProcedure(PROC_NAME);
    	proc.createParameter();
    	
    	IOutcome outcome = proc.validate();
    	
    	assertEquals(IOutcome.Level.OK, outcome.getLevel());
    }

}
