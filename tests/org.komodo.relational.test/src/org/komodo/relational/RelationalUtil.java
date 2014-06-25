/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.relational;

import java.util.Properties;

import org.komodo.relational.model.AccessPattern;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.ForeignKey;
import org.komodo.relational.model.Index;
import org.komodo.relational.model.Model;
import org.komodo.relational.model.Parameter;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Procedure;
import org.komodo.relational.model.ProcedureResultSet;
import org.komodo.relational.model.Schema;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;
import org.komodo.relational.model.View;

/**
 *
 */
public class RelationalUtil {
	
    /**
     * Create a Table
     * @param name the name
     * @return the created object
     */
    public static Table createTable(String name) {
    	Table table = new Table(name);
    	return table;
    }

    /**
     * Create a View
     * @param name the name
     * @return the created object
     */
    public static View createView(String name) {
    	View view = new View(name);
    	return view;
    }
    
    /**
     * Create a Column
     * @param name the name
     * @return the created object
     */
    public static Column createColumn(String name) {
    	Column column = new Column(name);
    	return column;
    }

    /**
     * Create a Model
     * @param name the name
     * @return the created object
     */
    public static Model createModel(String name) {
    	Model model = new Model(name);
    	return model;
    }
    
    /**
     * Create a Procedure
     * @param name the name
     * @return the created object
     */
    public static Procedure createProcedure(String name) {
    	Procedure proc = new Procedure(name);
    	return proc;
    }
    
    /**
     * Create a AccessPattern
     * @param name the name
     * @return the created object
     */
    public static AccessPattern createAccessPattern(String name) {
    	AccessPattern ap = new AccessPattern(name);
    	return ap;
    }
    
    /**
     * Create a PK
     * @param name the name
     * @return the created object
     */
    public static PrimaryKey createPrimaryKey(String name) {
    	PrimaryKey pk = new PrimaryKey(name);
    	return pk;
    }
    
    /**
     * Create a FK
     * @param name the name
     * @return the created object
     */
    public static ForeignKey createForeignKey(String name) {
    	ForeignKey fk = new ForeignKey(name);
    	return fk;
    }
    
    /**
     * Create a Index
     * @param name the name
     * @return the created object
     */
    public static Index createIndex(String name) {
    	Index index = new Index(name);
    	return index;
    }
    
    /**
     * Create a Parameter
     * @param name the name
     * @return the created object
     */
    public static Parameter createParameter(String name) {
    	Parameter param = new Parameter(name);
    	return param;
    }
    
    /**
     * Create a UniqueConstraint
     * @param name the name
     * @return the created object
     */
    public static UniqueConstraint createUniqueConstraint(String name) {
    	UniqueConstraint uc = new UniqueConstraint(name);
    	return uc;
    }
    
    /**
     * Create a ProcedureResultSet
     * @param name the name
     * @return the created object
     */
    public static ProcedureResultSet createProcedureResultSet(String name) {
    	ProcedureResultSet rs = new ProcedureResultSet(name);
    	return rs;
    }

    /**
     * Create a Schema
     * @param name the name
     * @return the created object
     */
    public static Schema createSchema(String name) {
    	Schema rs = new Schema(name);
    	return rs;
    }
    
    /**
     * Compares expected Properties to actual Properties
     * @param expected the expected props
     * @param actual the actual props
     * @return the message (empty if successful)
     */
    public static String compareExtensionProperties(Properties expected, Properties actual) {
    	StringBuffer sb = new StringBuffer();
    	// Check actual versus expected number of properties
    	if(actual.size()!=expected.size()) {
    		sb.append("Expected ["+expected.size()+"] properties, but actual number is ["+actual.size()+"]");  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
    		return sb.toString();
    	}
    	
    	expected.keySet();
    	for(Object key : expected.keySet()) {
    		// See if the expected key is in the actual set
    		if(actual.getProperty((String)key)==null) {
    			sb.append("Expected property ["+(String)key+"] is not present in the actual properties");  //$NON-NLS-1$//$NON-NLS-2$
    			break;
    		}
    		
    		// Check that values are the same
    		Object expectedValue = expected.get(key);
    		Object actualValue = actual.get(key);
    		if(expectedValue==null && actualValue!=null) {
    			sb.append("Expected value of ["+(String)key+"] is NULL but actual value is non-null"); //$NON-NLS-1$ //$NON-NLS-2$
    			break;
    		}
    		
    		if(!expectedValue.toString().equals(actualValue.toString())) {
    			sb.append(key+": Expected ["+expectedValue.toString()+"], but actual value is ["+actualValue.toString()+"]");  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
    			break;
    		}
    	}
    	
    	return sb.toString();
    }

}
