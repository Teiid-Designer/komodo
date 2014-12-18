/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.ddl.importer;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.komodo.relational.model.legacy.RelationalObject;

/**
 *
 */
public class TestUtil {

	@SuppressWarnings("javadoc")
	public static final Map<String,String> REL_OBJ_DEFAULTS = new HashMap<String,String>();
	static {
		REL_OBJ_DEFAULTS.put("NAME", null); //$NON-NLS-1$
		REL_OBJ_DEFAULTS.put("NAMEINSOURCE", null); //$NON-NLS-1$
		REL_OBJ_DEFAULTS.put("DESCRIPTION", null); //$NON-NLS-1$
	}
	@SuppressWarnings("javadoc")
	public static final Map<String,String> TABLE_PROPERTY_DEFAULTS = new HashMap<String,String>();
	static {
		TABLE_PROPERTY_DEFAULTS.putAll(REL_OBJ_DEFAULTS);
		TABLE_PROPERTY_DEFAULTS.put("SYSTEM", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		TABLE_PROPERTY_DEFAULTS.put("MATERIALIZED", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		TABLE_PROPERTY_DEFAULTS.put("CARDINALITY", "-1"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	@SuppressWarnings("javadoc")
	public static final Map<String,String> PROCEDURE_PROPERTY_DEFAULTS = new HashMap<String,String>();
	static {
		PROCEDURE_PROPERTY_DEFAULTS.putAll(REL_OBJ_DEFAULTS);
		PROCEDURE_PROPERTY_DEFAULTS.put("FUNCTION", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		PROCEDURE_PROPERTY_DEFAULTS.put("UPDATECOUNT", null); //$NON-NLS-1$
	}
	@SuppressWarnings("javadoc")
	public static final Map<String,String> RESULTSET_PROPERTY_DEFAULTS = new HashMap<String,String>();
	static {
		PROCEDURE_PROPERTY_DEFAULTS.putAll(REL_OBJ_DEFAULTS);
		PROCEDURE_PROPERTY_DEFAULTS.put("FUNCTION", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		PROCEDURE_PROPERTY_DEFAULTS.put("UPDATECOUNT", null); //$NON-NLS-1$
	}
	@SuppressWarnings("javadoc")
	public static final Map<String,String> COLUMN_PROPERTY_DEFAULTS = new HashMap<String,String>();
	static {
		COLUMN_PROPERTY_DEFAULTS.putAll(REL_OBJ_DEFAULTS);
		COLUMN_PROPERTY_DEFAULTS.put("SEARCHABILITY", "SEARCHABLE"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("SIGNED", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("CHARACTEROCTETLENGTH", "0"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("DEFAULTVALUE", null); //$NON-NLS-1$
		COLUMN_PROPERTY_DEFAULTS.put("CHARACTERSETNAME", null); //$NON-NLS-1$
		COLUMN_PROPERTY_DEFAULTS.put("MAXIMUMVALUE", null); //$NON-NLS-1$
		COLUMN_PROPERTY_DEFAULTS.put("MINIMUMVALUE", null); //$NON-NLS-1$
		COLUMN_PROPERTY_DEFAULTS.put("AUTOINCREMENTED", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("NATIVETYPE", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("LENGTH", "10"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("SCALE", "0"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("LENGTHFIXED", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("NULLABLE", "NO_NULLS"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("SELECTABLE", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("NULLVALUECOUNT", "0"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("DATATYPE", "string"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("CURRENCY", "false"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("RADIX", "0"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("FORMAT", null); //$NON-NLS-1$
		COLUMN_PROPERTY_DEFAULTS.put("COLLATIONNAME", null); //$NON-NLS-1$
		COLUMN_PROPERTY_DEFAULTS.put("UPDATEABLE", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("CASESENSITIVE", "true"); //$NON-NLS-1$ //$NON-NLS-2$
		COLUMN_PROPERTY_DEFAULTS.put("DISTINCTVALUECOUNT", "-1"); //$NON-NLS-1$ //$NON-NLS-2$
	}

    /**
     * Determine if the parent has child objects of supplied type that match the list of supplied names
     * @param parent the parent
     * @param objectNames the list of names
     * @param objType the type of object
     * @return 'true' if objects match the supplied names, 'false' if not
     */
    public static boolean childrenMatch(RelationalObject parent, List<String> objectNames, int objType) {
    	List<RelationalObject> rObjs = new ArrayList<RelationalObject>();
    	Collection<RelationalObject> children = parent.getChildren();
    	for(RelationalObject relObj : children) {
    		if(relObj.getType()==objType) {
    			rObjs.add(relObj);
    		}
    	}

    	// Must have equal numbers
    	if(objectNames.size()!=rObjs.size()) {
    		return false;
    	}

    	for(RelationalObject rObj : rObjs) {
    		String objName = rObj.getName();
    		boolean found = false;
    		for(String roName : objectNames) {
    			if(roName.equalsIgnoreCase(objName)) {
    				found = true;
    				break;
    			}
    		}
    		if(!found) {
    			return false;
    		}
    	}

    	return true;
    }

    /**
     * Determine if the object properties match the expected properties
     * @param relObj the relational object
     * @param expectedProps the expected properties
     * @return 'true' if properties match, 'false' if not
     */
    public static String compareProperties(RelationalObject relObj, Map<String,String> expectedProps) {
    	String result = "OK";  //$NON-NLS-1$
    	Map<String,String> actualProps = relObj.getProperties();
    	for(String expectedName : expectedProps.keySet()) {
    		if(!actualProps.keySet().contains(expectedName)) {
    			result = "Object properties do not contain property '"+expectedName+"'";  //$NON-NLS-1$ //$NON-NLS-2$
    			break;
    		}
    		String expectedValue = expectedProps.get(expectedName);
    		String actualValue = actualProps.get(expectedName);
    		if(   (expectedValue==null && actualValue!=null)
    	       || (actualValue==null && expectedValue!=null)
     		   || (actualValue!=null && !actualValue.equals(expectedValue))) {
    			result = "'"+expectedName+"' Actual value ["+actualValue+"] does not match Expected value ["+expectedValue+"]";  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    			break;
    		}
    	}
    	return result;
    }

}
