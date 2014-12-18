/*
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
*/
package org.komodo.relational.model.legacy;

import java.util.Properties;

/**
 *
 */
public class RelationalUtil {
	
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
