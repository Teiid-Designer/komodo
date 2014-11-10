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
package org.komodo.spi.udf;

import java.util.List;

/**
 *
 */
public interface FunctionLibrary {
    
    public enum FunctionName {
        // Special type conversion functions
        CONVERT("convert"), //$NON-NLS-1$
        CAST("cast"), //$NON-NLS-1$

        // Special lookup function
        LOOKUP("lookup"), //$NON-NLS-1$

        // Special user function
        USER("user"), //$NON-NLS-1$
        // Special environment variable lookup function
        ENV("env"), //$NON-NLS-1$
        SESSION_ID("session_id"), //$NON-NLS-1$
        
        // Special pseudo-functions only for XML queries
        CONTEXT("context"), //$NON-NLS-1$
        ROWLIMIT("rowlimit"), //$NON-NLS-1$
        ROWLIMITEXCEPTION("rowlimitexception"), //$NON-NLS-1$
        
        // Misc.
        DECODESTRING("decodestring"), //$NON-NLS-1$
        DECODEINTEGER("decodeinteger"), //$NON-NLS-1$
        COMMAND_PAYLOAD("commandpayload"), //$NON-NLS-1$
        
        CONCAT("CONCAT"), //$NON-NLS-1$
        CONCAT2("CONCAT2"), //$NON-NLS-1$
        CONCAT_OPERATOR("||"), //$NON-NLS-1$
        SUBSTRING("substring"), //$NON-NLS-1$
        NVL("NVL"), //$NON-NLS-1$
        IFNULL("IFNULL"), //$NON-NLS-1$
        
        FROM_UNIXTIME("from_unixtime"), //$NON-NLS-1$
        TIMESTAMPADD("timestampadd"), //$NON-NLS-1$
        
        PARSETIME("parsetime"), //$NON-NLS-1$
        PARSEDATE("parsedate"), //$NON-NLS-1$
        FORMATTIME("formattime"), //$NON-NLS-1$
        FORMATDATE("formatdate"), //$NON-NLS-1$
        
        NULLIF("nullif"), //$NON-NLS-1$
        COALESCE("coalesce"), //$NON-NLS-1$

        SPACE("space"), //$NON-NLS-1$
        ARRAY("array"), //$NON-NLS-1$
        ARRAY_GET("array_get"), //$NON-NLS-1$

        JSONARRAY("jsonarray"); //$NON-NLS-1$
        
        private final String text;
        
        FunctionName(String text) {
            this.text = text;
        }
        
        public String text() {
            return text;
        }

        /**
         * @return
         */
        public String toLowerCase() {
            return text.toLowerCase();
        }

        /**
         * @param name
         * @return
         */
        public boolean equalsIgnoreCase(String name) {
            return text.equalsIgnoreCase(name);
        }
    }
    
    /**
     * Get the function name according to the given enum value
     * 
     * @param functionName
     * 
     * @return function name of a function in the library
     */
    String getFunctionName(FunctionName functionName);

    /**
     * Categories of the functions in this library
     * 
     * @return names of the categories
     */
    List<String> getFunctionCategories();

    /**
     * Get the functions in the given category
     * 
     * @param category
     * 
     * @return those functions in the given category
     */
    @Deprecated
    <F extends FunctionForm> List<F> getFunctionForms(String category);

    /**
     * Find the function with the given name and number
     * of arguments
     * 
     * @param name
     * @param length number of arguments
     * 
     * @return function or null
     */
    @Deprecated
    FunctionForm findFunctionForm(String name, int length);

    /**
     * Find whether library contains function with the
     * given name and number of arguments
     * 
     * @param name
     * @param length number of arguments
     * 
     * @return true if library has function
     */
    boolean hasFunctionMethod(String name, int length);

    /**
     * Find a function descriptor given a name and the types of the arguments.
     * This method matches based on case-insensitive function name and
     * an exact match of the number and types of parameter arguments.
     * 
     * @param name {@link FunctionName} of the function
     * @param types Array of classes representing the types
     * 
     * @return Descriptor if found, null if not found
     */
    FunctionDescriptor findFunction(FunctionName name, Class<?>[] types);
    
    /**
     * Find a function descriptor given a name and the types of the arguments.
     * This method matches based on case-insensitive function name and
     * an exact match of the number and types of parameter arguments.
     * 
     * @param name Name of the function to resolve
     * @param types Array of classes representing the types
     * 
     * @return Descriptor if found, null if not found
     */
    FunctionDescriptor findFunction(String name, Class<?>[] types);
}
