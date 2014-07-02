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
package org.komodo.spi.query.sql.lang;

import java.util.List;

import org.komodo.spi.query.sql.symbol.IElementSymbol;


/**
 *
 */
public interface ISPParameter<E extends IElementSymbol> {

    /**
     * Enumerator for types of parameters 
     */
    enum ParameterInfo {
        /** Constant identifying an IN parameter */
        IN,

        /** Constant identifying an OUT parameter */
        OUT,

        /** Constant identifying an INOUT parameter */
        INOUT,

        /** Constant identifying a RETURN parameter */
        RETURN_VALUE,

        /** Constant identifying a RESULT SET parameter */
        RESULT_SET;

        /**
         * Get the index of the enumerator. For compatibility
         * with existing code, the index starts at 1 rather than 0.
         * 
         * @return value of index
         */
        public int index() {
            return ordinal() + 1;
        }

        public static ParameterInfo valueOf(int type) {
            for (ParameterInfo info : ParameterInfo.values()) {
                if (info.index() == type)
                    return info;
            }

            throw new IllegalArgumentException();
        }
    }

    /**
     * Add a result set column if this parameter is a return
     * result set.
     * 
     * @param colName Name of column
     * @param type Type of column
     * @param id id of column
     */
    void addResultSetColumn(String colName, Class<?> type, Object id);
    
    /**
     * Get element symbol representing this parameter.  The symbol will have the
     * same name and type as the parameter.
     * 
     * @return Element symbol representing the parameter
     */
    E getParameterSymbol();

    /**
     * Get full parameter name,.  If unknown, null is returned.
     * 
     * @return Parameter name
     */
    String getName();

    /**
     * Set full parameter name
     * 
     * @param name Parameter name
     */
    void setName(String name); 

    /**
     * Get type of parameter according to class constants.
     * 
     * @return Parameter type
     */
    int getParameterType();
    
    /**
     * Set parameter type according to class constants.
     * 
     * @param parameterType Type to set
     */
    void setParameterType(ParameterInfo parameterType);

    /**
     * Get the class type
     * 
     * @return class type
     */
    Class<?> getClassType();
    
    /**
     * Set the class type
     * 
     * @param klazz
     */
    void setClassType(Class<?> klazz);

    /**
     * Get the metadata ID
     * 
     * @return the metadata ID object
     */
    Object getMetadataID();
    
    /**
     * Set the metadata ID object
     * 
     * @param object
     */
    void setMetadataID(Object object);

    /**
     * @return list of result set columns
     */
    List<E> getResultSetColumns();

}
