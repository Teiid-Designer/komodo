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
package org.komodo.spi.runtime;

import java.util.Collection;
import java.util.Properties;

/**
 *
 *
 */
public interface TeiidTranslator {
	
	enum TranslatorPropertyType{IMPORT, OVERRIDE, EXTENSION_METADATA}

    /**
     * Obtains all the names of the properties whose values are invalid.
     * 
     * @return the names of the properties with invalid values (never <code>null</code> but can be empty)
     */
    Collection<String> findInvalidProperties(TranslatorPropertyType propType);

    /**
     * @return name of this translator
     */
    String getName();

    /**
     * @return the properties of this translator
     */
    Properties getProperties();

    /**
     * @return the value of the given property
     */
    String getPropertyValue(String name, TranslatorPropertyType type);

    /**
     * @return type
     */
    String getType();

    /**
     * @return the execution teiidInstance (never <code>null</code>)
     */
    TeiidInstance getTeiidInstance();

    /**
     * @return the string version of the default value for each property (empty string if no default)
     */
    Properties getDefaultPropertyValues();

    /**
     * @param name the property name
     * @param value the proposed new value
     * @return null if the property exists and the proposed value is valid or an error message
     *
     */
    String isValidPropertyValue(String name, String value, TranslatorPropertyType type);

    /**
     * Sets a connector property.
     * 
     * @param name the property name (never <code>null</code>)
     * @param value the new property value
     * @throws Exception if there is a problem changing the property
     *
     */
    void setPropertyValue(String name, String value, TranslatorPropertyType type) throws Exception;

    /**
     * @param changedProperties the list of properties that are being changed (never <code>null</code> or empty)
     * @throws Exception if there is a problem changing the properties
     *
     */
    void setProperties(Properties changedProperties) throws Exception;

    /**
     * @param name the name of the <code>TeiidPropertyDefinition</code> being requested (never <code>null</code> or empty)
     * @return the property definition or <code>null</code> if not found
     */
    TeiidPropertyDefinition getPropertyDefinition( String name , TranslatorPropertyType type);
    
    /**
     * @return an immutable collection of property definitions (never <code>null</code>);
     *
     */
    Collection<TeiidPropertyDefinition> getPropertyDefinitions();
    
    /**
     * @return an immutable collection of import property definitions (never <code>null</code>);
     *
     */
    Collection<TeiidPropertyDefinition> getImportPropertyDefinitions();
    
    /**
     * @return an immutable collection of import property definitions (never <code>null</code>);
     *
     */
    Collection<TeiidPropertyDefinition> getExtensionPropertyDefinitions();
}
