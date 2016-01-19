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
import java.util.HashMap;
import java.util.Map;

/**
 * Implementation independent version of the Teiid class PropertyDefinition
 * 
 *
 */
public class TeiidPropertyDefinition {

    private final static String NAME = "name"; //$NON-NLS-1$
    
    private final static String DISPLAY_NAME = "displayName"; //$NON-NLS-1$
    
    private final static String DESCRIPTION = "description"; //$NON-NLS-1$
    
    private final static String PROPERTY_TYPE_CLASS_NAME = "propertyTypeClassName"; //$NON-NLS-1$
    
    private final static String DEFAULT_VALUE = " defaultValue"; //$NON-NLS-1$
    
    private final static String ALLOWED_VALUES = "allowedValues"; //$NON-NLS-1$
    
    private final static String MODIFIABLE = "modifiable"; //$NON-NLS-1$
    
    private final static String CONSTRAINED_TO_ALLOWED_VALUES = "constrainedToAllowedValues"; //$NON-NLS-1$
    
    private final static String ADVANCED = "advanced"; //$NON-NLS-1$
    
    private final static String REQUIRED = "required"; //$NON-NLS-1$
    
    private final static String MASKED = "masked"; //$NON-NLS-1$

    /*
     * The owner or target / applicable property of the definition
     */
    private final static String OWNER = "owner"; //$NON-NLS-1$

    private Map<String, Object> properties = new HashMap<String, Object>();
    
    private <V> V getProperty(String key, Class<V> klazz) {
        Object value = properties.get(key);
        
        if (value == null && Boolean.class.equals(klazz))
            value = Boolean.FALSE;
        else if (value == null)
            return null;
        
        return klazz.cast(value);
    }
    
    private void setProperty(String key, Object value) {
        properties.put(key, value);
    }
    
    /**
     * @return the name
     */
    public String getName() {
        return getProperty(NAME, String.class);
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        setProperty(NAME, name);
    }


    /**
     * @return the displayName
     */
    public String getDisplayName() {
        return getProperty(DISPLAY_NAME, String.class);
    }

    /**
     * @param displayName the displayName to set
     */
    public void setDisplayName(String displayName) {
        setProperty(DISPLAY_NAME, displayName);
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return getProperty(DESCRIPTION, String.class);
    }

    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        setProperty(DESCRIPTION, description);
    }

    /**
     * @return the propertyTypeClassName
     */
    public String getPropertyTypeClassName() {
        return getProperty(PROPERTY_TYPE_CLASS_NAME, String.class);
    }

    /**
     * @param propertyTypeClassName the propertyTypeClassName to set
     */
    public void setPropertyTypeClassName(String propertyTypeClassName) {
        setProperty(PROPERTY_TYPE_CLASS_NAME, propertyTypeClassName);
    }

    /**
     * @return the defaultValue
     */
    public Object getDefaultValue() {
        return getProperty(DEFAULT_VALUE, Object.class);
    }

    /**
     * @param defaultValue the defaultValue to set
     */
    public void setDefaultValue(Object defaultValue) {
        setProperty(DEFAULT_VALUE, defaultValue);
    }

    /**
     * @return the allowedValues
     */
    @SuppressWarnings("unchecked")
	public Collection<String> getAllowedValues() {
        return getProperty(ALLOWED_VALUES, Collection.class);
    }

    /**
     * @param allowedValues the allowedValues to set
     */
    public void setAllowedValues(Collection<String> allowedValues) {
        setProperty(ALLOWED_VALUES, allowedValues);
    }

    /**
     * @return the modifiable
     */
    public boolean isModifiable() {
        return getProperty(MODIFIABLE, Boolean.class);
    }

    /**
     * @param modifiable the modifiable to set
     */
    public void setModifiable(boolean modifiable) {
        setProperty(MODIFIABLE, modifiable);
    }

    /**
     * @return the constrainedToAllowedValues
     */
    public boolean isConstrainedToAllowedValues() {
        return getProperty(CONSTRAINED_TO_ALLOWED_VALUES, Boolean.class);
    }

    /**
     * @param constrainedToAllowedValues the constrainedToAllowedValues to set
     */
    public void setConstrainedToAllowedValues(boolean constrainedToAllowedValues) {
        setProperty(CONSTRAINED_TO_ALLOWED_VALUES, constrainedToAllowedValues);
    }

    /**
     * @return the advanced
     */
    public boolean isAdvanced() {
        return getProperty(ADVANCED, Boolean.class);
    }

    /**
     * @param advanced the advanced to set
     */
    public void setAdvanced(boolean advanced) {
        setProperty(ADVANCED, advanced);
    }

    /**
     * @return the required
     */
    public boolean isRequired() {
        return getProperty(REQUIRED, Boolean.class);
    }

    /**
     * @param required the required to set
     */
    public void setRequired(boolean required) {
        setProperty(REQUIRED, required);
    }

    /**
     * @return the masked
     */
    public boolean isMasked() {
        return getProperty(MASKED, Boolean.class);
    }

    /**
     * @param masked the masked to set
     */
    public void setMasked(boolean masked) {
        setProperty(MASKED, masked);
    }

    /**
     * @return owner or target object type
     */
    public String getOwner() {
        return getProperty(OWNER, String.class);
    }

    /**
     * @param owner
     */
    public void setOwner(String owner) {
        setProperty(OWNER, owner);
    }
}
