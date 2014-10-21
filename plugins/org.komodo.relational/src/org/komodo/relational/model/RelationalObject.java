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
package org.komodo.relational.model;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import org.komodo.relational.constants.RelationalConstants;
import org.komodo.relational.core.RelationalObjectValidator;
import org.komodo.relational.core.RelationalValidator;
import org.komodo.relational.extension.RelationalModelExtensionConstants;
import org.komodo.spi.outcome.IOutcome;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.HashCodeUtils;
import org.komodo.utils.ModelType;
import org.komodo.utils.StringUtils;


/**
 * 
 *
 *
 */
public abstract class RelationalObject implements RelationalConstants, RelationalModelExtensionConstants.PropertyKeysNoPrefix {
    @SuppressWarnings("javadoc")
	public static final String KEY_NAME = "NAME"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_NAME_IN_SOURCE = "NAMEINSOURCE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_DESCRIPTION = "DESCRIPTION"; //$NON-NLS-1$
    
    @SuppressWarnings("javadoc")
    public static final int IGNORE = -1;
    @SuppressWarnings("javadoc")
    public static final int CREATE_ANYWAY = 0;
    @SuppressWarnings("javadoc")
    public static final int REPLACE = 1;
    @SuppressWarnings("javadoc")
    public static final int CREATE_UNIQUE_NAME = 2;
    
    private RelationalObject parent;
    private String  name;
    private String  nameInSource;
    private String  description;
    
    private int processType;
    
    protected IOutcome currentOutcome;
    
    private boolean isChecked = true;
    
    private int modelType = ModelType.PHYSICAL;
    
    private Properties extensionProperties = new Properties();
    
    private RelationalValidator validator = new RelationalObjectValidator();

    /**
     * RelationalReference constructor
     */
    public RelationalObject() {
        super();
        this.processType = CREATE_ANYWAY;
        this.currentOutcome = OutcomeFactory.getInstance().createOK();
        this.isChecked = true;
    }
    
    /**
     * RelationalReference constructor
     * @param name the name of the object
     */
    public RelationalObject( String name ) {
        super();
        this.name = name;
        this.processType = CREATE_ANYWAY;
        this.isChecked = true;
    }
    


    /* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	protected Object clone() throws CloneNotSupportedException {
		return super.clone();
	}
	
    /**
     * Set the object common properties
     * @param props the properties
     */
    public void setProperties(Properties props) {
        for( Object key : props.keySet() ) {
            String keyStr = (String)key;
            String value = props.getProperty(keyStr);

            if( value != null && value.length() == 0 ) {
                continue;
            }
            
            if( keyStr.equalsIgnoreCase(KEY_NAME) ) {
                setName(value);
            } else if(keyStr.equalsIgnoreCase(KEY_NAME_IN_SOURCE) ) {
                setNameInSource(value);
            } else if(keyStr.equalsIgnoreCase(KEY_DESCRIPTION) ) {
                setDescription(value);
            } 
        }
    	
        handleInfoChanged();
    }
    
    /**
     * Get the properties for this object
     * @return the properties
     */
    public Map<String,String> getProperties() {
    	Map<String,String> props = new HashMap<String,String>();
    	
    	props.put(KEY_NAME, getName());
    	props.put(KEY_NAME_IN_SOURCE, getNameInSource());
    	props.put(KEY_DESCRIPTION, getDescription());
    	
    	return props;
    }
	
	/**
	 * @param obj the relational reference
	 */
	public void inject(RelationalObject obj) {
		
	}
	/**
     * @return parent
     */
    public RelationalObject getParent() {
        return parent;
    }
    
	/**
	 * Add the supplied object as a child
     * @param relObj the object to add
     * @return 'true' if add was a success, 'false' if not
     */
    public boolean addChild(RelationalObject relObj) {
    	return false;
    }
    
	/**
	 * Remove the supplied object from child list
     * @param relObj the object to remove
     * @return 'true' if remove was a success, 'false' if not
     */
    public boolean removeChild(RelationalObject relObj) {
    	return false;
    }
    
	/**
     * @return parent
     */
    public Collection<RelationalObject> getChildren() {
        return Collections.emptyList();
    }
    
	/**
	 * Get the valid child types for this object
     * @return parent
     */
    public Collection<Integer> getValidChildTypes() {
        return Collections.emptyList();
    }
    
	/**
	 * Get the child of specified type at the specified path
	 * @param pathToChild the relative path to the child
	 * @param objType the type of relational object
     * @return the object - null if not found
     */
    public RelationalObject getChildAtPath(List<String> pathToChild, int objType) {
    	RelationalObject resultObj = null;
    	RelationalObject currentChild = this;
    	
    	for(int i=0; i<pathToChild.size(); i++) {
    		String currentElem = pathToChild.get(i);
        	Collection<RelationalObject> children = currentChild.getChildren();
        	// Look for matching child at this level
        	boolean foundChild = false;
        	for(RelationalObject child : children) {
        		if(currentElem.equalsIgnoreCase(child.getName())) {
        			currentChild=child;
        			if(i==pathToChild.size()-1) {
        				if(currentChild.getType()==objType) {
        					resultObj = currentChild;
        					foundChild=true;
        					break;
        				}
        			}
        			foundChild=true;
        			break;
        		}
        	}
    		if(!foundChild) break;
     	}

    	return resultObj;
    }

    /**
     * @param parent Sets parent to the specified value.
     */
    public void setParent( RelationalObject parent ) {
    	if(this.parent!=null && !this.parent.equals(parent)) {
    		this.parent.removeChild(this);
    	}
    	this.parent = parent;
    	this.parent.addChild(this);
        handleInfoChanged();
    }
    /**
     * @return name
     */
    public String getName() {
        return name;
    }
    /**
     * @param name Sets name to the specified value.
     */
    public void setName( String name ) {
    	if( StringUtils.areDifferent(this.name, name) ) {
    		this.name = name;
    		handleInfoChanged();
    	}
    }
    /**
     * @return nameInSource
     */
    public String getNameInSource() {
        return nameInSource;
    }
    /**
     * @param nameInSource Sets nameInSource to the specified value.
     */
    public void setNameInSource( String nameInSource ) {
    	if( StringUtils.areDifferent(this.nameInSource, nameInSource) ) {
    		this.nameInSource = nameInSource;
    		handleInfoChanged();
    	} 
    }
    /**
     * @return description
     */
    public String getDescription() {
        return description;
    }
    /**
     * @param description Sets description to the specified value.
     */
    public void setDescription( String description ) {
    	if( StringUtils.areDifferent(this.description, description) ) {
    		this.description = description;
    		handleInfoChanged();
    	} 
    }
    
    /**
     * @return the model type
     */
    public int getModelType() {
        return this.modelType;
    }
    
    /**
     * @param value the model type
     */
    public void setModelType(int value) {
        this.modelType = value;
    }
    
    /**
     * @return type
     */
    public int getType() {
        return TYPES.UNDEFINED;
    }
    
    /**
     * @return the process type
     */
    public int getProcessType() {
        return this.processType;
    }

    /**
     * @param value the type of processing
     * 
     */
    public void setDoProcessType(int value) {
        this.processType = value;
    }
    
    /**
     * @return the isChecked state
     */
    public boolean isChecked() {
        return this.isChecked;
    }

    /**
     * sets selected flag
     * @param isChecked 'true' if the item is selected
     * 
     */
    public void setChecked(boolean isChecked) {
        this.isChecked = isChecked;
    }    

    /**
     * Set the extension properties
     * @param extProps the extension properties
     */
    public void setExtensionProperties(Properties extProps) {
    	clearExtensionProperties();
    	if(extProps!=null) {
    		Set<Object> propKeys = extProps.keySet();
    		for(Object propKey : propKeys) {
    			String strKey = (String)propKey;
    			String strValue = extProps.getProperty(strKey);
    			int index = strKey.indexOf(':');
    			if(index!=-1) {
    				strKey = strKey.substring(index+1);
    			}
    			// TODO: Supports ID Lookup is not being returned in DDL Options - need to resolve.
    			if(strKey!=null && !strKey.equalsIgnoreCase("Supports ID Lookup")) {  //$NON-NLS-1$
    				addExtensionProperty(strKey,strValue);
    			}
    		}
    	}
    }
    
    /**
     * Add an extension property
     * @param propName property name
     * @param propValue property value
     */
    public void addExtensionProperty(String propName, String propValue) {
    	if(propName!=null) this.extensionProperties.put(propName,propValue);
    }
    
    /**
     * remove an extension property
     * @param propName property name
     */
    public void removeExtensionProperty(String propName) {
    	this.extensionProperties.remove(propName);
    }
    
    /**
     * clear the extension properties
     */
    public void clearExtensionProperties() {
    	this.extensionProperties.clear();
    }

    /**
     * @return the extension properties
     */
    public Properties getExtensionProperties() {
    	return this.extensionProperties;
    }
    
    
    
    /**
     * @return the display name
     */
    public String getDisplayName() {
    	return TYPE_NAMES[getType()];
    }

    /**
     * @return the current status
     */
    public IOutcome getOutcome() {
    	return this.currentOutcome;
    }

    /**
     * Get the validator
     * @return the string name validator
     */
    public RelationalValidator getValidator() {
    	return this.validator;
    }

    /**
     * @param validator the relational validator
     * 
     */
    public void setValidator(RelationalValidator validator) {
    	ArgCheck.isNotNull(validator, "validator"); //$NON-NLS-1$
    	this.validator = validator;
    }
    
    protected void handleInfoChanged() {
    	validate();
    }
    
    /**
     * @return the validation status
     */
    public IOutcome validate() {
    	return getValidator().validate(this);
    }
    
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(this.getClass().getName());
		sb.append(" : name = ").append(getName()); //$NON-NLS-1$
		return sb.toString();
	}
	
    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object object ) {
        if (this == object)
            return true;
        if (object == null)
            return false;
        if (getClass() != object.getClass())
            return false;
        final RelationalObject other = (RelationalObject)object;

        // string properties
        if (!StringUtils.valuesAreEqual(getName(), other.getName())
                || !StringUtils.valuesAreEqual(getNameInSource(), other.getNameInSource())
                || !StringUtils.valuesAreEqual(getDescription(), other.getDescription())) {
            return false;
        }
        
        if( !(getType()==other.getType()) ) {
        	return false;
        }
        if( !(getModelType()==other.getModelType()) ) {
        	return false;
        }
        if( !(getProcessType()==other.getProcessType()) ) {
        	return false;
        }
        if(!getExtensionProperties().equals(other.getExtensionProperties())) {
        	return false;
        }

        return true;
    }
    
    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        int result = HashCodeUtils.hashCode(0, getType());

        result = HashCodeUtils.hashCode(result, getType());
        result = HashCodeUtils.hashCode(result, getModelType());
        result = HashCodeUtils.hashCode(result, getProcessType());
        
        // string properties
        if (!StringUtils.isEmpty(getName())) {
            result = HashCodeUtils.hashCode(result, getName());
        }
        
        if (!StringUtils.isEmpty(getNameInSource())) {
            result = HashCodeUtils.hashCode(result, getNameInSource());
        }

        if (getDescription() != null && !getDescription().isEmpty()) {
            result = HashCodeUtils.hashCode(result, getDescription());
        }

        if ((this.extensionProperties != null) && !this.extensionProperties.isEmpty()) {
        	Iterator<Object> keyIter = this.extensionProperties.keySet().iterator();
        	while(keyIter.hasNext()) {
        		String key = (String)keyIter.next();
        		String value = this.extensionProperties.getProperty(key);
        		result = HashCodeUtils.hashCode(result, key);
        		result = HashCodeUtils.hashCode(result, value);
        	}
        }

        return result;
    } 
    
    /**
     * Reference comparator
     */
    public class ReferenceComparator implements Comparator<RelationalObject> {
    	@Override
    	public int compare(RelationalObject x, RelationalObject y) {
    		RelationalObject xParent = x.getParent();
    		RelationalObject yParent = y.getParent();

    		// if either of parents null, just use names
    		if(xParent==null || yParent==null) {
        	    return x.getName().compareTo(y.getName());
    		}
    		
    		int parentResult = xParent.getName().compareTo(yParent.getName());
    	    if (parentResult != 0) return parentResult;

    	    // if parent names match, use reference name
    	    return x.getName().compareTo(y.getName());
    	}

    }       
}
