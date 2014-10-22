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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Properties;

import org.komodo.utils.HashCodeUtils;
import org.komodo.utils.StringUtils;



/**
 * 
 *
 *
 */
public class ForeignKey extends RelationalObject {
    @SuppressWarnings("javadoc")
    public static final String KEY_FOREIGN_KEY_MULTIPLICITY = "FKMULTIPLICITY"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_PRIMARY_KEY_MULTIPLICITY = "PKMULTIPLICITY"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_UNIQUE_KEY_NAME = "UNIQUEKEYNAME"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_UNIQUE_KEY_TABLE_NAME = "UNIQUEKEYTABLENAME"; //$NON-NLS-1$
    
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_FOREIGN_KEY_MULTIPLICITY = MULTIPLICITY.ZERO_TO_MANY;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_PRIMARY_KEY_MULTIPLICITY = MULTIPLICITY.ONE;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_UNIQUE_KEY_NAME = null;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_UNIQUE_KEY_TABLE_NAME = null;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_ALLOW_JOIN = true;
    
    
    private Collection<Column> columns;
    private String foreignKeyMultiplicity;
    private String  primaryKeyMultiplicity;
    private String   uniqueKeyName;
    private String   uniqueKeyTableName;
    private boolean allowJoin = DEFAULT_ALLOW_JOIN;
    
    /**
     * RelationalForeignKey constructor
     */
    public ForeignKey() {
        super();
        this.columns = new ArrayList<Column>();
    }
    
    /**
     * RelationalForeignKey constructor
     * @param name the FK name
     */
    public ForeignKey( String name ) {
        super(name);
        this.columns = new ArrayList<Column>();
    }
    
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.FK;
    }

    @Override
	public ForeignKey clone() {
    	ForeignKey clonedFK = new ForeignKey(getName());
    	clonedFK.setNameInSource(getNameInSource());
    	clonedFK.setDescription(getDescription());
    	clonedFK.setForeignKeyMultiplicity(getForeignKeyMultiplicity());
    	clonedFK.setPrimaryKeyMultiplicity(getPrimaryKeyMultiplicity());
    	clonedFK.setUniqueKeyName(getUniqueKeyName());
    	clonedFK.setUniqueKeyTableName(getUniqueKeyTableName());
    	clonedFK.setModelType(getModelType());
    	clonedFK.setAllowJoin(isAllowJoin());
    	for( Column col : getColumns() ) {
    		clonedFK.addColumn(col);
    	}
    	return clonedFK;
    }
    
    @Override
    public void inject(RelationalObject originalFK) {
    	super.inject(originalFK);
    	ForeignKey theFK = (ForeignKey)originalFK;
    	setName(theFK.getName());
    	setNameInSource(theFK.getNameInSource());
    	setDescription(theFK.getDescription());
    	setForeignKeyMultiplicity(theFK.getForeignKeyMultiplicity());
    	setPrimaryKeyMultiplicity(theFK.getPrimaryKeyMultiplicity());
    	setUniqueKeyName(theFK.getUniqueKeyName());
    	setUniqueKeyTableName(theFK.getUniqueKeyTableName());
    	setModelType(theFK.getModelType());
    	getColumns().clear();
    	for( Column col : theFK.getColumns() ) {
    		addColumn(col);
    	}
    }
    
    /**
     * @return columns
     */
    public Collection<Column> getColumns() {
        return columns;
    }

    /**
     * Add a column to this FK
     * @param column the column
     */
    public void addColumn( Column column ) {
        this.columns.add(column);
    }
    
    /**
     * Remove a column
     * @param column the column to remove
     * @return 'true' if the move was successful
     */
    public boolean removeColumn(Column column) {
    	if( this.columns.remove(column) ) {
    		return true;
    	}
    	return false;
    }
    
    /**
     * @return foreignKeyMultiplicity
     */
    public String getForeignKeyMultiplicity() {
        return foreignKeyMultiplicity;
    }
    
    /**
     * @param foreignKeyMultiplicity Sets foreignKeyMultiplicity to the specified value.
     */
    public void setForeignKeyMultiplicity( String foreignKeyMultiplicity ) {
        if( foreignKeyMultiplicity == null || this.foreignKeyMultiplicity == null || !this.foreignKeyMultiplicity.equals(foreignKeyMultiplicity) ) {
        	this.foreignKeyMultiplicity = foreignKeyMultiplicity;
        	handleInfoChanged();
        } 
    }
    /**
     * @return primaryKeyMultiplicity
     */
    public String getPrimaryKeyMultiplicity() {
        return primaryKeyMultiplicity;
    }
    /**
     * @param primaryKeyMultiplicity Sets primaryKeyMultiplicity to the specified value.
     */
    public void setPrimaryKeyMultiplicity( String primaryKeyMultiplicity ) {
        if( primaryKeyMultiplicity == null || this.primaryKeyMultiplicity == null || !this.primaryKeyMultiplicity.equals(primaryKeyMultiplicity) ) {
        	this.primaryKeyMultiplicity = primaryKeyMultiplicity;
        	handleInfoChanged();
        } 
    }
    /**
     * @return uniqueKeyName
     */
    public String getUniqueKeyName() {
        return uniqueKeyName;
    }
    /**
     * @param uniqueKeyName Sets uniqueKeyName to the specified value.
     */
    public void setUniqueKeyName( String uniqueKeyName ) {
        if( uniqueKeyName == null || this.uniqueKeyName == null || !this.uniqueKeyName.equals(uniqueKeyName) ) {
        	this.uniqueKeyName = uniqueKeyName;
        	handleInfoChanged();
        }
    }
    
    /**
     * @return uniqueKeyTableName
     */
    public String getUniqueKeyTableName() {
        return uniqueKeyTableName;
    }
    
    /**
     * @param uniqueKeyTableName Sets uniqueKeyTableName to the specified value.
     */
    public void setUniqueKeyTableName( String uniqueKeyTableName ) {
        if( uniqueKeyTableName == null || this.uniqueKeyTableName == null || !this.uniqueKeyTableName.equals(uniqueKeyTableName) ) {
        	this.uniqueKeyTableName = uniqueKeyTableName;
        	handleInfoChanged();
        }
    }
    
    /**
     * @return the table
     */
    public Table getTable() {
    	if( getParent() != null ) {
    		return (Table)getParent();
    	}
    	
    	return null;
    }
    
    /**
     * @return allowJoin
     */
    public boolean isAllowJoin() {
        return allowJoin;
    }
    
    /**
     * @param allowJoin 'true' to allow join
     */
    public void setAllowJoin( boolean allowJoin ) {
        if( this.allowJoin != allowJoin ) {
        	this.allowJoin = allowJoin;
        	handleInfoChanged();
        }
    }

    /**
     * Get the properties for this object
     * @return the properties
     */
    @Override
	public Map<String,String> getProperties() {
    	Map<String,String> props = super.getProperties();
    	
    	props.put(KEY_FOREIGN_KEY_MULTIPLICITY, getForeignKeyMultiplicity());
    	props.put(KEY_PRIMARY_KEY_MULTIPLICITY, getPrimaryKeyMultiplicity());
    	props.put(KEY_UNIQUE_KEY_NAME, getUniqueKeyName());
    	props.put(KEY_UNIQUE_KEY_TABLE_NAME, getUniqueKeyTableName());
    	
    	return props;
    }

    /**
     * Set properties
     * @param props the properties
     */
    @Override
	public void setProperties(Properties props) {
    	// Set common properties
    	super.setProperties(props);
    	
        for( Object key : props.keySet() ) {
            String keyStr = (String)key;
            String value = props.getProperty(keyStr);

            if( value != null && value.length() == 0 ) {
                continue;
            }
            
            if(keyStr.equalsIgnoreCase(KEY_FOREIGN_KEY_MULTIPLICITY) ) {
                setForeignKeyMultiplicity(value);
            } else if(keyStr.equalsIgnoreCase(KEY_PRIMARY_KEY_MULTIPLICITY) ) {
                setPrimaryKeyMultiplicity(value);
            } else if(keyStr.equalsIgnoreCase(KEY_UNIQUE_KEY_NAME) ) {
                setUniqueKeyName(value);
            } else if(keyStr.equalsIgnoreCase(KEY_UNIQUE_KEY_TABLE_NAME) ) {
                setUniqueKeyTableName(value);
            }
        }
    	
        handleInfoChanged();
    }
    
	@Override
	public void handleInfoChanged() {
		super.handleInfoChanged();
		
		// Set extension properties here
		
		if( !this.allowJoin ) {
			getExtensionProperties().put(ALLOW_JOIN, Boolean.toString(this.isAllowJoin()) );
		} else getExtensionProperties().remove(ALLOW_JOIN);
			
	}
    
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(this.getClass().getName());
		sb.append(" : name = ").append(getName()); //$NON-NLS-1$
		if( !getColumns().isEmpty() ) {
			sb.append("\n\t").append(getColumns().size()).append(" columns"); //$NON-NLS-1$  //$NON-NLS-2$
			for( Column col : getColumns() ) {
				sb.append("\n\tcol = ").append(col); //$NON-NLS-1$
			}
		}
		return sb.toString();
	}
	
    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals( final Object object ) {
		if (!super.equals(object)) {
			return false;
		}
        if (this == object)
            return true;
        if (object == null)
            return false;
        if (getClass() != object.getClass())
            return false;
        final ForeignKey other = (ForeignKey)object;

        // string properties
        if (!StringUtils.valuesAreEqual(getForeignKeyMultiplicity(), other.getForeignKeyMultiplicity()) ||
        		!StringUtils.valuesAreEqual(getPrimaryKeyMultiplicity(), other.getPrimaryKeyMultiplicity()) ||
        		!StringUtils.valuesAreEqual(getUniqueKeyName(), other.getUniqueKeyName()) || 
        		!StringUtils.valuesAreEqual(getUniqueKeyTableName(), other.getUniqueKeyTableName()) ) {
            return false;
        }
        
        // Columns
        Collection<Column> thisColumns = getColumns();
        Collection<Column> thatColumns = other.getColumns();

        if (thisColumns.size() != thatColumns.size()) {
            return false;
        }
        
        if (!thisColumns.isEmpty() && !thisColumns.containsAll(thatColumns)) {
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
        int result = super.hashCode();

        // string properties
        if (!StringUtils.isEmpty(getForeignKeyMultiplicity())) {
            result = HashCodeUtils.hashCode(result, getForeignKeyMultiplicity());
        }
        if (!StringUtils.isEmpty(getPrimaryKeyMultiplicity())) {
            result = HashCodeUtils.hashCode(result, getPrimaryKeyMultiplicity());
        }
        if (!StringUtils.isEmpty(getUniqueKeyName())) {
            result = HashCodeUtils.hashCode(result, getUniqueKeyName());
        }
        if (!StringUtils.isEmpty(getUniqueKeyTableName())) {
            result = HashCodeUtils.hashCode(result, getUniqueKeyTableName());
        }
        
        Collection<Column> cols = getColumns();
        for(Column col: cols) {
            result = HashCodeUtils.hashCode(result, col);
        }
                
        return result;
    }    

}
