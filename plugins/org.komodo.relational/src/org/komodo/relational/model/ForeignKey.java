/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;

import org.komodo.core.CoreStringUtil;
import org.komodo.core.HashCodeUtil;
import org.komodo.core.IStatus;
import org.komodo.core.Status;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.RELATIONAL;
import org.komodo.relational.core.RelationalStringNameValidator;



/**
 * 
 *
 * @since 8.0
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
        setNameValidator(new RelationalStringNameValidator(false));
    }
    
    /**
     * RelationalForeignKey constructor
     * @param name the FK name
     */
    public ForeignKey( String name ) {
        super(name);
        this.columns = new ArrayList<Column>();
        setNameValidator(new RelationalStringNameValidator(false));
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
    
	@Override
	public IStatus validate() {
		// Walk through the properties for the table and set the status
		this.currentStatus = super.validate();
		
		if( !this.currentStatus.isOK() ) {
			return this.currentStatus;
		}
		
		if( this.getColumns().isEmpty() ) {
			this.currentStatus = new Status(IStatus.ERROR, PLUGIN_ID, 
					Messages.getString(RELATIONAL.validate_error_fkNoColumnsDefined, getName()) );
			return this.currentStatus;
		}
				
		if( this.getUniqueKeyName() == null || this.getUniqueKeyName().length() == 0 ) {
			this.currentStatus = new Status(IStatus.ERROR, PLUGIN_ID, 
					Messages.getString(RELATIONAL.validate_error_fKUniqueKeyNameIsUndefined, getName()) );
			return this.currentStatus;
		}
		
		if( this.getUniqueKeyTableName() == null || this.getUniqueKeyTableName().length() == 0 ) {
			this.currentStatus = new Status(IStatus.ERROR, PLUGIN_ID, 
					Messages.getString(RELATIONAL.validate_error_fKReferencedUniqueKeyTableIsUndefined) );
			return this.currentStatus;
		}
		return this.currentStatus;
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
        if (!CoreStringUtil.valuesAreEqual(getForeignKeyMultiplicity(), other.getForeignKeyMultiplicity()) ||
        		!CoreStringUtil.valuesAreEqual(getPrimaryKeyMultiplicity(), other.getPrimaryKeyMultiplicity()) ||
        		!CoreStringUtil.valuesAreEqual(getUniqueKeyName(), other.getUniqueKeyName()) || 
        		!CoreStringUtil.valuesAreEqual(getUniqueKeyTableName(), other.getUniqueKeyTableName()) ) {
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
        if (!CoreStringUtil.isEmpty(getForeignKeyMultiplicity())) {
            result = HashCodeUtil.hashCode(result, getForeignKeyMultiplicity());
        }
        if (!CoreStringUtil.isEmpty(getPrimaryKeyMultiplicity())) {
            result = HashCodeUtil.hashCode(result, getPrimaryKeyMultiplicity());
        }
        if (!CoreStringUtil.isEmpty(getUniqueKeyName())) {
            result = HashCodeUtil.hashCode(result, getUniqueKeyName());
        }
        if (!CoreStringUtil.isEmpty(getUniqueKeyTableName())) {
            result = HashCodeUtil.hashCode(result, getUniqueKeyTableName());
        }
        
        Collection<Column> cols = getColumns();
        for(Column col: cols) {
            result = HashCodeUtil.hashCode(result, col);
        }
                
        return result;
    }    

}
