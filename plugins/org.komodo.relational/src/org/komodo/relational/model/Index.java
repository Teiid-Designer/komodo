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
import java.util.List;
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
public class Index extends RelationalObject {

    @SuppressWarnings("javadoc")
	public static final String KEY_AUTO_UPDATE = "AUTOUPDATE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_FILTER_CONDITION = "FILTERCONDITION"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_NULLABLE = "NULLABLE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_UNIQUE = "UNIQUE"; //$NON-NLS-1$
    
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_AUTO_UPDATE = false;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_FILTER_CONDITION = null;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_NULLABLE = false;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_UNIQUE = false;
    
    private List<Column> columns;
    private boolean autoUpdate;
    private String  filterCondition;
    private boolean nullable;
    private boolean unique;
    
    private boolean existingTable;
    private Table relationalTable;
    
    /**
     * 
     */
    public Index() {
        super();
        this.columns = new ArrayList<Column>();
        setNameValidator(new RelationalStringNameValidator(false));
    }
    
    /**
     * @param name the index name
     */
    public Index( String name ) {
        super(name);
        this.columns = new ArrayList<Column>();
        setNameValidator(new RelationalStringNameValidator(false));
    }
    
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.INDEX;
    }

    @Override
	public Index clone() {
    	Index clonedIndex = new Index(getName());
    	clonedIndex.setNameInSource(getNameInSource());
    	clonedIndex.setDescription(getDescription());
    	clonedIndex.setModelType(getModelType());
    	clonedIndex.setUnique(isUnique());
    	clonedIndex.setAutoUpdate(isAutoUpdate());
    	clonedIndex.setFilterCondition(getFilterCondition());
    	clonedIndex.setNullable(isNullable());
    	for( Column col : getColumns() ) {
    		clonedIndex.addColumn(col);
    	}
    	return clonedIndex;
    }
    
    @Override
    public void inject(RelationalObject originalIndex) {
    	super.inject(originalIndex);
    	Index theIndex = (Index)originalIndex;
    	setName(theIndex.getName());
    	setNameInSource(theIndex.getNameInSource());
    	setDescription(theIndex.getDescription());
    	setModelType(theIndex.getModelType());
    	setFilterCondition(theIndex.getFilterCondition());
    	setNullable(theIndex.isNullable());
    	setAutoUpdate(theIndex.isAutoUpdate());
    	setUnique(theIndex.isUnique());
    	getColumns().clear();
    	for( Column col : theIndex.getColumns() ) {
    		addColumn(col);
    	}
    }
    
    /**
     * @return columns
     */
    public List<Column> getColumns() {
        return columns;
    }

    /**
     * @param column the collumn
     */
    public void addColumn( Column column ) {
        this.columns.add(column);
    }
    /**
     * @return autoUpdate
     */
    public boolean isAutoUpdate() {
        return autoUpdate;
    }
    /**
     * @param autoUpdate Sets autoUpdate to the specified value.
     */
    public void setAutoUpdate( boolean autoUpdate ) {
        this.autoUpdate = autoUpdate;
    }
    /**
     * @return filterCondition
     */
    public String getFilterCondition() {
        return filterCondition;
    }
    /**
     * @param filterCondition Sets filterCondition to the specified value.
     */
    public void setFilterCondition( String filterCondition ) {
        this.filterCondition = filterCondition;
    }
    /**
     * @return nullable
     */
    public boolean isNullable() {
        return nullable;
    }
    /**
     * @param nullable Sets nullable to the specified value.
     */
    public void setNullable( boolean nullable ) {
        this.nullable = nullable;
    }
    /**
     * @return unique
     */
    public boolean isUnique() {
        return unique;
    }
    /**
     * @param unique Sets unique to the specified value.
     */
    public void setUnique( boolean unique ) {
        this.unique = unique;
    }
    
    /**
	 * @return the existingTable
	 */
	public boolean usesExistingTable() {
		return this.existingTable;
	}

	/**
	 * @param usesExistingTable the existingTable to set
	 */
	public void setUsesExistingTable(boolean usesExistingTable) {
		this.existingTable = usesExistingTable;
	}

	/**
	 * @return the relationalTable
	 */
	public Table getRelationalTable() {
		return this.relationalTable;
	}

	/**
	 * @param relationalTable the relationalTable to set
	 */
	public void setRelationalTable(Table relationalTable) {
		this.relationalTable = relationalTable;
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
            
            if(keyStr.equalsIgnoreCase(KEY_NULLABLE) ) {
                setNullable(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_UNIQUE) ) {
                setUnique(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_AUTO_UPDATE) ) {
                setAutoUpdate(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_FILTER_CONDITION) ) {
                setFilterCondition(value);
            }
        }
    	
        handleInfoChanged();
    }
    
	@Override
	public IStatus validate() {
		// Walk through the properties for the table and set the status
		super.validate();
		
		if( getStatus().getSeverity() == IStatus.ERROR ) {
			return this.currentStatus;
		}
		
		
		
		// Check Column Status values
		for( Column col : getColumns() ) {
			if( col.getStatus().getSeverity() == IStatus.ERROR ) {
				this.currentStatus = new Status(IStatus.ERROR, PLUGIN_ID, col.getStatus().getMessage() );
				return this.currentStatus;
			}
		}
		
		// Check Column Status values
		for( Column outerColumn : getColumns() ) {
			for( Column innerColumn : getColumns() ) {
				if( outerColumn != innerColumn ) {
					if( outerColumn.getName().equalsIgnoreCase(innerColumn.getName())) {
						this.currentStatus = new Status(IStatus.ERROR, PLUGIN_ID, 
								Messages.getString(RELATIONAL.validate_error_duplicateColumnNamesReferencedInIndex, getName()) ); 
						return this.currentStatus;
					}
				}
			}
		}
		
		if( this.getColumns().isEmpty() ) {
			this.currentStatus = new Status(IStatus.WARNING, PLUGIN_ID, 
					Messages.getString(RELATIONAL.validate_warning_noColumnReferencesDefined, getName()) );
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
        final Index other = (Index)object;


        
        // string properties
        if (!CoreStringUtil.valuesAreEqual(getFilterCondition(), other.getFilterCondition()) ) {
            return false;
        }
        
        if( !(isAutoUpdate()==other.isAutoUpdate()) ||  
            !(isNullable()==other.isNullable()) ||
            !(isUnique()==other.isUnique()) ) {
        	return false;
        }
        
        // Table
        if (relationalTable == null) {
            if (other.relationalTable != null)
                return false;   
        } else if (!relationalTable.equals(other.relationalTable))
            return false;

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
        if (!CoreStringUtil.isEmpty(getFilterCondition())) {
            result = HashCodeUtil.hashCode(result, getFilterCondition());
        }
        
        result = HashCodeUtil.hashCode(result, isAutoUpdate());
        result = HashCodeUtil.hashCode(result, isNullable());
        result = HashCodeUtil.hashCode(result, isUnique());
       
        if(relationalTable!=null) {
            result = HashCodeUtil.hashCode(result, relationalTable);
        }

        List<Column> cols = getColumns();
        for(Column col: cols) {
            result = HashCodeUtil.hashCode(result, col);
        }
        
        return result;
    }    
    
}
