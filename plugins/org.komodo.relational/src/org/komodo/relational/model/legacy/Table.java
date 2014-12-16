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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import org.komodo.utils.HashCodeUtils;
import org.komodo.utils.StringUtils;


/**
 * 
 *
 *
 */
public class Table extends RelationalObject {
    @SuppressWarnings("javadoc")
    public static final String KEY_CARDINALITY = "CARDINALITY"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_MATERIALIZED = "MATERIALIZED"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_MATERIALIZED_TABLE = "MATERIALIZEDTABLE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_SUPPORTS_UPDATE = "SUPPORTSUPDATE"; //$NON-NLS-1$
    @SuppressWarnings("javadoc")
    public static final String KEY_SYSTEM = "SYSTEM"; //$NON-NLS-1$
    
    @SuppressWarnings("javadoc")
    public static final int DEFAULT_CARDINALITY = -1;
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_MATERIALIZED = false;
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_MATERIALIZED_TABLE = null; 
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_SUPPORTS_UPDATE = true; 
    @SuppressWarnings("javadoc")
    public static final boolean DEFAULT_SYSTEM = false;

    
    @SuppressWarnings("javadoc")
    public static final String DEFAULT_DATATYPE = "string"; //$NON-NLS-1$

    private int  cardinality = DEFAULT_CARDINALITY;
    private boolean materialized = DEFAULT_MATERIALIZED;
    private RelationalObject materializedTable;
    private boolean supportsUpdate = DEFAULT_SUPPORTS_UPDATE;
    private boolean system = DEFAULT_SYSTEM;
    private List<Column> columns;
    private PrimaryKey primaryKey;
    private UniqueConstraint uniqueConstraint;
    private List<UniqueConstraint> uniqueConstraints;
    private List<AccessPattern> accessPatterns;
    private List<ForeignKey> foreignKeys;
    private List<Index> indexes;
    private String nativeQuery;
    
    
    /**
     * RelationalTable constructor
     */
    public Table() {
        super();
        init();
    }
    
    /**
     * RelationalTable constructor
     * @param name the table name
     */
    public Table( String name ) {
        super(name);
        init();
    }
    
    private void init() {
        this.columns = new ArrayList<Column>();
        this.accessPatterns = new ArrayList<AccessPattern>();
        this.foreignKeys = new ArrayList<ForeignKey>();
        this.indexes = new ArrayList<Index>();
        this.uniqueConstraints = new ArrayList<UniqueConstraint>();
        getValidator().setNameValidator(new RelationalStringNameValidator(true));
    }
    
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.TABLE;
    }

    /**
     * @return cardinality
     */
    public int getCardinality() {
        return cardinality;
    }
    /**
     * @param cardinality Sets cardinality to the specified value.
     */
    public void setCardinality( int cardinality ) {
        if( this.cardinality != cardinality ) {
        	this.cardinality = cardinality;
        	handleInfoChanged();
        }
    }
    /**
     * @return materialized
     */
    public boolean isMaterialized() {
        return materialized;
    }
    /**
     * @param materialized Sets materialized to the specified value.
     */
    public void setMaterialized( boolean materialized ) {
        if( this.materialized != materialized ) {
        	this.materialized = materialized;
        	handleInfoChanged();
        }
    }
    /**
     * @return materializedTable
     */
    public RelationalObject getMaterializedTable() {
        return materializedTable;
    }
    /**
     * @param materializedTable Sets materializedTable to the specified value.
     */
    public void setMaterializedTable( RelationalObject materializedTable ) {
    	if( this.materializedTable != materializedTable ) {
    		this.materializedTable = materializedTable;
    		handleInfoChanged();
    	}
    }
    /**
     * @return supportsUpdate
     */
    public boolean getSupportsUpdate() {
        return supportsUpdate;
    }
    /**
     * @param supportsUpdate Sets supportsUpdate to the specified value.
     */
    public void setSupportsUpdate( boolean supportsUpdate ) {
    	if( this.supportsUpdate != supportsUpdate ) {
    		this.supportsUpdate = supportsUpdate;
    		handleInfoChanged();
    	}
    }
    /**
     * @return system
     */
    public boolean isSystem() {
        return system;
    }
    /**
     * @param system Sets system to the specified value.
     */
    public void setSystem( boolean system ) {
    	if( this.system != system ) {
	        this.system = system;
	        handleInfoChanged();
    	}
    }
    
    /**
     * @return nativeQuery may be null
     */
    public String getNativeQuery() {
        return nativeQuery;
    }
    /**
     * @param newQuery sets nativeQuery to the specified value. may be null
     */
    public void setNativeQuery( String newQuery ) {
    	if( StringUtils.areDifferent(this.nativeQuery, newQuery) ) {
    		this.nativeQuery = newQuery;
    		handleInfoChanged();
    	}
    }

    /**
     * Add a child to this model
     * @param child the child
     * @return 'true' if child was added
     */
    @Override
	public boolean addChild(RelationalObject child) {
    	int objectType = child.getType();
    	boolean success = false;
    	
		switch(objectType) {
		case TYPES.COLUMN:
			success = addColumn((Column)child);
			break;
		case TYPES.UC:
			success = addUniqueConstraint((UniqueConstraint)child);
			break;
		case TYPES.AP:
			success = addAccessPattern((AccessPattern)child);
			break;
		case TYPES.PK:
			success = setPrimaryKey((PrimaryKey)child);
			break;
		case TYPES.FK:
			success = addForeignKey((ForeignKey)child);
			break;
		case TYPES.INDEX:
			success = addIndex((Index)child);
		}
        
        return success;
    }
    
    /**
     * Remove specified child from the model
     * @param child the child to remove
     * @return 'true' if child was removed
     */
    @Override
	public boolean removeChild(RelationalObject child) {
    	int objectType = child.getType();
    	boolean success = false;
    	
		switch(objectType) {
		case TYPES.COLUMN:
			success = removeColumn((Column)child);
			break;
		case TYPES.UC:
			success = removeUniqueConstraint((UniqueConstraint)child);
			break;
		case TYPES.AP:
			success = removeAccessPattern((AccessPattern)child);
			break;
		case TYPES.PK:
			success = setPrimaryKey(null);
			break;
		case TYPES.FK:
			success = removeForeignKey((ForeignKey)child);
			break;
		case TYPES.INDEX:
			success = removeIndex((Index)child);
		}
        
        return success;
    }
    
    /**
     * Get the children for this Table
     * @return children
     */
    @Override
	public Collection<RelationalObject> getChildren() {
    	Collection<RelationalObject> children = new ArrayList<RelationalObject>();
    	
    	if(getColumns()!=null && !getColumns().isEmpty()) {
    		children.addAll(getColumns());
    	}
    	if(getPrimaryKey()!=null) {
    		children.add(getPrimaryKey());
    	}
    	if(getForeignKeys()!=null && !getForeignKeys().isEmpty()) {
    		children.addAll(getForeignKeys());
    	}
    	if(getIndexes()!=null && !getIndexes().isEmpty()) {
    		children.addAll(getIndexes());
    	}
    	if(getUniqueConstraints()!=null && !getUniqueConstraints().isEmpty()) {
    		children.addAll(getUniqueConstraints());
    	}
    	
        return children;
    }
    
    /**
     * Get the properties for this object
     * @return the properties
     */
    @Override
	public Map<String,String> getProperties() {
    	Map<String,String> props = super.getProperties();
    	
    	props.put(KEY_CARDINALITY, String.valueOf(getCardinality()));
    	props.put(KEY_MATERIALIZED, String.valueOf(isMaterialized()));
    	props.put(KEY_SUPPORTS_UPDATE, String.valueOf(getSupportsUpdate()));
    	props.put(KEY_SYSTEM, String.valueOf(isSystem()));
    	
    	return props;
    }
    
    /**
     * @return columns
     */
    public List<Column> getColumns() {
        return columns;
    }
    
    /**
     * Add a column to the table
     * @param column the column to add
     * @return 'true' if the add was successful
     */
    public boolean addColumn(Column column) {
    	boolean wasAdded = false;
    	if(!this.columns.contains(column)) {
        	wasAdded = this.columns.add(column);
    	}
    	if( wasAdded ) {
    		if(column.getParent()!=this) column.setParent(this);
    		handleInfoChanged();
    	} 
    	return wasAdded;
    }
    
    /**
     * Remove a column from the table
     * @param column the column to remove
     * @return 'true' if the remove was successful
     */
    public boolean removeColumn(Column column) {
    	boolean wasRemoved = this.columns.remove(column);
    	if( wasRemoved ) {
    		handleInfoChanged();
    	}
    	return wasRemoved;
    }

    /**
     * @return primaryKeys
     */
    public PrimaryKey getPrimaryKey() {
        return primaryKey;
    }

    /**
     * Set the tables PK
     * @param pk the pk
     * @return 'true' if successfully set
     */
    public boolean setPrimaryKey(PrimaryKey pk) {
    	boolean wasSet = false;
    	if( this.primaryKey != pk ) {
	        this.primaryKey = pk;
	    	pk.setParent(this);
	        wasSet = true;
	        handleInfoChanged();
    	}
    	return wasSet;
    }
    
    /**
     * @return uniqueContraints
     */
    public UniqueConstraint getUniqueContraint() {
        return uniqueConstraint;
    }
    
    /**
     * Set the unique constraint
     * @param uc the uc
     * @return 'true' if the UC was set
     */
    public boolean setUniqueConstraint(UniqueConstraint uc) {
    	boolean wasSet = false;
    	if( this.uniqueConstraint != uc ) {
	    	if( uc != null ) {
	    		uc.setParent(this);
	    	}
	        this.uniqueConstraint = uc;
	        wasSet = true;
	        handleInfoChanged();
    	}
    	return wasSet;
    }
    
    /**
     * @return uniqueContraints
     */
    public Collection<UniqueConstraint> getUniqueConstraints() {
        return uniqueConstraints;
    }
    
    /**
     * Add a unique constraint
     * @param constraint the constraint
     * @return 'true' if successfully added
     */
    public boolean addUniqueConstraint(UniqueConstraint constraint) {
    	boolean wasAdded = false;
    	if(!this.uniqueConstraints.contains(constraint)) {
        	wasAdded = this.uniqueConstraints.add(constraint);
    	}
    	if( wasAdded ) {
    		if(constraint.getParent()!=this) constraint.setParent(this);
    		handleInfoChanged();
    	} 
    	return wasAdded;
    }
    
    /**
     * Remove a unique constraint
     * @param constraint the constraint
     * @return 'true' if removed, 'false' if not
     */
    public boolean removeUniqueConstraint(UniqueConstraint constraint) {
    	boolean wasRemoved = this.uniqueConstraints.remove(constraint);
    	if( wasRemoved ) {
    		handleInfoChanged();
    	}
    	return wasRemoved;
    }

    /**
     * @return accessPatterns
     */
    public List<AccessPattern> getAccessPatterns() {
        return accessPatterns;
    }
    
    /**
     * Add an AccessPattern to the table
     * @param ap the AccessPattern
     * @return 'true' if the ap was added
     */
    public boolean addAccessPattern(AccessPattern ap) {
    	boolean wasAdded = false;
    	if(!this.accessPatterns.contains(ap)) {
        	wasAdded = this.accessPatterns.add(ap);
    	}
    	if( wasAdded ) {
    		if(ap.getParent()!=this) ap.setParent(this);
    		handleInfoChanged();
    	} 
    	return wasAdded;
    }
    
    /**
     * Remove an AccessPattern from the table
     * @param ap the AccessPattern
     * @return 'true' if removed, 'false' if not
     */
    public boolean removeAccessPattern(AccessPattern ap) {
    	boolean wasRemoved = this.accessPatterns.remove(ap);
    	if( wasRemoved ) {
    		handleInfoChanged();
    	}
    	return wasRemoved;
    }

    /**
     * @return foreignKeys
     */
    public List<ForeignKey> getForeignKeys() {
        return foreignKeys;
    }

    /**
     * Add FK to the table
     * @param fk the fk
     * @return 'true' if the FK was added
     */
    public boolean addForeignKey(ForeignKey fk) {
    	boolean wasAdded = false;
    	if(!this.foreignKeys.contains(fk)) {
        	wasAdded = this.foreignKeys.add(fk);
    	}
    	if( wasAdded ) {
    		if(fk.getParent()!=this) fk.setParent(this);
    		handleInfoChanged();
    	} 
    	return wasAdded;
    }
    
    /**
     * Remove FK from the table
     * @param fk the fk
     * @return 'true' if removed, 'false' if not.
     */
    public boolean removeForeignKey(ForeignKey fk) {
    	boolean wasRemoved = this.foreignKeys.remove(fk);
    	if( wasRemoved ) {
    		handleInfoChanged();
    	}
    	return wasRemoved;
    }
    
    /**
     * @return indexes
     */
    public List<Index> getIndexes() {
        return indexes;
    }

    /**
     * @param index the index
     * @return 'true' if successfully added
     */
    public boolean addIndex(Index index) {
    	boolean wasAdded = false;
    	if(!this.indexes.contains(index)) {
        	wasAdded = this.indexes.add(index);
    	}
    	if( wasAdded ) {
    		// NOTE: indexes are children of a schema so set parent to table's parent
    		if(index.getParent()!=this) index.setParent(this);
    		handleInfoChanged();
    	} 
    	return wasAdded;
    }
    
    /**
     * @param index the index
     * @return if index was removed
     */
    public boolean removeIndex(Index index) {
    	boolean wasRemoved = this.indexes.remove(index);
    	if( wasRemoved ) {
    		handleInfoChanged();
    	}
    	return wasRemoved;
    }
    
    /**
     * Set the object properties
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
            
            if(keyStr.equalsIgnoreCase(KEY_CARDINALITY) ) {
                setCardinality(Integer.parseInt(value));
            } else if(keyStr.equalsIgnoreCase(KEY_MATERIALIZED) ) {
                setMaterialized(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_SUPPORTS_UPDATE) ) {
                setSupportsUpdate(Boolean.parseBoolean(value));
            } else if(keyStr.equalsIgnoreCase(KEY_SYSTEM) ) {
                setSystem(Boolean.parseBoolean(value));
            } 
        }
    	
        handleInfoChanged();
    }
    
	/**
	 * Create a new column in the table.  The column is added at the end of the current columns list
	 * @return the created Column
	 */
	public Column createColumn() {
    	return createColumn(DEFAULT_DATATYPE, Column.DEFAULT_STRING_LENGTH);
    }
    
    /**
     * Create a new column of the specified datatype and length
     * @param datatype the data type
     * @param length the length
     * @return the created Column
     */
    public Column createColumn(String datatype, int length) {
    	return createColumn("newColumn_" + (getColumns().size() + 1), datatype, length); //$NON-NLS-1$
    }
    
    /**
     * Create a new column with the specified name, data type and length
     * @param name the name
     * @param datatype the data type
     * @param length the length
     * @return the created Column
     */
    public Column createColumn(String name, String datatype, int length) {
    	Column newColumn = new Column(name);
    	DataType dType = new DataType(datatype,length);
    	newColumn.setDatatype(dType);
    	addColumn(newColumn);
    	return newColumn;
    }
    
	/**
	 * Determine if the column can be moved up from the current position
	 * @param column the column
	 * @return 'true' if move up is possible
	 */
	public boolean canMoveColumnUp(Column column) {
		return getColumnIndex(column) > 0;
	}
	
	/**
	 * Determine if the column can be moved down from the current position
	 * @param column the column
	 * @return 'true' if move down is possible
	 */
	public boolean canMoveColumnDown(Column column) {
		return getColumnIndex(column) < getColumns().size()-1;
	}
	
	private int getColumnIndex(Column column) {
		int i=0;
		for( Column existingColumn : getColumns() ) {
			if( existingColumn == column) {
				return i;
			}
			i++;
		}
		
		// Shouldn't ever get here!
		return -1;
	}
	
	/**
	 * Move the specified column up one position
	 * @param theColumn the column
	 */
	public void moveColumnUp(Column theColumn) {
		int startIndex = getColumnIndex(theColumn);
		if( startIndex > 0 ) {
			// Make Copy of List & get columnInfo of startIndex-1
			Column[] existingColumns = getColumns().toArray(new Column[0]);
			Column priorColumn = existingColumns[startIndex-1];
			existingColumns[startIndex-1] = theColumn;
			existingColumns[startIndex] = priorColumn;
			
			List<Column> newColumns = new ArrayList<Column>(existingColumns.length);
			for( Column info : existingColumns) {
				newColumns.add(info);
			}
			
			this.columns = newColumns;
		}
	}
	
	/**
	 * Move the specified column down one position
	 * @param theColumn the column
	 */
	public void moveColumnDown(Column theColumn) {
		int startIndex = getColumnIndex(theColumn);
		if( startIndex < (getColumns().size()-1) ) {
			// Make Copy of List & get columnInfo of startIndex+1
			Column[] existingColumns = getColumns().toArray(new Column[0]);
			Column afterColumn = existingColumns[startIndex+1];
			existingColumns[startIndex+1] = theColumn;
			existingColumns[startIndex] = afterColumn;
			
			List<Column> newColumns = new ArrayList<Column>(existingColumns.length);
			for( Column info : existingColumns) {
				newColumns.add(info);
			}
			
			this.columns = newColumns;
		}
	}
	
	@Override
	public void handleInfoChanged() {
		super.handleInfoChanged();
		
		// Set extension properties here??
		
		if( this.nativeQuery != null ) {
			getExtensionProperties().put(NATIVE_QUERY, this.nativeQuery );
		} else getExtensionProperties().remove(NATIVE_QUERY);
			
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
        final Table other = (Table)object;

        // string properties
        if (!StringUtils.valuesAreEqual(getNativeQuery(), other.getNativeQuery())) {
            return false;
        }
        
        if( !(getCardinality()==other.getCardinality()) ||  
            !(getSupportsUpdate()==other.getSupportsUpdate()) ||
            !(isMaterialized()==other.isMaterialized()) ||
        	!(isSystem()==other.isSystem())) {
        	return false;
        }
 
        if (materializedTable == null) {
            if (other.materializedTable != null)
                return false;   
        } else if (!materializedTable.equals(other.materializedTable))
            return false;

        if (uniqueConstraint == null) {
            if (other.uniqueConstraint != null)
                return false;   
        } else if (!uniqueConstraint.equals(other.uniqueConstraint))
            return false;

        if (primaryKey == null) {
            if (other.primaryKey != null)
                return false;   
        } else if (!primaryKey.equals(other.primaryKey))
            return false;

        // Columns
        List<Column> thisColumns = getColumns();
        List<Column> thatColumns = other.getColumns();

        if (thisColumns.size() != thatColumns.size()) {
            return false;
        }
        
        if (!thisColumns.isEmpty() && !thisColumns.equals(thatColumns)) {
            return false;
        }
        
        // ForeignKeys
        List<ForeignKey> thisFKs = getForeignKeys();
        List<ForeignKey> thatFKs = other.getForeignKeys();

        if (thisFKs.size() != thatFKs.size()) {
            return false;
        }
        
        if (thisFKs.size()==1) {
        	if(!thisFKs.get(0).equals(thatFKs.get(0))) {
        		return false;
        	}
        } else if(thisFKs.size()>1){
            ReferenceComparator comparator = new ReferenceComparator();
            List<ForeignKey> sortedThisFKs = new ArrayList<ForeignKey>(getForeignKeys());
            List<ForeignKey> sortedThatFKs = new ArrayList<ForeignKey>(other.getForeignKeys());
            Collections.sort(sortedThisFKs,comparator);
            Collections.sort(sortedThatFKs,comparator);
            
            if (!sortedThisFKs.equals(sortedThatFKs)) {
                return false;
            }
        }
        
        // Indexes
        List<Index> thisIndexes = getIndexes();
        List<Index> thatIndexes = other.getIndexes();

        if (thisIndexes.size() != thatIndexes.size()) {
            return false;
        }
        
        if (!thisIndexes.isEmpty() && !thisIndexes.equals(thatIndexes)) {
            return false;
        }

        // AccessPatterns
        List<AccessPattern> thisAPs = getAccessPatterns();
        List<AccessPattern> thatAPs = other.getAccessPatterns();

        if (thisAPs.size() != thatAPs.size()) {
            return false;
        }
        
        if (!thisAPs.isEmpty() && !thisAPs.equals(thatAPs)) {
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
        if (!StringUtils.isEmpty(getNativeQuery())) {
            result = HashCodeUtils.hashCode(result, getNativeQuery());
        }
        
        result = HashCodeUtils.hashCode(result, getCardinality());
        result = HashCodeUtils.hashCode(result, getSupportsUpdate());
        result = HashCodeUtils.hashCode(result, isMaterialized());
        result = HashCodeUtils.hashCode(result, isSystem());
        
        if(materializedTable!=null) {
            result = HashCodeUtils.hashCode(result, materializedTable);
        }
        if(uniqueConstraint!=null) {
            result = HashCodeUtils.hashCode(result, uniqueConstraint);
        }
        if(primaryKey!=null) {
            result = HashCodeUtils.hashCode(result, primaryKey);
        }

        List<Column> cols = getColumns();
        for(Column col: cols) {
            result = HashCodeUtils.hashCode(result, col);
        }
        
        List<ForeignKey> fks = getForeignKeys();
        for(ForeignKey fk: fks) {
            result = HashCodeUtils.hashCode(result, fk);
        }

        List<Index> indexes = getIndexes();
        for(Index index: indexes) {
            result = HashCodeUtils.hashCode(result, index);
        }

        List<AccessPattern> aps = getAccessPatterns();
        for(AccessPattern ap: aps) {
            result = HashCodeUtils.hashCode(result, ap);
        }
        
        return result;
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
		if( primaryKey != null ) {
			sb.append("\n\t").append("PK = ").append(primaryKey); //$NON-NLS-1$  //$NON-NLS-2$
		}
		if( uniqueConstraint != null ) {
			sb.append("\n\t").append("UC = ").append(uniqueConstraint); //$NON-NLS-1$  //$NON-NLS-2$
		}
		if( !getAccessPatterns().isEmpty() ) {
			sb.append("\n\t").append(getAccessPatterns().size()).append(" access patterns"); //$NON-NLS-1$  //$NON-NLS-2$
			for( AccessPattern ap : getAccessPatterns() ) {
				sb.append("\n\tap = ").append(ap); //$NON-NLS-1$
			}
		}
		return sb.toString();
	}

}
