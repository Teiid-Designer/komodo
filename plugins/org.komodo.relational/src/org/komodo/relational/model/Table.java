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
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.RELATIONAL;
import org.komodo.relational.core.RelationalStringNameValidator;
import org.komodo.spi.outcome.IOutcome;
import org.komodo.spi.outcome.IOutcome.Level;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.utils.HashCodeUtil;
import org.komodo.utils.StringUtil;
import org.komodo.utils.StringUtilities;


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
        setNameValidator(new RelationalStringNameValidator(true));
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
    	if( StringUtilities.areDifferent(this.nativeQuery, newQuery) ) {
    		this.nativeQuery = newQuery;
    		handleInfoChanged();
    	}
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
     */
    public void addColumn(Column column) {
    	if( this.columns.add(column) ) {
    		column.setParent(this);
    		handleInfoChanged();
    	} 
    }
    
    /**
     * Remove a column from the table
     * @param column the column to remove
     * @return 'true' if the move was successful
     */
    public boolean removeColumn(Column column) {
    	if( this.columns.remove(column) ) {
    		handleInfoChanged();
    		return true;
    	}
    	return false;
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
     */
    public void setPrimaryKey(PrimaryKey pk) {
    	if( this.primaryKey != pk ) {
	    	if( pk != null ) {
	    		pk.setParent(this);
	    	}
	        this.primaryKey = pk;
	        handleInfoChanged();
    	}
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
     */
    public void setUniqueConstraint(UniqueConstraint uc) {
    	if( this.uniqueConstraint != uc ) {
	    	if( uc != null ) {
	    		uc.setParent(this);
	    	}
	        this.uniqueConstraint = uc;
	        handleInfoChanged();
    	}
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
     */
    public void addUniqueConstraint(UniqueConstraint constraint) {
    	if( this.uniqueConstraints.add(constraint) ) {
    		constraint.setParent(this);
    		handleInfoChanged();
    	}
    }
    
    /**
     * Remove a unique constraint
     * @param constraint the constraint
     * @return 'true' if removed, 'false' if not
     */
    public boolean removeUniqueConstraint(UniqueConstraint constraint) {
    	if( this.uniqueConstraints.remove(constraint) ) {
    		handleInfoChanged();
    		return true;
    	}
    	return false;
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
     */
    public void addAccessPattern(AccessPattern ap) {
    	if( this.accessPatterns.add(ap) ) {
    		ap.setParent(this);
    		handleInfoChanged();
    	}
    }
    
    /**
     * Remove an AccessPattern from the table
     * @param ap the AccessPattern
     * @return 'true' if removed, 'false' if not
     */
    public boolean removeAccessPattern(AccessPattern ap) {
    	if( this.accessPatterns.remove(ap) ) {
    		handleInfoChanged();
    		return true;
    	}
    	return false;
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
     */
    public void addForeignKey(ForeignKey fk) {
    	if( this.foreignKeys.add(fk) ) {
    		fk.setParent(this);
    		handleInfoChanged();
    	}
    }
    
    /**
     * Remove FK from the table
     * @param fk the fk
     * @return 'true' if removed, 'false' if not.
     */
    public boolean removeForeignKey(ForeignKey fk) {
    	if( this.foreignKeys.remove(fk) ) {
    		handleInfoChanged();
    		return true;
    	}
    	return false;
    }
    
    /**
     * @return indexes
     */
    public List<Index> getIndexes() {
        return indexes;
    }

    /**
     * @param index the index
     */
    public void addIndex(Index index) {
    	if( this.indexes.add(index) ) {
    		// NOTE: indexes are children of a schema so set parent to table's parent
    		index.setParent(this.getParent());
    		handleInfoChanged();
    	}
    }
    
    /**
     * @param index the index
     * @return if index was removed
     */
    public boolean removeIndex(Index index) {
    	if( this.indexes.remove(index) ) {
    		handleInfoChanged();
    		return true;
    	}
    	return false;
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
    	newColumn.setDatatype(datatype);
    	newColumn.setLength(length);
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
	
	@Override
	public IOutcome validate() {
		// Walk through the properties for the table and set the status
		this.currentOutcome = super.validate();
		
		if( this.currentOutcome.getLevel() == Level.ERROR ) {
			return this.currentOutcome;
		}
		
		if( this.isMaterialized() && this.materializedTable == null ) {
			this.currentOutcome = OutcomeFactory.getInstance().createWarning(
					Messages.getString(RELATIONAL.validate_error_materializedTableHasNoTableDefined) );
			return this.currentOutcome;
		}
		
		if( this.getPrimaryKey() != null && !this.getPrimaryKey().getOutcome().isOK()) {
			this.currentOutcome = this.getPrimaryKey().getOutcome();
			return this.currentOutcome;
		}
		
		if( this.getUniqueContraint() != null && !this.getUniqueContraint().getOutcome().isOK()) {
			this.currentOutcome = this.getUniqueContraint().getOutcome();
			return this.currentOutcome;
		}
		
		for( ForeignKey fk : this.getForeignKeys() ) {
			if( !fk.getOutcome().isOK()) {
				this.currentOutcome = fk.getOutcome();
				return this.currentOutcome;
			}
		}
		
		// Check Column Status values
		for( Column col : getColumns() ) {
			if( col.getOutcome().getLevel() == Level.ERROR ) {
				this.currentOutcome = OutcomeFactory.getInstance().createError(col.getOutcome().getMessage() );
				return this.currentOutcome;
			}
		}
		
		// Check Column Status values
		for( Column outerColumn : getColumns() ) {
			for( Column innerColumn : getColumns() ) {
				if( outerColumn != innerColumn ) {
					if( outerColumn.getName().equalsIgnoreCase(innerColumn.getName())) {
						this.currentOutcome = OutcomeFactory.getInstance().createError(
								Messages.getString(RELATIONAL.validate_error_duplicateColumnNamesInTable, getName()) );
						return this.currentOutcome;
					}
				}
			}
		}
		
		if( this.getColumns().isEmpty() ) {
			if( this.getParent() != null && this.getParent() instanceof Procedure ) {
				this.currentOutcome = OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefinedForResultSet) ); 
				return this.currentOutcome;
			} else {
				this.currentOutcome = OutcomeFactory.getInstance().createWarning(
						Messages.getString(RELATIONAL.validate_warning_noColumnsDefined) ); 
				return this.currentOutcome;
			}
		}
		
		return this.currentOutcome;
		
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
        if (!StringUtil.valuesAreEqual(getNativeQuery(), other.getNativeQuery())) {
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
        if (!StringUtil.isEmpty(getNativeQuery())) {
            result = HashCodeUtil.hashCode(result, getNativeQuery());
        }
        
        result = HashCodeUtil.hashCode(result, getCardinality());
        result = HashCodeUtil.hashCode(result, getSupportsUpdate());
        result = HashCodeUtil.hashCode(result, isMaterialized());
        result = HashCodeUtil.hashCode(result, isSystem());
        
        if(materializedTable!=null) {
            result = HashCodeUtil.hashCode(result, materializedTable);
        }
        if(uniqueConstraint!=null) {
            result = HashCodeUtil.hashCode(result, uniqueConstraint);
        }
        if(primaryKey!=null) {
            result = HashCodeUtil.hashCode(result, primaryKey);
        }

        List<Column> cols = getColumns();
        for(Column col: cols) {
            result = HashCodeUtil.hashCode(result, col);
        }
        
        List<ForeignKey> fks = getForeignKeys();
        for(ForeignKey fk: fks) {
            result = HashCodeUtil.hashCode(result, fk);
        }

        List<Index> indexes = getIndexes();
        for(Index index: indexes) {
            result = HashCodeUtil.hashCode(result, index);
        }

        List<AccessPattern> aps = getAccessPatterns();
        for(AccessPattern ap: aps) {
            result = HashCodeUtil.hashCode(result, ap);
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
