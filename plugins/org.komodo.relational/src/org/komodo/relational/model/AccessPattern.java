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

import org.komodo.core.HashCodeUtil;
import org.komodo.relational.core.RelationalStringNameValidator;



/**
 * RelationalAccessPattern
 *
 * @since 8.0
 */
public class AccessPattern extends RelationalObject {
    private List<Column> columns;
    
    /**
     * constructor
     */
    public AccessPattern() {
        super();
        this.columns = new ArrayList<Column>();
        setNameValidator(new RelationalStringNameValidator(false));
    }
    
    /**
     * constructor
     * @param name the name for the access pattern
     */
    public AccessPattern( String name ) {
        super(name);
        this.columns = new ArrayList<Column>();
        setNameValidator(new RelationalStringNameValidator(false));
    }

    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.AP;
    }
    
    /**
     * @return columns
     */
    public List<Column> getColumns() {
        return columns;
    }
    
    /**
     * Add a column
     * @param column the relational column to add
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
     * Get the table
     * @return the relational table
     */
    public Table getTable() {
    	if( getParent() != null ) {
    		return (Table)getParent();
    	}
    	
    	return null;
    }
    
    /**
     * Set properties
     * @param props the properties
     */
    @Override
	public void setProperties(Properties props) {
    	// Set common properties
    	super.setProperties(props);
    	
        handleInfoChanged();
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
        final AccessPattern other = (AccessPattern)object;

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

        List<Column> cols = getColumns();
        for(Column col: cols) {
            result = HashCodeUtil.hashCode(result, col);
        }
        
        return result;
    }    

}
