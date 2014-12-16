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
import java.util.Properties;

import org.komodo.utils.HashCodeUtils;



/**
 * 
 *
 *
 */
public class PrimaryKey extends RelationalObject {

    private Collection<Column> columns;
    
    /**
     * RelationalPrimaryKey constructor
     */
    public PrimaryKey( ) {
        super();
        this.columns = new ArrayList<Column>();
    }
    
    /**
     * RelationalPrimaryKey constructor
     * @param name the primary key name
     */
    public PrimaryKey( String name ) {
        super(name);
        this.columns = new ArrayList<Column>();
    }
    
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.PK;
    }

    /**
     * @return columns
     */
    public Collection<Column> getColumns() {
        return columns;
    }
    /**
     * @param columns Sets columns to the specified value.
     */
    public void setColumns( Collection<Column> columns ) {
        this.columns = columns;
        handleInfoChanged();
    }
    
    /**
     * Add a column to this PK
     * @param column the column
     */
    public void addColumn(Column column) {
    	if( this.columns.add(column) ) {
    		//column.setParent(this);
    		handleInfoChanged();
    	} 
    }
    
    /**
     * Remove a column
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
     * The the parent table
     * @return the table
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
        final PrimaryKey other = (PrimaryKey)object;

        // Columns
        Collection<Column> thisColumns = getColumns();
        Collection<Column> thatColumns = other.getColumns();

        if (thisColumns.size() != thatColumns.size()) {
            return false;
        }
        
        if (!thisColumns.isEmpty() && !thisColumns.equals(thatColumns)) {
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

        Collection<Column> cols = getColumns();
        for(Column col: cols) {
            result = HashCodeUtils.hashCode(result, col);
        }
                
        return result;
    }    

}
