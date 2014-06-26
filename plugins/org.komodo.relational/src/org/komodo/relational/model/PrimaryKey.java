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
import org.komodo.relational.Messages;
import org.komodo.relational.Messages.RELATIONAL;
import org.komodo.relational.core.RelationalStringNameValidator;
import org.komodo.spi.outcome.IOutcome;
import org.komodo.spi.outcome.OutcomeFactory;
import org.komodo.utils.HashCodeUtil;



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
        setNameValidator(new RelationalStringNameValidator(false));
    }
    
    /**
     * RelationalPrimaryKey constructor
     * @param name the primary key name
     */
    public PrimaryKey( String name ) {
        super(name);
        this.columns = new ArrayList<Column>();
        setNameValidator(new RelationalStringNameValidator(false));
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
    
	@Override
	public IOutcome validate() {
		// Walk through the properties for the table and set the status
		this.currentOutcome = super.validate();
		
		if( !this.getOutcome().isOK() ) {
			return this.currentOutcome;
		}
		
		if( this.getColumns().isEmpty() ) {
			this.currentOutcome = OutcomeFactory.getInstance().createError(
					Messages.getString(RELATIONAL.validate_error_pkNoColumnsDefined, getName()) );
			return this.currentOutcome;
		}
		return this.currentOutcome;
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
            result = HashCodeUtil.hashCode(result, col);
        }
                
        return result;
    }    

}
