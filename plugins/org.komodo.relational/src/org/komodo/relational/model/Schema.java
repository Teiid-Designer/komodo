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
import java.util.List;
import org.komodo.relational.core.RelationalStringNameValidator;
import org.komodo.utils.HashCodeUtil;



/**
 * Relational Schema
 *
 *
 */
public class Schema extends RelationalObject {

	private List<Index> indexes;
	private List<Procedure> procedures;
	private List<Table> tables;
    
	/**
	 * RelationalSchema constructor
	 */
    public Schema() {
        super();
        this.indexes = new ArrayList<Index>();
        this.procedures = new ArrayList<Procedure>();
        this.tables = new ArrayList<Table>();
        setNameValidator(new RelationalStringNameValidator(false));
    }
    
    /**
     * RelationalSchema constructor
     * @param name the schema name
     */
    public Schema( String name ) {
        super(name);
        this.indexes = new ArrayList<Index>();
        this.procedures = new ArrayList<Procedure>();
        this.tables = new ArrayList<Table>();
        setNameValidator(new RelationalStringNameValidator(false));
    }
    
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.SCHEMA;
    }

    /**
     * @return indexes
     */
    public List<Index> getIndexes() {
        return this.indexes;
    }
    
    /**
     * @param index the new index
     */
    public void addIndex(Index index) {
        if( this.indexes.add(index) ) {
        	index.setParent(this);
    		handleInfoChanged();
        }
    }
    
    /**
     * @param index the index to remove
     * @return if index was removed or not
     */
    public boolean removeIndex(Index index) {
    	if( this.indexes.remove(index) ) {
    		handleInfoChanged();
    		return true;
    	}
    	return false;
    }
    
    /**
     * @return procedures
     */
    public List<Procedure> getProcedures() {
        return this.procedures;
    }
    
    /**
     * @param procedure the new procedure
     */
    public void addProcedure(Procedure procedure) {
        if( this.procedures.add(procedure) ) {
        	procedure.setParent(this);
    		handleInfoChanged();
        }
    }
    
    /**
     * @param procedure the procedure to remove
     * @return if procedure was removed or not
     */
    public boolean removeProcedure(Procedure procedure) {
    	if( this.procedures.remove(procedure) ) {
    		handleInfoChanged();
    		return true;
    	}
    	return false;
    }
    
    /**
     * @return tables
     */
    public List<Table> getTables() {
        return this.tables;
    }
    
    /**
     * @param table the new table
     */
    public void addTable(Table table) {
        if( this.tables.add(table) ) {
        	table.setParent(this);
    		handleInfoChanged();
        }
    }
    
    /**
     * @param table the table to remove
     * @return if table was removed or not
     */
    public boolean removeTable(Table table) {
    	if( this.tables.remove(table) ) {
    		handleInfoChanged();
    		return true;
    	}
    	return false;
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
		if (!super.equals(object)) {
			return false;
		}
        if (this == object)
            return true;
        if (object == null)
            return false;
        if (getClass() != object.getClass())
            return false;
        final Schema other = (Schema)object;

        // Tables
        Collection<Table> thisTables = getTables();
        Collection<Table> thatTables = other.getTables();

        if (thisTables.size() != thatTables.size()) {
            return false;
        }
        
        if (!thisTables.isEmpty() && !thisTables.containsAll(thatTables)) {
            return false;
        }

        // Procedures
        Collection<Procedure> thisProcedures = getProcedures();
        Collection<Procedure> thatProcedures = other.getProcedures();

        if (thisProcedures.size() != thatProcedures.size()) {
            return false;
        }
        
        if (!thisProcedures.isEmpty() && !thisProcedures.containsAll(thatProcedures)) {
            return false;
        }

        // Indexes
        Collection<Index> thisIndexes = getIndexes();
        Collection<Index> thatIndexes = other.getIndexes();

        if (thisIndexes.size() != thatIndexes.size()) {
            return false;
        }
        
        if (!thisIndexes.isEmpty() && !thisIndexes.containsAll(thatIndexes)) {
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

        List<Table> tables = getTables();
        for(Table table: tables) {
            result = HashCodeUtil.hashCode(result, table);
        }
        List<Procedure> procs = getProcedures();
        for(Procedure proc: procs) {
            result = HashCodeUtil.hashCode(result, proc);
        }
        List<Index> indexes = getIndexes();
        for(Index index: indexes) {
            result = HashCodeUtil.hashCode(result, index);
        }

        return result;
    }    
	
}
