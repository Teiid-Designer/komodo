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

import org.komodo.utils.ArgCheck;

/**
 * 
 *
 *
 */
public class Model extends RelationalObject {
	private Collection<RelationalObject> allObjects = new ArrayList<RelationalObject>();
    private Collection<RelationalObject> children = new ArrayList<RelationalObject>();
    
    /**
     * RelationalModel constructor
     */
    public Model( ) {
        super();
    }
    
    /**
     * RelationalModel constructor
     * @param name the model name
     */
    public Model( String name ) {
        super(name);
    }
       
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.MODEL;
    }

    /**
     * Get all objects for this model
     * @return all objects
     */
    public Collection<RelationalObject> getAllObjects() {
    	this.allObjects.clear();
    	if(this.children!=null) {
        	for(RelationalObject ref:this.children) {
        		addRecursive(ref,this.allObjects);
        	}
    	}
    	return this.allObjects;
    }
    
    /**
     * Get the top level children for this model
     * @return model children
     */
    @Override
	public Collection<RelationalObject> getChildren() {
        return this.children;
    }
    
    /**
     * Add a child to this model
     * @param child the child
     * @return 'true' if child was added
     */
    @Override
	public boolean addChild(RelationalObject child) {
    	boolean wasAdded = false;
    	if(!this.children.contains(child)) {
        	wasAdded = this.children.add(child);
    	}
        if( wasAdded ) {
    		if(child.getParent()!=this) child.setParent(this);
        }
        return wasAdded;
    }
    
    /**
     * Remove specified child from the model
     * @param child the child to remove
     * @return 'true' if child was removed
     */
    @Override
	public boolean removeChild(RelationalObject child) {
        boolean wasRemoved = this.children.remove(child);
        
        return wasRemoved;
    }
    
    /**
     * Determine if the model has a child with the specified name
     * @param name the child name
     * @return 'true' if model contains child with matching name
     */
    public boolean hasChild(String name) {
        ArgCheck.isNotNull(name, "name"); //$NON-NLS-1$
        for( RelationalObject child : this.children ) {
            if( name.equalsIgnoreCase( child.getName())) {
                return true;
            }
        }
        
        return false;
    }
    
    /**
     * Get the model child with the specified name
     * @param name the child name
     * @return the child, null if no matching child
     */
    public RelationalObject getChildWithName(String name) {
        ArgCheck.isNotNull(name, "name"); //$NON-NLS-1$
        for( RelationalObject child : this.children ) {
            if( name.equalsIgnoreCase( child.getName())) {
                return child;
            }
        }
        
        return null;
    }
    
    /**
     * Recursively adds all references
     * @param ref
     * @param allRefs
     */
    private void addRecursive(RelationalObject ref, Collection<RelationalObject> allRefs) {
    	if(ref instanceof Table) {
    		allRefs.add(ref);
    		Collection<AccessPattern> accessPatterns = ((Table)ref).getAccessPatterns();
    		for(RelationalObject ap: accessPatterns) {
    			addRecursive(ap,allRefs);
    		}
    		
    		Collection<Column> columns  = ((Table)ref).getColumns();
    		for(RelationalObject col: columns) {
    			addRecursive(col,allRefs);
    		}
    		
    		Collection<ForeignKey> fks = ((Table)ref).getForeignKeys();
    		for(RelationalObject fk: fks) {
    			addRecursive(fk,allRefs);
    		}
    		
    		PrimaryKey  pk = ((Table)ref).getPrimaryKey();
    		addRecursive(pk,allRefs);
    		
    		Collection<Index> indexes = ((Table)ref).getIndexes();
    		for(RelationalObject index: indexes) {
    			addRecursive(index,allRefs);
    		}

    		Collection<UniqueConstraint> ucs = ((Table)ref).getUniqueConstraints();
    		if(ucs!=null) {
    			for(RelationalObject uc: ucs) {
    				addRecursive(uc,allRefs);
    			}
    		}
    	} else if(ref instanceof Procedure) {
    		allRefs.add(ref);
    		Collection<Parameter> procParams = ((Procedure)ref).getParameters();
    		for(RelationalObject param: procParams) {
    			addRecursive(param,allRefs);
    		}
    		ProcedureResultSet resultSet = ((Procedure)ref).getResultSet();
    		addRecursive(resultSet,allRefs);
    		
    	} else if(ref instanceof View) {
    		allRefs.add(ref);
    		Collection<AccessPattern> accessPatterns = ((View)ref).getAccessPatterns();
    		for(RelationalObject ap: accessPatterns) {
    			addRecursive(ap,allRefs);
    		}
    		Collection<Column> columns = ((View)ref).getColumns();
    		for(RelationalObject col: columns) {
    			addRecursive(col,allRefs);
    		}
    		
    	} else {
    		allRefs.add(ref);
    	}
    }
        
}
