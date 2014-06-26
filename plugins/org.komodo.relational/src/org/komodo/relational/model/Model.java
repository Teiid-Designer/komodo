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
import org.komodo.utils.ArgCheck;

/**
 * 
 *
 * @since 8.0
 */
public class Model extends RelationalObject {
	private Collection<RelationalObject> allRefs = new ArrayList<RelationalObject>();
    private Collection<RelationalObject> children;
    
    /**
     * RelationalModel constructor
     * @param name the model name
     */
    public Model( String name ) {
        super(name);
        this.children = new ArrayList<RelationalObject>();
    }
       
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.MODEL;
    }

    /**
     * Get all reference objects for this model
     * @return all reference objects
     */
    public Collection<RelationalObject> getAllReferences() {
    	this.allRefs.clear();
    	if(this.children!=null) {
        	for(RelationalObject ref:this.children) {
        		addRecursive(ref,this.allRefs);
        	}
    	}
    	return this.allRefs;
    }
    
    /**
     * Get the top level children for this model
     * @return model children
     */
    public Collection<RelationalObject> getChildren() {
        return this.children;
    }
    
    /**
     * Add a child to this model
     * @param child the child
     * @return 'true' if child was added
     */
    public boolean addChild(RelationalObject child) {
        if( this.children == null ) {
            this.children = new ArrayList<RelationalObject>();
        }

        boolean wasAdded = false;
        if( !this.children.contains(child) ) {
        	child.setParent(this);
            wasAdded = this.children.add(child);
        }
        
        return wasAdded;
    }
    
    /**
     * Remove specified child from the model
     * @param child the child to remove
     * @return 'true' if child was removed
     */
    public boolean removeChild(RelationalObject child) {
        if( this.children == null ) {
            return false;
        }
        
        if( this.children.contains(child) ) {
            return this.children.remove(child);
        }
        
        return false;
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
