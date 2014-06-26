/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;

import java.util.Properties;


/**
 * 
 *
 */
public class ProcedureResultSet extends Table {

    
    /**
     * RelationalProcedureResultSet constructor
     */
    public ProcedureResultSet() {
        super();
    }
    
    /**
     * RelationalProcedureResultSet constructor
     * @param name the resultset name
     */
    public ProcedureResultSet( String name ) {
        super(name);
    }

    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.RESULT_SET;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addAccessPattern(org.komodo.relational.model.AccessPattern)
     */
    @Override
    public void addAccessPattern( AccessPattern ap ) {
        throw new UnsupportedOperationException("addAccessPattern() not supported for Procedure Result Sets"); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addForeignKey(org.komodo.relational.model.ForeignKey)
     */
    @Override
    public void addForeignKey( ForeignKey fk ) {
        throw new UnsupportedOperationException("addForeignKey() not supported for Procedure Result Sets"); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setPrimaryKey(org.komodo.relational.model.PrimaryKey)
     */
    @Override
    public void setPrimaryKey( PrimaryKey pk ) {
        throw new UnsupportedOperationException("addPrimaryKey() not supported for Procedure Result Sets"); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setUniqueConstraint(org.komodo.relational.model.UniqueConstraint)
     */
    @Override
    public void setUniqueConstraint( UniqueConstraint uc ) {
        throw new UnsupportedOperationException("addUniqueConstraint() not supported for Procedure Result Sets"); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setCardinality(int)
     */
    @Override
    public void setCardinality( int cardinality ) {
        throw new UnsupportedOperationException("setCardinality() not supported for Procedure Result Sets"); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setMaterialized(boolean)
     */
    @Override
    public void setMaterialized( boolean materialized ) {
        throw new UnsupportedOperationException("setMaterialized() not supported for Procedure Result Sets"); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setMaterializedTable(org.komodo.relational.model.RelationalObject)
     */
    @Override
    public void setMaterializedTable( RelationalObject materializedTable ) {
        throw new UnsupportedOperationException("setMaterializedTable() not supported for Procedure Result Sets"); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setSupportsUpdate(boolean)
     */
    @Override
    public void setSupportsUpdate( boolean supportsUpdate ) {
        throw new UnsupportedOperationException("setSupportsUpdate() not supported for Procedure Result Sets"); //$NON-NLS-1$
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setSystem(boolean)
     */
    @Override
    public void setSystem( boolean system ) {
        throw new UnsupportedOperationException("setSystem() not supported for Procedure Result Sets"); //$NON-NLS-1$
    }

    @Override
	public void setProperties(Properties props) {
    	// Set common properties
    	super.setProperties(props);
    	
        handleInfoChanged();
    }
        
}
