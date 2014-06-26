/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.relational.model;




/**
 * 
 *
 *
 */
public class View extends Table {

	/**
	 * RelationalView constructor
	 */
    public View() {
        super();
    }
    
    /**
     * RelationalView constructor
     * @param name the name
     */
    public View( String name ) {
        super(name);
    }
    
    /**
     * @return type
     */
    @Override
	public int getType() {
        return TYPES.VIEW;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#addForeignKey(org.komodo.relational.model.ForeignKey)
     */
    @Override
    public void addForeignKey( ForeignKey fk ) {
        throw new UnsupportedOperationException("addForeignKey() not supported for Relational Views"); //$NON-NLS-1$
    }
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setPrimaryKey(org.komodo.relational.model.PrimaryKey)
     */
    @Override
    public void setPrimaryKey( PrimaryKey pk ) {
        throw new UnsupportedOperationException("addPrimaryKey() not supported for Relational Views"); //$NON-NLS-1$
    }
    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.model.Table#setUniqueConstraint(org.komodo.relational.model.UniqueConstraint)
     */
    @Override
    public void setUniqueConstraint( UniqueConstraint uc ) {
        throw new UnsupportedOperationException("addUniqueConstraint() not supported for Relational Views"); //$NON-NLS-1$
    }
    
}
