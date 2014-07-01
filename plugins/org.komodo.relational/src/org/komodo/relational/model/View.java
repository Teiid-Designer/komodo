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
