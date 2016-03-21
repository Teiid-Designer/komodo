/*************************************************************************************
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
 ************************************************************************************/
package org.komodo.spi.runtime;

import java.util.Properties;

/**
 *
 */
public class FailedTeiidDataSource implements TeiidDataSource {

	String modelName;
	String jndiName;
	int reasonCode;
	
	/**
	 * 
	 */
	public FailedTeiidDataSource(String modelName, String jndiName, int reasonCode) {
		this.modelName = modelName;
		this.jndiName = jndiName;
		this.reasonCode = reasonCode;
	}
	
	/**
	 * @return the modelName
	 */
	public String getModelName() {
		return this.modelName;
	}

	/**
	 * @return the jndiName
	 */
	public String getJndiName() {
		return this.jndiName;
	}

	/**
	 * @return the reasonCode
	 */
	public int getReasonCode() {
		return this.reasonCode;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getDisplayName()
	 */
	@Override
	public String getDisplayName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getName()
	 */
	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getType()
	 */
	@Override
	public String getType() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getProperties()
	 */
	@Override
	public Properties getProperties() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getPropertyValue(java.lang.String)
	 */
	@Override
	public String getPropertyValue(String name) {
		// TODO Auto-generated method stub
		return null;
	}

    @Override
    public String getConnectionUrl() {
        // TODO Auto-generated method stub
        return null;
    }
}
