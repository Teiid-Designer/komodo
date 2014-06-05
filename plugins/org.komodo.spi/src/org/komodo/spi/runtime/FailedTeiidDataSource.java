/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.komodo.spi.runtime;

import java.util.Properties;

/**
 *
 */
public class FailedTeiidDataSource implements ITeiidDataSource {

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

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#setProfileName(java.lang.String)
	 */
	@Override
	public void setProfileName(String name) {
		// TODO Auto-generated method stub

	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#getProfileName()
	 */
	@Override
	public String getProfileName() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#isPreview()
	 */
	@Override
	public boolean isPreview() {
		// TODO Auto-generated method stub
		return false;
	}

	/* (non-Javadoc)
	 * @see org.teiid.designer.runtime.spi.ITeiidDataSource#setPreview(boolean)
	 */
	@Override
	public void setPreview(boolean isPreview) {
		// TODO Auto-generated method stub

	}

}
