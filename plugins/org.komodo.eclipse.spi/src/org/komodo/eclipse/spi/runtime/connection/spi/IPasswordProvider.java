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
package org.komodo.eclipse.spi.runtime.connection.spi;

import org.komodo.eclipse.spi.KEclipseSPIPlugin;

/**
 * Simple interface provides DQP preview manager ability to ask for user password
 * during Preview action setup.
 *
 *
 */
public interface IPasswordProvider {

    /**
     * Extension Point ID
     */
    String PASSWORD_PROVIDER_EXTENSION_POINT_ID = KEclipseSPIPlugin.PLUGIN_ID + ".teiidPasswordProvider"; //$NON-NLS-1$

    /**
     * Extension Point Element ID
     */
    String PASSWORD_PROVIDER_ELEMENT_ID = "passwordProvider"; //$NON-NLS-1$

	/**
	 * Get the password for the given criteria
	 *
	 * @param modleName
	 * @param profileName
	 * @return the password
	 */
	String getPassword(String modleName, String profileName);
}
