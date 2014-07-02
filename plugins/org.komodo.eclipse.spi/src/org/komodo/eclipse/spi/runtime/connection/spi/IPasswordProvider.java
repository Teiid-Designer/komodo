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
