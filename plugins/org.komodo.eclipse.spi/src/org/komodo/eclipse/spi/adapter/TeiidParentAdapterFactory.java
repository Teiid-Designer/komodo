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
package org.komodo.eclipse.spi.adapter;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.wst.server.core.IServer;
import org.komodo.spi.runtime.TeiidParent;

/**
 * Adapter factory that can adapt an {@link TeiidParent} to an {@link IServer}
 * 
 *
 */
public class TeiidParentAdapterFactory implements IAdapterFactory {

	@Override
	public Object getAdapter(Object adaptableObject, Class adapterType) {
		if (! IServer.class.isAssignableFrom(adapterType))
            return null;

		if (adaptableObject instanceof IServer)
			return adaptableObject;

		if (adaptableObject instanceof TeiidParent) {
			TeiidParent teiidParent = (TeiidParent) adaptableObject;
			Object parentObject = teiidParent.getParentObject();
			return getAdapter(parentObject, adapterType);
		}
        
        return null;
	}

	@Override
	public Class[] getAdapterList() {
		return new Class[] { IServer.class };
	}

}
