/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.eclipse.spi.adapter;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.wst.server.core.IServer;
import org.komodo.spi.runtime.ITeiidParent;

/**
 * Adapter factory that can adapt an {@link ITeiidParent} to an {@link IServer}
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

		if (adaptableObject instanceof ITeiidParent) {
			ITeiidParent teiidParent = (ITeiidParent) adaptableObject;
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
