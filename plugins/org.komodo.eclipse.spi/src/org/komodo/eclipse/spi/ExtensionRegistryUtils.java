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
package org.komodo.eclipse.spi;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.util.NLS;

/**
 * Utilities for interfacing with the extension registry
 */
public class ExtensionRegistryUtils {

    /**
     * Creates an extension instance using the metadata from the given {@link IExtensionRegistryCallback}
     *
     * @param callback
     * @throws Exception
     */
    public static <T> void createExtensionInstances(IExtensionRegistryCallback<T> callback) throws Exception {
        IExtensionRegistry extRegistry = Platform.getExtensionRegistry();
        IConfigurationElement[] extensions = extRegistry.getConfigurationElementsFor(callback.getExtensionPointId());

        int extCount = 0;
        for (IConfigurationElement element : extensions) {
            if (callback.getElementId() != null && ! callback.getElementId().equals(element.getName()))
                continue;

            // Found at least 1 implementation of this extension
            extCount++;

            @SuppressWarnings("unchecked")
			T extension = (T) element.createExecutableExtension(callback.getAttributeId());
            callback.process(extension, element);

            if (callback.isSingle())
                return;
        }

        if (extCount > 0)
            return;

        throw new IllegalStateException(NLS.bind(Messages.NoRegisteredExtension, callback.getExtensionPointId()));
    }
}
