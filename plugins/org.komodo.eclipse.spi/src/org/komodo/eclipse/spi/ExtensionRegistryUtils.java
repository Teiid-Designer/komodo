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
