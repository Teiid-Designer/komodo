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

/**
 * Callback that provides metadata for the creation of an object instance
 * from an extension point.
 *
 * Implementations should override {@link #process(Object, IConfigurationElement)} in order to
 * register, assign or otherwise process the created instances.
 *
 * @param <T> Class of the required extension instance
 */
public interface IExtensionRegistryCallback<T> {

    /**
     * Class attribute commonly used for defining a java class in an extension point
     */
    String CLASS_ATTRIBUTE_ID = "class"; //$NON-NLS-1$

    /**
     * @return extension point identifier
     */
    String getExtensionPointId();

    /**
     * @return extension point element identifier
     */
    String getElementId();

    /**
     * @return extension point element attribute identifier
     */
    String getAttributeId();

    /**
     * Callback method providing the created instance executable and its
     * related configuration element.
     *
     * Note: this will be called for each implementation of the extension point
     * hence it is expected that implementations of this method should
     * handle multiple calls appropriately.
     *
     * @param instance
     * @param element
     */
    void process(T instance, IConfigurationElement element);

    /**
     * Whether this extension expects a single implementation
     *
     * @return true for single implementations and false for multiple 
     */
    boolean isSingle();
}
