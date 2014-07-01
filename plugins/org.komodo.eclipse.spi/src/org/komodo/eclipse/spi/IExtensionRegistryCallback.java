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
