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
package org.komodo.eclipse.spi.registry;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.IConfigurationElement;
import org.komodo.eclipse.spi.ExtensionRegistryUtils;
import org.komodo.eclipse.spi.IExtensionRegistryCallback;

/**
 * @param <K> 
 * @param <V>
 */
public abstract class AbstractExtensionRegistry<K, V> {

    private Map<K, V> extensions = new HashMap<K, V>();

    private String extPointId;

    private String elementId;

    protected AbstractExtensionRegistry(String extPointId, String elementId) throws Exception {
        this.extPointId = extPointId;
        this.elementId = elementId;
        load();
    }

    private void load() throws Exception {
        IExtensionRegistryCallback<V> callback = new IExtensionRegistryCallback<V>() {

            @Override
            public String getExtensionPointId() {
                return extPointId;
            }

            @Override
            public String getElementId() {
                return elementId;
            }

            @Override
            public String getAttributeId() {
                return CLASS_ATTRIBUTE_ID;
            }

            @Override
            public boolean isSingle() {
                return false;
            }

            @Override
            public void process(V instance, IConfigurationElement element) {
                register(element, instance);
            }
        };

        ExtensionRegistryUtils.createExtensionInstances(callback);
    }
    
    protected abstract void register(IConfigurationElement configurationElement, V extension);

    protected void register(K key, V value) {
        extensions.put(key, value);
    }

    /**
     * Get a register value applicable for the given key
     * 
     * @param key
     * 
     * @return instance of V
     */
    public V getRegistered(K key) {
        return extensions.get(key);
    }

    /**
     * @return all the registered keys
     */
    public Collection<K> getRegisteredKeys() {
        return Collections.unmodifiableCollection(extensions.keySet());
    }

    /**
     * @return all the registered extensions
     */
    public Collection<V> getRegistered() {
        return Collections.unmodifiableCollection(extensions.values());
    }

    protected Collection<Map.Entry<K, V>> getRegisteredEntries() {
        return Collections.unmodifiableCollection(extensions.entrySet());
    }
}
