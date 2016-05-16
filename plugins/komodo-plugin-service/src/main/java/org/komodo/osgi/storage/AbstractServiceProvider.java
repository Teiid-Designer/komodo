/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.osgi.storage;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import org.komodo.osgi.PluginService;
import org.komodo.spi.bundle.BundleService;
import org.osgi.framework.Bundle;
import org.osgi.util.tracker.BundleTracker;

public abstract class AbstractServiceProvider<S extends BundleService, K> extends BundleTracker<S> {

    private final PluginService pluginService;

    private final Map<K, String> bundleIndex = new HashMap<>();

    public AbstractServiceProvider(PluginService pluginService) {
        super(pluginService.getContext(), Bundle.ACTIVE | Bundle.STOPPING, null);
        this.pluginService = pluginService;
    }

    public PluginService getPluginService() {
        return pluginService;
    }

    public Set<K> getKeys() {
        return Collections.unmodifiableSet(bundleIndex.keySet());
    }

    /**
     * Register the bundle against the storageId
     *
     * @param storageId
     * @param symbolicName
     */
    public void register(K key, String symbolicName) {
        bundleIndex.put(key, symbolicName);
    }

    /**
     * @param storageId
     * @return the bundle that satisfies the given key
     */
    public String searchBundleIndex(K key) {
        return bundleIndex.get(key);
    }

    public void dispose() {
        close();
        bundleIndex.clear();
    }
}
