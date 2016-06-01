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

import java.util.Dictionary;
import java.util.Set;
import org.komodo.osgi.PluginService;
import org.komodo.spi.storage.StorageService;
import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleEvent;

public class StorageServiceProvider extends AbstractServiceProvider<StorageService, String> {

    private class StorageServiceKeyAdapter implements KeyFromValueAdapter<String, StorageService> {

        @Override
        public String getKey(StorageService service) {
            try {
                return service.getStorageId();
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
        }
    }

    private KeyInValueHashMap<String, StorageService> storageServices = new KeyInValueHashMap<>(new StorageServiceKeyAdapter());

    public StorageServiceProvider(PluginService pluginService) {
        super(pluginService);
    }

    @Override
    public StorageService addingBundle(Bundle bundle, BundleEvent event) {
        Dictionary<String, String> dict = bundle.getHeaders();
        String storageId = dict.get(StorageService.STORAGE_ID_PROPERTY);
        if (storageId == null)
            return null;

        // Get the class of the extension.
        String className = dict.get(StorageService.CLASS_PROPERTY);
        if (className == null)
            return null;

        StorageProxyService storageService = new StorageProxyService(className, bundle.getBundleId(), bundle.getBundleContext(),
                                                                     getPluginService().getCacheExpirationValue(),
                                                                     getPluginService().getCacheExpirationUnits());

        addStorageService(storageService);
        return storageService;
    }

    @Override
    public void removedBundle(Bundle bundle, BundleEvent event, StorageService storageService) {
        if (storageService == null)
            return;

        storageServices.remove(storageService);
    }

    /**
     * dispose of this provider
     */
    @Override
    public void dispose() {
        if (storageServices != null) {
            for (StorageService service : storageServices.values()) {
                service.dispose();
            }
            storageServices.clear();
        }
    
        super.dispose();
    }

    /**
     * @return the set of supported storage types
     */
    public Set<String> getSupportedStorageTypes() {
        return getKeys();
    }

    protected void addStorageService(StorageService storageService) {
        storageServices.add(storageService);
    }

    /**
     * @param storageId
     * @return the teiid service for the storage id
     * @throws Exception
     */
    public synchronized StorageService getStorageService(String storageId) throws Exception {
        StorageService storageService = storageServices.get(storageId);
        if (storageService != null)
            return storageService;

        String symbolicName = searchBundleIndex(storageId);
        if (symbolicName == null)
            throw new UnsupportedStorageException(storageId);

        Bundle bundle = getPluginService().findBundleBySymbolicName(symbolicName);
        if (bundle == null)
            throw new UnsupportedStorageException(storageId);

        // Once the bundle has become active, the PluginServiceTracker
        // should assign the StorageService back to this service
        getPluginService().startBundle(symbolicName);

        storageService = storageServices.get(storageId);
        return storageService;
    }
}
