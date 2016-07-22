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

import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.komodo.osgi.AbstractProxyService;
import org.komodo.spi.storage.StorageConnector;
import org.komodo.spi.storage.StorageService;
import org.komodo.spi.storage.StorageConnector.Descriptor;
import org.komodo.utils.KLog;
import org.osgi.framework.BundleContext;

public class StorageProxyService extends AbstractProxyService<StorageService, StorageConnector>implements StorageService {

    public StorageProxyService(String className, long bundleId,
                                                       BundleContext bundleContext,
                                                       int cacheExpiryValue,
                                                       TimeUnit cacheExpiryUnits) {
        super(className, bundleId, bundleContext, cacheExpiryValue, cacheExpiryUnits);
    }

    @Override
    protected void onInstanceRemoval(StorageConnector instance) {
        // TODO Auto-generated method stub

    }

    @Override
    public String getStorageId() throws Exception {
        return getDelegate().getStorageId();
    }

    @Override
    public String getDescription() throws Exception {
        return getDelegate().getDescription();
    }

    @Override
    public Set<Descriptor> getDescriptors() throws Exception {
        return getDelegate().getDescriptors();
    }

    @Override
    public StorageConnector getConnector(Properties parameters) throws Exception {
        getDelegate();

        synchronized (StorageProxyService.class) {
            KLog logger = KLog.getLogger();

            StringBuffer buf = new StringBuffer(OPEN_SQUARE_BRACKET);
            Iterator<Entry<Object, Object>> entrySet = parameters.entrySet().iterator();
            while (entrySet.hasNext()) {
                Map.Entry<Object, Object> property = entrySet.next();
                buf.append(property.getKey()).append(COLON).append(property.getValue());

                if (entrySet.hasNext())
                    buf.append(COMMA).append(SPACE);
            }
            buf.append(CLOSE_SQUARE_BRACKET);

            if (logger.isDebugEnabled()) {
                logger.debug("Getting Storage Connector for properties: {0}", buf.toString());
            }

            String key = buf.toString();
            StorageConnector instance = retrieve(key);
            if (instance != null) {
                invalidate(key);
                instance = null;
            }

            if (instance == null) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Storage Connector with properties {0} not found in cache({1}). Creating new one.",
                                 key,
                                 cacheSize());
                }

                instance = getDelegate().getConnector(parameters);
                cache(key, instance);
            }

            return instance;
        }
    }
}
