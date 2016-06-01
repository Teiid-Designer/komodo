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
package org.komodo.osgi;

import java.util.concurrent.TimeUnit;
import org.komodo.spi.bundle.BundleService;
import org.komodo.utils.ArgCheck;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.RemovalListener;
import com.google.common.cache.RemovalNotification;

public abstract class AbstractProxyService<D extends BundleService, T> {

    private RemovalListener<String, T> removalListener = new RemovalListener<String, T>() {

        @Override
        public void onRemoval(RemovalNotification<String, T> notification) {
            T instance = notification.getValue();
            if (instance == null)
                return;

            onInstanceRemoval(instance);
        }
    };

    private D delegate;

    private final String className;

    private long bundleId;

    private final BundleContext bundleContext;

    private final Cache<String, T> instanceCache;

    public AbstractProxyService(String className, long bundleId,
                                                        BundleContext bundleContext,
                                                        int cacheExpiryValue,
                                                        TimeUnit cacheExpiryUnits) {
        ArgCheck.isNotNull(className);
        ArgCheck.isNotNull(bundleContext);

        this.className = className;
        this.bundleId = bundleId;
        this.bundleContext = bundleContext;
        this.instanceCache = CacheBuilder.newBuilder().concurrencyLevel(4).expireAfterWrite(cacheExpiryValue,
                                                                                            cacheExpiryUnits).softValues().removalListener(removalListener).build();
    }

    @SuppressWarnings( "unchecked" )
    private synchronized void load() throws Exception {
        if (delegate != null)
            return;

        // Get the bundle.
        Bundle bundle = bundleContext.getBundle(bundleId);
        // Load the class and instantiate it.
        Class<?> clazz = bundle.loadClass(className);
        delegate = (D) clazz.getConstructor(String.class).newInstance(bundle.getSymbolicName());
    }

    public D getDelegate() throws Exception {
        load();
        return delegate;
    }

    protected abstract void onInstanceRemoval(T instance);

    public String getParentBundle() {
        Bundle bundle = bundleContext.getBundle(bundleId);
        return bundle.getSymbolicName();
    }

    protected T retrieve(String key) {
        return instanceCache.getIfPresent(key);
    }

    protected void cache(String key, T instance) {
        instanceCache.put(key, instance);
    }

    protected void invalidate(String key) {
        instanceCache.invalidate(key);
    }

    protected long cacheSize() {
        return instanceCache.size();
    }

    public void dispose() {
        instanceCache.invalidateAll();

        if (delegate != null)
            delegate.dispose();
    }
}
