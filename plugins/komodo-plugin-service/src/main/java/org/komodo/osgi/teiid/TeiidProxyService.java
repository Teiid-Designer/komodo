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
package org.komodo.osgi.teiid;

import org.komodo.spi.query.TeiidService;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.DataTypeManager;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

public class TeiidProxyService implements TeiidService {

    private final String className;

    private long bundleId;

    private final BundleContext bundleContext;

    private TeiidService delegate;

    public TeiidProxyService(String className, long bundleId, BundleContext bundleContext) {
        this.className = className;
        this.bundleId = bundleId;
        this.bundleContext = bundleContext;
    }

    private synchronized void load() throws Exception {
        if (delegate != null)
            return;

        // Get the bundle.
        Bundle bundle = bundleContext.getBundle(bundleId);
        // Load the class and instantiate it.
        Class<?> clazz = bundle.loadClass(className);
        delegate = (TeiidService) clazz.getConstructor(String.class).newInstance(bundle.getSymbolicName());
    }

    @Override
    public TeiidVersion getVersion() throws Exception {
        load();
        return delegate.getVersion();
    }

    @Override
    public String getParentBundle() {
        Bundle bundle = bundleContext.getBundle(bundleId);
        return bundle.getSymbolicName();
    }

    @Override
    public DataTypeManager getDataTypeManager() throws Exception {
        load();
        return delegate.getDataTypeManager();
    }

    @Override
    public void nodeConvert(String sql, Object parent) throws Exception {
        load();
        delegate.nodeConvert(sql, parent);
    }

    @Override
    public TeiidInstance getTeiidInstance(TeiidParent teiidParent, TeiidJdbcInfo jdbcInfo) throws Exception {
        load();

        //
        // Ensure that 1 teiid instance at a time is fetched from
        // all delegates since teiid admin initialization is not thread-safe
        //
        // For example, 2 threads accessing vdb-builder hosted on jboss
        // can deadlock it since jboss' logging system is not thread-safe and
        // teiid uses the latter a lot for recording its progress
        //
        synchronized(TeiidProxyService.class) {
            return delegate.getTeiidInstance(teiidParent, jdbcInfo);
        }
    }
}
