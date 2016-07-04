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

import java.util.concurrent.TimeUnit;
import org.komodo.osgi.AbstractProxyService;
import org.komodo.spi.query.QueryService;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.TeiidJdbcInfo;
import org.komodo.spi.runtime.TeiidParent;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.DataTypeManager;
import org.komodo.utils.KLog;
import org.osgi.framework.BundleContext;

public class TeiidProxyService extends AbstractProxyService<TeiidService, TeiidInstance>
    implements TeiidService {

    public TeiidProxyService(String className, long bundleId,
                                                 BundleContext bundleContext,
                                                 int cacheExpiryValue, TimeUnit cacheExpiryUnits) {
        super(className, bundleId, bundleContext, cacheExpiryValue, cacheExpiryUnits);
    }

    @Override
    protected void onInstanceRemoval(TeiidInstance instance) {
        instance.disconnect();
    }

    @Override
    public TeiidVersion getVersion() throws Exception {
        return getDelegate().getVersion();
    }

    @Override
    public DataTypeManager getDataTypeManager() throws Exception {
        return getDelegate().getDataTypeManager();
    }

    @Override
    public void nodeConvert(String sql, Object parent) throws Exception {
        getDelegate().nodeConvert(sql, parent);
    }

    @Override
    public TeiidInstance getTeiidInstance(TeiidParent teiidParent, TeiidJdbcInfo jdbcInfo) throws Exception {

        //
        // Ensure that 1 teiid instance at a time is fetched from
        // all delegates since teiid admin initialization is not thread-safe
        //
        // For example, 2 threads accessing vdb-builder hosted on jboss
        // can deadlock it since jboss' logging system is not thread-safe and
        // teiid uses the latter a lot for recording its progress
        //
        synchronized(TeiidProxyService.class) {
            KLog logger = KLog.getLogger();

            StringBuffer buf = new StringBuffer();
            int parentHash = teiidParent.hashCode();
            buf.append(parentHash);
            buf.append(jdbcInfo.getUrl());

            if (logger.isDebugEnabled()) {
                logger.debug("Getting Teiid Instance for host {0} and identity code {1}",
                                                      teiidParent.getHost(), buf.toString());
            }

            String key = buf.toString();
            TeiidInstance instance = retrieve(key);
            if (instance != null && ! instance.isSound()) {
                invalidate(key);
                instance = null;
            }

            if (instance == null) {
                if (logger.isDebugEnabled()) {
                    logger.debug("Teiid Instance with id {0} not found in cache({1}). Creating new one.",
                                                          key, cacheSize());
                }

                instance = getDelegate().getTeiidInstance(teiidParent, jdbcInfo);
                cache(key, instance);
            }

            return instance;
        }
    }

    @Override
    public QueryService getQueryService(String user, String passwd) throws Exception {
        return getDelegate().getQueryService(user, passwd);
    }
}
