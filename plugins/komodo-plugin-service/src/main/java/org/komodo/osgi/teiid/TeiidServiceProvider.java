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

import java.util.Dictionary;
import java.util.Set;
import org.komodo.osgi.Messages;
import org.komodo.osgi.PluginService;
import org.komodo.osgi.storage.AbstractServiceProvider;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleEvent;

public class TeiidServiceProvider extends AbstractServiceProvider<TeiidService, TeiidVersion> {

    private TeiidService teiidService;

    public TeiidServiceProvider(PluginService pluginService) {
        super(pluginService);
    }

    @Override
    public TeiidService addingBundle(Bundle bundle, BundleEvent event) {
        Dictionary<String, String> dict = bundle.getHeaders();
        String version = dict.get(TeiidService.VERSION_PROPERTY);
        if (version == null)
            return null;

        // Get the class of the extension.
        String className = dict.get(TeiidService.CLASS_PROPERTY);
        TeiidProxyService teiidService = new TeiidProxyService(className, bundle.getBundleId(), bundle.getBundleContext(),
                                                               getPluginService().getCacheExpirationValue(),
                                                               getPluginService().getCacheExpirationUnits());
        setTeiidService(teiidService);
        return teiidService;
    }

    @Override
    public void removedBundle(Bundle bundle, BundleEvent event, TeiidService teiidService) {
        if (teiidService == null)
            return;

        if (!teiidService.equals(getTeiidService()))
            return;

        setTeiidService(null);
    }

    /**
     * dispose of this provider
     */
    @Override
    public void dispose() {
        if (teiidService != null) {
            teiidService.dispose();
            teiidService = null;
        }

        super.dispose();
    }

    /**
     * Register the bundle against the teiid version
     *
     * @param version
     * @param symbolicName
     */
    public void register(String version, String symbolicName) {
        // This bundle is a teiid service bundle
        TeiidVersion teiidVersion = new DefaultTeiidVersion(version);
        register(teiidVersion, symbolicName);

        // Add a fallback
        teiidVersion = new DefaultTeiidVersion(teiidVersion.getMajor(),
                                                                               teiidVersion.getMinor(),
                                                                               TeiidVersion.WILDCARD);
        register(teiidVersion, symbolicName);
    }

    @Override
    public String searchBundleIndex(TeiidVersion teiidVersion) {
        String bundleName = super.searchBundleIndex(teiidVersion);
        if (bundleName != null)
            return bundleName;

        // Try the wildcard version
        teiidVersion = new DefaultTeiidVersion(teiidVersion.getMajor(),
                                               teiidVersion.getMinor(),
                                               TeiidVersion.WILDCARD);
        return super.searchBundleIndex(teiidVersion);
    }

    /**
     * @return the set of supported Teiid versions
     */
    public Set<TeiidVersion> getSupportedTeiidVersions() {
        return getKeys();
    }

    /**
     * @return the teiid service
     */
    public TeiidService getTeiidService() {
        return teiidService;
    }

    protected void setTeiidService(TeiidService teiidService) {
        this.teiidService = teiidService;
    }

    /**
     * @param version
     * @return the teiid service for the given version
     * @throws Exception
     */
    public synchronized TeiidService getTeiidService(TeiidVersion version) throws Exception {
        if (teiidService != null) {
            if (teiidService.getVersion().equals(version))
                return teiidService;

            //
            // teiid service is not the appropriate version so
            // - dispose it
            // - stop its parent bundle
            //
            teiidService.dispose();
            String parentBundleName = teiidService.getParentBundle();

            // Once the bundle has stopped, the PluginServiceTracker
            // should null the teiidService field
            getPluginService().stopBundle(parentBundleName);

            if (getTeiidService() != null)
                throw new Exception(Messages.getString(Messages.PluginService.TeiidServiceBundleFailedToStop, parentBundleName));
        }

        String symbolicName = searchBundleIndex(version);
        if (symbolicName == null)
            throw new UnsupportedTeiidException(version);

        Bundle bundle = getPluginService().findBundleBySymbolicName(symbolicName);
        if (bundle == null)
            throw new UnsupportedTeiidException(version);

        // Once the bundle has become active, the PluginServiceTracker
        // should assign the TeiidService back to this service
        getPluginService().startBundle(symbolicName);

        return this.teiidService;
    }
}
