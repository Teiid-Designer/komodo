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

import java.util.Dictionary;
import org.komodo.osgi.teiid.TeiidProxyService;
import org.komodo.spi.query.TeiidService;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleEvent;
import org.osgi.util.tracker.BundleTracker;

public class PluginServiceTracker extends BundleTracker<TeiidService> {

    private final PluginService service;

    public PluginServiceTracker(PluginService service) {
        super(service.getContext(), Bundle.ACTIVE | Bundle.STOPPING, null);
        this.service = service;
    }

    @Override
    public TeiidService addingBundle(Bundle bundle, BundleEvent event) {
        Dictionary<String, String> dict = bundle.getHeaders();
        String version = dict.get(TeiidService.VERSION_PROPERTY);
        if (version == null)
            return null;

        // Get the class of the extension.
        String className = dict.get(TeiidService.CLASS_PROPERTY);
        TeiidProxyService proxy = new TeiidProxyService(className, 
                                                                                                bundle.getBundleId(), bundle.getBundleContext());
        service.setTeiidService(proxy);
        return proxy;
    }

    @Override
    public void removedBundle(Bundle bundle, BundleEvent event, TeiidService teiidService) {
        if (teiidService == null)
            return;

        if (! teiidService.equals(service.getTeiidService()))
            return;

        service.setTeiidService(null);
    }
}
