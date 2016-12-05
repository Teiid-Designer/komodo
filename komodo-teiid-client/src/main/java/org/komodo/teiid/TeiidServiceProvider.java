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
package org.komodo.teiid;

import java.util.HashSet;
import java.util.Set;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.teiid.impl.TeiidServiceImpl;

public class TeiidServiceProvider {

    private static TeiidServiceProvider instance;

    public static TeiidServiceProvider getInstance() {
        if (instance == null)
            instance = new TeiidServiceProvider();

        return instance;
    }

    private TeiidService teiidService;

    private void init() throws Exception {
        if (teiidService != null)
            return;

        teiidService = new TeiidServiceImpl();

        //
        // Check that the version of the actual teiid service matches that expected
        // by the default teiid service
        //
        TeiidVersion runtimeVersion = teiidService.getVersion();
        if (! isSupportedTeiidVersion(runtimeVersion))
            throw new UnsupportedTeiidException(runtimeVersion);
    }

    /**
     * dispose of this provider
     */
    public void dispose() {
        if (teiidService != null) {
            teiidService.dispose();
            teiidService = null;
        }
    }

    /**
     * Using the version specified in the build as the default (thus supported),
     * return it and its major/minor wildcard as supported versions.
     * 
     * @return the set of supported Teiid versions.
     */
    public Set<TeiidVersion> getSupportedTeiidVersions() {
        TeiidVersion version = DefaultTeiidVersion.Version.DEFAULT_TEIID_VERSION.get();
        DefaultTeiidVersion wildcard = new DefaultTeiidVersion(version.getMajor(), version.getMinor(), TeiidVersion.WILDCARD);
        Set<TeiidVersion> versions = new HashSet<>();
        versions.add(version);
        versions.add(wildcard);
        return versions;
    }

    /**
     * @param teiidVersion
     * @return true if this teiid version is supported, otherwise false
     */
    public boolean isSupportedTeiidVersion(TeiidVersion teiidVersion) {
        for (TeiidVersion version : getSupportedTeiidVersions()) {
            if (teiidVersion.compareTo(version)) {
                return true;
            }
        }

        return false;
    }

    /**
     * @return the teiid service
     * @throws Exception
     */
    public TeiidService getTeiidService() throws Exception {
        init();
        return teiidService;
    }

    /**
     * @param version
     * @return the teiid service for the given version
     * @throws Exception
     */
    public synchronized TeiidService getTeiidService(TeiidVersion version) throws Exception {
        init();

        if (teiidService != null && isSupportedTeiidVersion(version)) {
            return teiidService;
        }

        throw new UnsupportedTeiidException(version);
    }

    /**
         * Used internally for allocating the teiid service. Only declared public for test purposes.
         *
         * @param teiidService
         */
    public void setTeiidService(TeiidService teiidService) {
        this.teiidService = teiidService;
    }
}
