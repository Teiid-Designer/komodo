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
package org.komodo.rest.cors;

import org.komodo.rest.cors.rest3.KCorsFilter;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.utils.ArgCheck;

/**
 * Factory for instantiating the Cors interceptor / filter
 */
public class KCorsFactory {

    private static KCorsFactory instance;

    public static KCorsFactory getInstance() {
        if (instance == null)
            instance = new KCorsFactory();

        return instance;
    }

    private KCorsFactory() { /* nothing to do */ };

    /**
     * 
     * @param teiidVersion
     * @return new {@link KCorsHandler}
     */
    public KCorsHandler createHandler(TeiidVersion teiidVersion) throws Exception {
        ArgCheck.isNotNull(teiidVersion, "teiid version");

        if (teiidVersion.isLessThan(Version.TEIID_9_1))
            throw new IllegalStateException("Version of Teiid incompatible");

        return new KCorsFilter();
    }
}
