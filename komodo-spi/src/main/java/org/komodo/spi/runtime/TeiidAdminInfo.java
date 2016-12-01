/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.komodo.spi.runtime;

import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;

/**
 *
 */
public interface TeiidAdminInfo extends TeiidConnectionInfo {

    /**
     * The default Teiid Admin persist password flag. Value is {@value} .
     */
    public static final boolean DEFAULT_PERSIST_PASSWORD = true;
    
    /**
     * The default Teiid Admin secure protocol flag. Value is {@value} .
     */
    public static final boolean DEFAULT_SECURE = true;
    
    /**
     * The default username for the Teiid Instance
     */
    public static final String DEFAULT_ADMIN_USERNAME = "admin"; //$NON-NLS-1$
    /**
     * The default password for the Teiid Instance
     */
    public static final String DEFAULT_ADMIN_PASSWORD = "admin"; //$NON-NLS-1$

    static class Util {

        public static int defaultPort(TeiidVersion version) {
            if (version == null)
                return 9999;

            if (DefaultTeiidVersion.Version.TEIID_9_1.get().isGreaterThan(version))
                return 9999;

            // Teiid 9.1 and Wildfly 10.x no longer use 9999 by default
            return 9990;
        }

        public static int defaultPort(String version) {
            return defaultPort(new DefaultTeiidVersion(version));
        }
    }
}
