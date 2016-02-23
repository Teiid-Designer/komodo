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
package org.komodo.spi.query;

import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.DataTypeManager;

public interface TeiidService extends StringConstants {

    /**
     * The prefix of all teiid service bundles
     */
    String TEIID_BUNDLE_PREFIX = "org.komodo.teiid" + HYPHEN;

    /**
     * The version property designated in the manifests of the Teiid bundles
     */
    String VERSION_PROPERTY = "Teiid-Client-Version";

    /**
     * The class property designated in the manifests of the teiid bundles
     */
    String CLASS_PROPERTY = "Teiid-Service-Class";

    /**
     * @return version of this service
     * @throws Exception 
     */
    TeiidVersion getVersion() throws Exception;

    /**
     * @return the data type manager
     * @throws Exception 
     */
    DataTypeManager getDataTypeManager() throws Exception;

    /**
     * @return the symbolic name of this service's parent bundle
     */
    String getParentBundle();

    /**
     * Convert the sql into a tree beneath parent
     *
     * @param sql
     * @param parent
     * @throws Exception 
     */
    void convert(String sql, Object parent) throws Exception;
}