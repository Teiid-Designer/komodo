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
package org.teiid.runtime.client.admin;

import java.io.InputStream;
import java.util.Collection;
import org.komodo.spi.runtime.DataSourceDriver;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;
import org.teiid.adminapi.VDB;
import org.teiid.runtime.client.Messages;
import org.teiid.runtime.client.admin.v8.Admin8Spec;

/**
 *
 */
public abstract class AdminSpec {

    private final TeiidVersion teiidVersion;

    /**
     * @param teiidVersion
     */
    public AdminSpec(TeiidVersion teiidVersion) {
        this.teiidVersion = teiidVersion;
    }

    protected TeiidVersion getTeiidVersion() {
        return this.teiidVersion;
    }

    /**
     * Get the instance of the {@link AdminSpec} appropriate to the version
     * of the teiid instance.
     * <p>
     * Note. the teiid version is wrapped since the implementation of the
     * version 8 spec caches against the version.
     *
     * @param teiidVersion
     * @return admin spec for specific teiid version
     */
    public static AdminSpec getInstance(TeiidVersion teiidVersion) {
        if (teiidVersion.isLessThan(Version.TEIID_8_0.get()))
            throw new UnsupportedOperationException(Messages.getString(Messages.Misc.TeiidVersionNotSupported, teiidVersion));
        else
            return new Admin8Spec(teiidVersion);
    }

    /**
     * @param teiidInstance
     * @return new admin instance
     * @throws AdminException 
     */
    public abstract Admin createAdmin(TeiidInstance teiidInstance) throws AdminException;

    /**
     * @return test VDB configuration
     */
    public abstract String getTestVDB();

    /**
     * @return the loading status value
     */
    public abstract VDB.Status getLoadingVDBStatus();

    /**
     * @param admin
     * @param fileName
     * @param iStream
     * @throws AdminException 
     */
    public abstract void deploy(Admin admin, String fileName, InputStream iStream) throws AdminException;

    /**
     * @param admin
     * @param vdbName
     * @param version
     * @throws AdminException 
     */
    public abstract void undeploy(Admin admin, String vdbName, int version) throws AdminException;

    /**
     * @param admin
     * @return collection of installed data source drivers
     * @throws AdminException
     */
    public abstract Collection<DataSourceDriver> getDataSourceDrivers(Admin admin) throws AdminException;
}
