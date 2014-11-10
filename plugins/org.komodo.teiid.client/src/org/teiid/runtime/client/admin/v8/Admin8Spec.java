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
package org.teiid.runtime.client.admin.v8;

import java.io.InputStream;
import java.util.Collection;
import org.komodo.spi.runtime.DataSourceDriver;
import org.komodo.spi.runtime.TeiidAdminInfo;
import org.komodo.spi.runtime.TeiidInstance;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminComponentException;
import org.teiid.adminapi.AdminException;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.VDB.Status;
import org.teiid.runtime.client.admin.AdminSpec;

/**
 *
 */
public class Admin8Spec extends AdminSpec {

    private static final String TEST_VDB = "<vdb name=\"ping\" version=\"1\">" + //$NON-NLS-1$
    "<model visible=\"true\" name=\"Foo\" type=\"PHYSICAL\" path=\"/dummy/Foo\">" + //$NON-NLS-1$
    "<source name=\"s\" translator-name=\"loopback\"/>" + //$NON-NLS-1$
    "<metadata type=\"DDL\"><![CDATA[CREATE FOREIGN TABLE G1 (e1 string, e2 integer);]]> </metadata>" + //$NON-NLS-1$
    "</model>" + //$NON-NLS-1$
    "</vdb>"; //$NON-NLS-1$

    /**
     * @param teiidVersion
     */
    public Admin8Spec(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    @Override
    public Admin createAdmin(TeiidInstance teiidInstance) throws AdminException {
        TeiidAdminInfo teiidAdminInfo = teiidInstance.getTeiidAdminInfo();
        char[] passwordArray = null;
        if (teiidAdminInfo.getPassword() != null) {
            passwordArray = teiidAdminInfo.getPassword().toCharArray();
        }

        TeiidVersion teiidVersion = null;
        try {
            teiidVersion = teiidInstance.getVersion();
        } catch (Exception ex) {
            throw new AdminComponentException(ex);
        }

        Admin admin = Admin8Factory.getInstance().createAdmin(teiidVersion,
                                                              teiidInstance.getHost(),
                                                              teiidAdminInfo.getPort(),
                                                              teiidAdminInfo.getUsername(),
                                                              passwordArray);

        return admin;
    }

    @Override
    public String getTestVDB() {
        return TEST_VDB;
    }

    @Override
    public Status getLoadingVDBStatus() {
        return VDB.Status.LOADING;
    }

    @Override
    public void deploy(Admin admin, String fileName, InputStream iStream) throws AdminException {
        admin.deploy(fileName, iStream);
    }

    @Override
    public void undeploy(Admin admin, String vdbName, int version) throws AdminException {
        admin.undeploy(vdbName);
    }

    @Override
    public Collection<DataSourceDriver> getDataSourceDrivers(Admin admin) throws AdminException {
        return admin.getDataSourceDrivers();
    }
}
