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
package org.teiid.runtime.client.admin.v7;

import java.io.InputStream;

import org.komodo.spi.runtime.IExecutionAdmin;
import org.komodo.spi.runtime.ITeiidAdminInfo;
import org.komodo.spi.runtime.ITeiidInstance;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.VDB.Status;
import org.teiid.runtime.client.admin.AdminSpec;

/**
 *
 */
public class Admin7Spec extends AdminSpec {

    /**
     * Test VDB model
     */
    public static final String TEST_VDB = "<vdb name=\"ping\" version=\"1\">" + //$NON-NLS-1$
    "<model visible=\"true\" name=\"Foo\" type=\"VIRTUAL\" path=\"/dummy/Foo\">" + //$NON-NLS-1$
    "<source name=\"s\" translator-name=\"loopback\"/>" + //$NON-NLS-1$
    "</model>" + //$NON-NLS-1$
    "</vdb>"; //$NON-NLS-1$

    /**
     * @param teiidVersion
     */
    public Admin7Spec(ITeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    @Override
    public Admin createAdmin(ITeiidInstance teiidInstance) throws AdminException {
        ITeiidAdminInfo teiidAdminInfo = teiidInstance.getTeiidAdminInfo();
        char[] passwordArray = null;
        if (teiidAdminInfo.getPassword() != null) {
            passwordArray = teiidAdminInfo.getPassword().toCharArray();
        }

        Admin admin = Admin7Factory.getInstance().createAdmin(teiidAdminInfo.getUsername(), 
                                                            passwordArray, 
                                                            teiidAdminInfo.getUrl());

        return admin;
    }

    @Override
    public String getTestVDB() {
        return TEST_VDB;
    }

    @Override
    public Status getLoadingVDBStatus() {
        return VDB.Status.ACTIVE;
    }

    @Override
    public void deploy(Admin admin, String fileName, InputStream iStream) throws AdminException {
        admin.deployVDB(fileName, iStream);
    }

    @Override
    public void undeploy(Admin admin, String vdbName, int version) {
        if (IExecutionAdmin.PING_VDB.equals(vdbName)) {
            /* Teiid 7 uses the name attribute of the vdb */
            vdbName = "ping"; //$NON-NLS-1$
        }

        admin.deleteVDB(vdbName, version);
    }
}
