/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.teiid.runtime.client.admin;

import java.io.InputStream;

import org.komodo.spi.runtime.ITeiidServer;
import org.komodo.spi.runtime.version.ITeiidServerVersion;
import org.komodo.spi.runtime.version.TeiidServerVersion.Version;
import org.teiid.adminapi.Admin;
import org.teiid.adminapi.AdminException;
import org.teiid.adminapi.VDB;
import org.teiid.runtime.client.admin.v7.Admin7Spec;
import org.teiid.runtime.client.admin.v8.Admin8Spec;

/**
 *
 */
public abstract class AdminSpec {

    private final ITeiidServerVersion teiidVersion;

    /**
     * @param teiidVersion
     */
    public AdminSpec(ITeiidServerVersion teiidVersion) {
        this.teiidVersion = teiidVersion;
    }

    protected ITeiidServerVersion getTeiidVersion() {
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
    public static AdminSpec getInstance(ITeiidServerVersion teiidVersion) {
        if (teiidVersion.isLessThan(Version.TEIID_8_0.get()))
            return new Admin7Spec(teiidVersion);
        else
            return new Admin8Spec(teiidVersion);
    }

    /**
     * @param teiidServer
     * @return new admin instance
     * @throws AdminException 
     */
    public abstract Admin createAdmin(ITeiidServer teiidServer) throws AdminException;

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
}
