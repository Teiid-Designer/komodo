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
package org.komodo.teiid.impl;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import org.komodo.spi.runtime.TeiidVdb;
import org.komodo.teiid.Messages;
import org.teiid.adminapi.Model;
import org.teiid.adminapi.VDB;
import org.teiid.adminapi.VDB.Status;
import org.teiid.adminapi.impl.VDBMetaData;
import org.teiid.adminapi.impl.VDBMetadataParser;
import org.teiid.core.util.ArgCheck;

public class TeiidVdbImpl implements TeiidVdb, Comparable<TeiidVdbImpl> {

    private static final String PREVIEW = "preview"; //$NON-NLS-1$

    private static final String DEPLOYMENT_NAME = "deployment-name"; //$NON-NLS-1$

    private String name;

    private String version;

    private final boolean isPreview;

    private final String deploymentName;

    private boolean isActive = false;
    private boolean isLoading = false;
    private boolean hasFailed = false;
    private boolean wasRemoved = false;
    private boolean hasModels = false;

    private List<String> errors;

    private Collection<String> modelNames = new ArrayList<>();

    private Properties properties = new Properties();

    private String vdbExport;

    public TeiidVdbImpl(VDB vdb) throws Exception {
        ArgCheck.isNotNull(vdb, "vdb"); //$NON-NLS-1$

        if (! (vdb instanceof VDBMetaData))
            throw new Exception(Messages.getString(Messages.TeiidVdb.onlySupportingDynamicVdbs));

        VDBMetaData vdbMeta = (VDBMetaData)vdb;
        if (! vdbMeta.isXmlDeployment())
            throw new Exception(Messages.getString(Messages.TeiidVdb.onlySupportingDynamicVdbs));

        name = vdb.getName();

        // Autoboxing first if version is an int as defined in teiid 8
        Object o = vdb.getVersion();
        version = o.toString();

        isPreview = Boolean.parseBoolean(vdb.getProperties().getProperty(PREVIEW));
        deploymentName = vdb.getProperties().getProperty(DEPLOYMENT_NAME);

        Status status = vdb.getStatus();
        isActive = Status.ACTIVE.equals(status);
        isLoading = Status.LOADING.equals(status);
        hasFailed = Status.FAILED.equals(status);
        wasRemoved = Status.REMOVED.equals(status);
        hasModels = !vdb.getModels().isEmpty();

        errors = vdb.getValidityErrors();

        for (Model model : vdb.getModels()) {
            modelNames.add(model.getName());
        }

        for (String name : vdb.getProperties().stringPropertyNames()) {
            properties.setProperty(name, vdb.getPropertyValue(name));
        }

        ByteArrayOutputStream out = new ByteArrayOutputStream();
        VDBMetadataParser.marshell((VDBMetaData)vdb, out);
        vdbExport = new String(out.toByteArray());
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#compareTo(org.teiid.designer.runtime.impl.TeiidVdb)
     */
    @Override
    public int compareTo(TeiidVdbImpl vdb) {
        ArgCheck.isNotNull(vdb, "vdb"); //$NON-NLS-1$
        return getName().compareTo(vdb.getName());
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (obj.getClass() != getClass())
            return false;

        TeiidVdb other = (TeiidVdb)obj;

        if (getName().equals(other.getName()))
            return true;

        return false;
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.getName() == null) ? 0 : this.getName().hashCode());
        return result;
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#getName()
     */
    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getDeployedName() {
        return deploymentName;
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#getVersion()
     */
    @Override
    public String getVersion() {
        return version;
    }

    /* (non-Javadoc)
     * @see org.teiid.designer.runtime.impl.ITeiidVdb#isPreviewVdb()
     */
    @Override
    public boolean isPreviewVdb() {
        return isPreview;
    }

    @Override
    public boolean isActive() {
        return isActive;
    }

    @Override
    public boolean isLoading() {
        return isLoading;
    }

    @Override
    public boolean hasFailed() {
        return hasFailed;
    }

    @Override
    public boolean wasRemoved() {
        return wasRemoved;
    }

    @Override
    public List<String> getValidityErrors() {
        if (this.errors != null)
            return Collections.unmodifiableList(this.errors);

        return Collections.emptyList();
    }

    @Override
    public boolean hasModels() {
        return hasModels;
    }

    @Override
    public Collection<String> getModelNames() {
        if (!hasModels())
            return Collections.emptyList();

        return modelNames;
    }

    @Override
    public String getPropertyValue(String key) {
        return properties.getProperty(key);
    }

    @Override
    public Properties getProperties() {
        return properties;
    }

    @Override
    public String export() throws Exception {
        return vdbExport;
    }
}
