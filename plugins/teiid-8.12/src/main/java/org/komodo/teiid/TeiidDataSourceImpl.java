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

import java.util.Properties;
import org.komodo.spi.runtime.TeiidDataSource;
import org.teiid.core.util.ArgCheck;

public class TeiidDataSourceImpl implements Comparable<TeiidDataSourceImpl>, TeiidDataSource {

    private final String displayName;
    private final String dataSourceName;
    private final String dataSourceType;
    private final Properties properties;

    private String connectionProfileName;

    private boolean isPreview = false;

    public TeiidDataSourceImpl(String displayName,
                               String dataSourceName,
                               String dataSourceType) {
        this(displayName, dataSourceName, dataSourceType, new Properties());
    }

    public TeiidDataSourceImpl(String displayName,
                               String dataSourceName,
                               String dataSourceType,
                               Properties properties) {
        ArgCheck.isNotEmpty(dataSourceName, "dataSourceName"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(dataSourceType, "dataSourceType"); //$NON-NLS-1$

        this.displayName = displayName;
        this.dataSourceName = dataSourceName;
        this.dataSourceType = dataSourceType;
        this.properties = properties;
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(TeiidDataSourceImpl dataSource) {
        ArgCheck.isNotNull(dataSource, "dataSource"); //$NON-NLS-1$
        return getName().compareTo(dataSource.getName());
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null)
            return false;
        if (obj.getClass() != getClass())
            return false;

        TeiidDataSource other = (TeiidDataSource)obj;

        if (getName().equals(other.getName()))
            return true;

        return false;
    }

    @Override
    public String getDisplayName() {
        if (this.connectionProfileName != null) {
            return this.displayName + ":" + this.connectionProfileName; //$NON-NLS-1$
        }
        return this.displayName;
    }

    @Override
    public String getName() {
        return this.dataSourceName;
    }

    /**
     * Returns the data source type name
     * 
     * @return the type
     */
    @Override
    public String getType() {
        return this.dataSourceType;
    }

    @Override
    public Properties getProperties() {
        return this.properties;
    }

    @Override
    public String getPropertyValue(String name) {
        return this.properties.getProperty(name);
    }

    @Override
    public void setProfileName(String name) {
        this.connectionProfileName = name;
    }

    @Override
    public String getProfileName() {
        return this.connectionProfileName;
    }

    /**
     * @return isPreview
     */
    @Override
    public boolean isPreview() {
        return isPreview;
    }

    /**
     * @param isPreview Sets isPreview to the specified value.
     */
    @Override
    public void setPreview(boolean isPreview) {
        this.isPreview = isPreview;
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        int result = 0;
        final int prime = 31;
        result = prime * result + ((this.getName() == null) ? 0 : this.getName().hashCode());
        return result;
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Data Source:\t" + getName()); //$NON-NLS-1$
        if (!getType().equalsIgnoreCase("<unknown>")) { //$NON-NLS-1$
            sb.append("\nType: \t\t" + getType()); //$NON-NLS-1$
        }

        return sb.toString();
    }
}
