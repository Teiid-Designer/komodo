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

import java.util.Properties;
import org.komodo.spi.runtime.TeiidDataSource;
import org.komodo.spi.runtime.TeiidInstance;
import org.teiid.core.util.ArgCheck;

public class TeiidDataSourceImpl implements Comparable<TeiidDataSourceImpl>, TeiidDataSource {

    private final String name;
    private final Properties properties = new Properties();

    public TeiidDataSourceImpl(String name, Properties properties) {
        ArgCheck.isNotEmpty(name, "name"); //$NON-NLS-1$
        ArgCheck.isNotEmpty(properties, "properties"); //$NON-NLS-1$

        this.name = name;
        for (String propName : properties.stringPropertyNames()) {
            this.properties.setProperty(propName, properties.getProperty(propName));
        }
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(TeiidDataSourceImpl dataSource) {
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
    public String getName() {
        return this.name;
    }

    @Override
    public String getDisplayName() {
        return getPropertyValue(TeiidInstance.DATASOURCE_DISPLAYNAME);
    }

    @Override
    public Properties getProperties() {
        return this.properties;
    }

    @Override
    public String getPropertyValue(String name) {
        return this.properties.getProperty(name);
    }

    /**
     * Returns the data source type name
     * 
     * @return the type
     */
    @Override
    public String getType() {
        return getPropertyValue(TeiidInstance.DATASOURCE_DRIVERNAME);
    }

    /**
     * Returns the data source jndi name
     * 
     * @return the jndi name
     */
    @Override
    public String getJndiName() {
        return getPropertyValue(TeiidInstance.DATASOURCE_JNDINAME);
    }

    /**
     * Returns the data source connection url
     * 
     * @return the connection url
     */
    @Override
    public String getConnectionUrl() {
        return getPropertyValue(TeiidInstance.DATASOURCE_CONNECTION_URL);
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
