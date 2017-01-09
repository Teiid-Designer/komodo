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

import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import org.komodo.spi.runtime.TeiidTranslator;
import org.komodo.teiid.Messages;
import org.teiid.adminapi.AdminObject;
import org.teiid.adminapi.Translator;
import org.teiid.core.util.ArgCheck;

public class TeiidTranslatorImpl implements Comparable<TeiidTranslatorImpl>, TeiidTranslator {

    private String name;

    private String type;

    private String description;

    private Properties properties = new Properties();

    /**
     * @param translator
     * @param propDefs
     * @param teiidInstance
     */
    public TeiidTranslatorImpl(Translator translator) {
        ArgCheck.isNotNull(translator, "translator"); //$NON-NLS-1$

        this.name = translator.getName();
        this.type = translator.getType();
        this.description = translator.getDescription();

        setProperties(translator.getProperties());
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(TeiidTranslatorImpl translator) {
        ArgCheck.isNotNull(translator, "translator"); //$NON-NLS-1$
        return getName().compareTo(translator.getName());
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        TeiidTranslatorImpl other = (TeiidTranslatorImpl)obj;
        if (description == null) {
            if (other.description != null)
                return false;
        } else if (!description.equals(other.description))
            return false;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        if (properties == null) {
            if (other.properties != null)
                return false;
        } else if (!properties.equals(other.properties))
            return false;
        if (type == null) {
            if (other.type != null)
                return false;
        } else if (!type.equals(other.type))
            return false;
        return true;
    }

    /**
     * {@inheritDoc}
     * 
     * @see AdminObject#getName()
     */
    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getDescription() {
        return description;
    }

    /**
     * {@inheritDoc}
     * 
     * @see AdminObject#getProperties()
     */
    @Override
    public Properties getProperties() {
        return properties;
    }

    /**
     * @return type
     */
    @Override
    public String getType() {
        return type;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((description == null) ? 0 : description.hashCode());
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        result = prime * result + ((properties == null) ? 0 : properties.hashCode());
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        return result;
    }

    /**
     * @param changedProperties the list of properties that are being changed (never <code>null</code> or empty)
     * @throws Exception if there is a problem changing the properties
     *
     */
    @Override
    public void setProperties(Properties changedProperties) {
        ArgCheck.isNotNull(changedProperties, "changedProperties"); //$NON-NLS-1$
        Set<Entry<Object, Object>> entrySet = changedProperties.entrySet();
        ArgCheck.isNotEmpty(entrySet, "changedProperties"); //$NON-NLS-1$

        for (Entry<Object, Object> entry : entrySet) {
            properties.setProperty((String)entry.getKey(), (String)entry.getValue());
        }
    }

    /**
     * {@inheritDoc}
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return Messages.getString(Messages.ExecutionAdmin.connectorDetailedName, getName(), getType());
    }
}
