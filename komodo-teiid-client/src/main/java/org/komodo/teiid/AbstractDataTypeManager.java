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

import org.komodo.spi.annotation.AnnotationUtils;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.DataTypeManager;

public abstract class AbstractDataTypeManager implements DataTypeManager {

    private final TeiidVersion version;

    public AbstractDataTypeManager(TeiidVersion version) {
        this.version = version;
    }

    /**
     * @return teiid version
     */
    public TeiidVersion getVersion() {
        return version;
    }

    protected boolean isArrayType(String name) {
        return name.endsWith(ARRAY_SUFFIX);
    }

    protected String getComponentType(String name) {
        return name.substring(0, name.lastIndexOf(ARRAY_SUFFIX));
    }

    @Override
    public String getDataSourceType(DataSourceTypes dataSourceType) {
        if (dataSourceType == null)
            return DataSourceTypes.UNKNOWN.id();

        if (! AnnotationUtils.isApplicable(dataSourceType, getVersion()))
            return DataSourceTypes.UNKNOWN.id();

        return AnnotationUtils.getUpdatedName(dataSourceType, dataSourceType.id(), getVersion());
    }

    @Override
    public <T> T transformValue(Object value, DataTypeName dataTypeName) throws Exception {
        Class<DataTypeName> typeClass = dataTypeName.getDeclaringClass();
        return transformValue(value, typeClass);
    }
}
