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

import java.util.Set;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.teiid.AbstractDataTypeManager;
import org.teiid.core.types.DataTypeManager;

public class DataTypeManagerImpl extends AbstractDataTypeManager {

    public DataTypeManagerImpl(TeiidVersion version) {
        super(version);
    }

    @Override
    public Class<?> getDataTypeClass(String name) {
        return DataTypeManager.getDataTypeClass(name);
    }

    @Override
    public DataTypeName getDataTypeName(String dataTypeId) {        
        if (dataTypeId == null)
            return DataTypeName.NULL;

        dataTypeId = DataTypeName.correctBigUnderscores(dataTypeId);

        // Should eliminate any aliases
        Class<?> dataTypeClass = getDataTypeClass(dataTypeId);
        dataTypeId = DataTypeManager.getDataTypeName(dataTypeClass);

        boolean isArray = isArrayType(dataTypeId);

        if (isArray)
            dataTypeId = getComponentType(dataTypeId);

        DataTypeName dataType = DataTypeName.findDataTypeName(dataTypeId);
        if (dataType == null)
            dataType = DataTypeName.OBJECT;

        if (isArray)
            return dataType.getArrayType();
        else
            return dataType;
    }

    @Override
    public String getDataTypeName(Class<?> typeClass) {
        return DataTypeManager.getDataTypeName(typeClass);
    }

    @Override
    public DataTypeName retrieveDataTypeName(Class<?> typeClass) {
        String typeName = getDataTypeName(typeClass);
        return DataTypeName.findDataTypeName(typeName);
    }

    @Override
    public Set<String> getAllDataTypeNames() {
        return DataTypeManager.getAllDataTypeNames();
    }

    @Override
    public Class<?> getDefaultDataClass(DataTypeName dataTypeName) {
        if (dataTypeName == null)
            return getDataTypeClass(null);

        return getDataTypeClass(dataTypeName.name());
    }

    @Override
    public boolean isExplicitConversion(String sourceTypeName, String targetTypeName) {
        return DataTypeManager.isExplicitConversion(sourceTypeName, targetTypeName);
    }

    @Override
    public boolean isImplicitConversion(String sourceTypeName, String targetTypeName) {
        return DataTypeManager.isImplicitConversion(sourceTypeName, targetTypeName);
    }

    @Override
    public boolean isTransformable(String sourceTypeName, String targetTypeName) {
        return DataTypeManager.isTransformable(sourceTypeName, targetTypeName);
    }

    @Override
    public boolean isLOB(Class<?> type) {
        return DataTypeManager.isLOB(type);
    }

    @SuppressWarnings( "unchecked" )
    @Override
    public <T> T transformValue(Object value, Class<?> typeClass) throws Exception {
        return (T) DataTypeManager.transformValue(value, typeClass);
    }
}
