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

import java.util.Set;
import org.komodo.spi.annotation.AnnotationUtils;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.BinaryType;
import org.komodo.spi.type.DataTypeManager;
import org.teiid.core.util.ArgCheck;

public class DataTypeManagerImpl implements DataTypeManager {

    private final TeiidVersion version;

    public DataTypeManagerImpl(TeiidVersion version) {
        this.version = version;
    }

    private boolean isArrayType(String name) {
        return name.endsWith(ARRAY_SUFFIX);
    }

    private String getComponentType(String name) {
        return name.substring(0, name.lastIndexOf(ARRAY_SUFFIX));
    }

    /**
     * @return teiid version
     */
    public TeiidVersion getVersion() {
        return version;
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
    public String getDefaultDataType(DataTypeName dataTypeName) {
        ArgCheck.isNotNull(dataTypeName);

        String dataTypeId = org.teiid.core.types.DataTypeManager.getDataTypeName(dataTypeName.getDeclaringClass());

        if (dataTypeName.isArrayType())
            return dataTypeId + ARRAY_SUFFIX;

        return dataTypeId;
    }

    @Override
    public Class<?> getDataTypeClass(String name) {
        return org.teiid.core.types.DataTypeManager.getDataTypeClass(name);
    }

    @Override
    public DataTypeName getDataTypeName(String dataTypeId) {        
        if (dataTypeId == null)
            return DataTypeName.NULL;

        // Should eliminate any aliases
        Class<?> dataTypeClass = org.teiid.core.types.DataTypeManager.getDataTypeClass(dataTypeId);
        dataTypeId = org.teiid.core.types.DataTypeManager.getDataTypeName(dataTypeClass);

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
        return org.teiid.core.types.DataTypeManager.getDataTypeName(typeClass);
    }

    @Override
    public DataTypeName retrieveDataTypeName(Class<?> typeClass) {
        String typeName = getDataTypeName(typeClass);
        return DataTypeName.findDataTypeName(typeName);
    }

    @Override
    public Set<String> getAllDataTypeNames() {
        return org.teiid.core.types.DataTypeManager.getAllDataTypeNames();
    }

    @Override
    public Class<?> getDefaultDataClass(DataTypeName dataTypeName) {
        return getDataTypeClass(dataTypeName.name());
    }

    @Override
    public boolean isExplicitConversion(String sourceTypeName, String targetTypeName) {
        return org.teiid.core.types.DataTypeManager.isExplicitConversion(sourceTypeName, targetTypeName);
    }

    @Override
    public boolean isImplicitConversion(String sourceTypeName, String targetTypeName) {
        return org.teiid.core.types.DataTypeManager.isImplicitConversion(sourceTypeName, targetTypeName);
    }

    @Override
    public boolean isTransformable(String sourceTypeName, String targetTypeName) {
        return org.teiid.core.types.DataTypeManager.isTransformable(sourceTypeName, targetTypeName);
    }

    @Override
    public boolean isLOB(Class<?> type) {
        return org.teiid.core.types.DataTypeManager.isLOB(type);
    }

    @Override
    public <T> T transformValue(Object value, DataTypeName dataTypeName) throws Exception {
        Class<DataTypeName> typeClass = dataTypeName.getDeclaringClass();
        return transformValue(value, typeClass);
    }

    @SuppressWarnings( "unchecked" )
    @Override
    public <T> T transformValue(Object value, Class<?> typeClass) throws Exception {
        return (T) org.teiid.core.types.DataTypeManager.transformValue(value, typeClass);
    }

    @Override
    public Integer getDataTypeLimit(String dataTypeName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public int getDataTypeLimit(DataTypeName dataTypeName) {
        // TODO Auto-generated method stub
        return 0;
    }

    @Override
    public String getDataTypeValidChars(String dataTypeName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public BinaryType createBinaryType(byte[] bytes) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean isDecimalAsDouble() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public DataTypeName getCountType() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public DataTypeName getSumReturnType(DataTypeName sumArgType) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public DataTypeName getAverageReturnType(DataTypeName avgArgType) {
        // TODO Auto-generated method stub
        return null;
    }
}
