/*
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
*/
package org.teiid.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.Map.Entry;
import java.util.Set;

import org.junit.Test;
import org.teiid.core.types.BinaryType;
import org.teiid.core.types.DataTypeManagerService;
import org.teiid.core.types.DataTypeManagerService.DefaultDataTypes;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.type.IDataTypeManagerService.DataSourceTypes;
import org.komodo.spi.type.IDataTypeManagerService.DataTypeName;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class Test8DataTypeManagerService extends AbstractTestDataTypeManagerService {

    public Test8DataTypeManagerService() {
        super(Version.TEIID_8_0.get(), Version.TEIID_8_1.get(), Version.TEIID_8_2.get(), Version.TEIID_8_3.get(),
                    Version.TEIID_8_4.get(), Version.TEIID_8_5.get(), Version.TEIID_8_6.get(), Version.TEIID_8_7.get());
    }

    @Test
    public void testGetDefaultDataType1() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            assertSame(DefaultDataTypes.VARBINARY.getId(), dataTypeManager.getDefaultDataType(DataTypeName.VARBINARY));
        }
    }

    @Test
    public void testGetDataType1() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            assertSame(DefaultDataTypes.VARBINARY, dataTypeManager.getDataType("varbinary"));
        }
    }

    @Test
    public void testGetDataTypeClass1() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            assertSame(BinaryType.class, dataTypeManager.getDataTypeClass("varbinary"));
        }
    }

    @Test
    public void testGetDefaultDataTypeClass1() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            assertSame(BinaryType.class, dataTypeManager.getDefaultDataClass(DataTypeName.VARBINARY));
        }
    }

    @Test
    public void testGetDataType4Class1() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            assertSame(DefaultDataTypes.VARBINARY, dataTypeManager.getDataType(BinaryType.class));
        }
    }

    @Test
    public void testGetAllDataTypeNames() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            Set<String> names = dataTypeManager.getAllDataTypeNames();
            assertTrue(!names.isEmpty());
            assertTrue(names.contains(DefaultDataTypes.BIG_DECIMAL.getId()));
            assertTrue(names.contains(DefaultDataTypes.STRING.getId()));
            assertTrue(names.contains(DefaultDataTypes.VARBINARY.getId()));
        }
    }

    @Test
    public void testGetDataSourceType1() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            assertEquals(DataSourceTypes.SALESFORCE.id(), dataTypeManager.getDataSourceType(DataSourceTypes.SALESFORCE));
            assertEquals(DataSourceTypes.LDAP.id(), dataTypeManager.getDataSourceType(DataSourceTypes.LDAP));
            assertEquals(DataSourceTypes.FILE.id(), dataTypeManager.getDataSourceType(DataSourceTypes.FILE));
            assertEquals(DataSourceTypes.WS.id(), dataTypeManager.getDataSourceType(DataSourceTypes.WS));
        }
    }
}
