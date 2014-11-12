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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Map.Entry;
import java.util.Set;

import org.junit.Test;
import org.teiid.core.types.BinaryTypeImpl;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.core.types.DefaultDataTypeManager.DefaultDataTypes;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.type.DataTypeManager.DataSourceTypes;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class Test7DataTypeManagerService extends AbstractTestDataTypeManagerService {

    public Test7DataTypeManagerService() {
        super(Version.TEIID_7_7.get());
    }

    @Test
    public void testGetDefaultDataType1() {
        for (Entry<TeiidVersion, DefaultDataTypeManager> entry : dataTypeManagerCache.entrySet()) {
            DefaultDataTypeManager dataTypeManager = entry.getValue();

            try {
                dataTypeManager.getDefaultDataType(DataTypeName.VARBINARY);
                fail("VARBINARY should be not applicable");
            } catch (IllegalArgumentException ex) {
                // pass
            }
        }
    }

    @Test
    public void testGetDataType1() {
        for (Entry<TeiidVersion, DefaultDataTypeManager> entry : dataTypeManagerCache.entrySet()) {
            DefaultDataTypeManager dataTypeManager = entry.getValue();

            assertSame(DefaultDataTypes.OBJECT, dataTypeManager.getDataType("varbinary"));
        }
    }

    @Test
    public void testGetDataTypeClass1() {
        for (Entry<TeiidVersion, DefaultDataTypeManager> entry : dataTypeManagerCache.entrySet()) {
            DefaultDataTypeManager dataTypeManager = entry.getValue();

            assertSame(Object.class, dataTypeManager.getDataTypeClass("varbinary"));
        }
    }

    @Test
    public void testGetDefaultDataTypeClass1() {
        for (Entry<TeiidVersion, DefaultDataTypeManager> entry : dataTypeManagerCache.entrySet()) {
            DefaultDataTypeManager dataTypeManager = entry.getValue();

            try {
                dataTypeManager.getDefaultDataClass(DataTypeName.VARBINARY);
                fail("VARBINARY should be not applicable");
            } catch (IllegalArgumentException ex) {
                // should throw an exception
            }
        }
    }

    @Test
    public void testGetDataType4Class1() {
        for (Entry<TeiidVersion, DefaultDataTypeManager> entry : dataTypeManagerCache.entrySet()) {
            DefaultDataTypeManager dataTypeManager = entry.getValue();

            assertSame(DefaultDataTypes.OBJECT, dataTypeManager.getDataType(BinaryTypeImpl.class));
        }
    }

    @Test
    public void testGetAllDataTypeNames() {
        for (Entry<TeiidVersion, DefaultDataTypeManager> entry : dataTypeManagerCache.entrySet()) {
            DefaultDataTypeManager dataTypeManager = entry.getValue();

            Set<String> names = dataTypeManager.getAllDataTypeNames();
            assertTrue(!names.isEmpty());
            assertTrue(names.contains(DefaultDataTypes.BIG_DECIMAL.getId()));
            assertTrue(names.contains(DefaultDataTypes.STRING.getId()));
            assertFalse(names.contains(DefaultDataTypes.VARBINARY.getId()));
        }
    }

    @Test
    public void testGetDataSourceType1() {
        for (Entry<TeiidVersion, DefaultDataTypeManager> entry : dataTypeManagerCache.entrySet()) {
            DefaultDataTypeManager dataTypeManager = entry.getValue();

            // Should retrieve the old value from the Updated annotation
            assertEquals("connector-salesforce", dataTypeManager.getDataSourceType(DataSourceTypes.SALESFORCE));
            assertEquals("connector-ldap", dataTypeManager.getDataSourceType(DataSourceTypes.LDAP));
            assertEquals("connector-file", dataTypeManager.getDataSourceType(DataSourceTypes.FILE));
            assertEquals("connector-ws", dataTypeManager.getDataSourceType(DataSourceTypes.WS));
        }
    }
}
