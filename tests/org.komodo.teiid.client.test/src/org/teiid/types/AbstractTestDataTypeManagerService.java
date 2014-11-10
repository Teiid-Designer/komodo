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

import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.fail;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import org.junit.Test;
import org.teiid.core.types.DataTypeManagerService;
import org.teiid.core.types.DataTypeManagerService.DefaultDataTypes;
import org.teiid.core.types.NullType;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion;
import org.komodo.spi.type.IDataTypeManagerService.DataSourceTypes;
import org.komodo.spi.type.IDataTypeManagerService.DataTypeName;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractTestDataTypeManagerService {

    protected final Map<TeiidVersion, DataTypeManagerService> dataTypeManagerCache = new HashMap<TeiidVersion, DataTypeManagerService>();

    /**
     * @param teiidVersion
     */
    public AbstractTestDataTypeManagerService(TeiidVersion... Version) {
        for (TeiidVersion teiidVersion : Version) {
            dataTypeManagerCache.put(teiidVersion, DataTypeManagerService.getInstance(teiidVersion));
        }
    }

    @Test
    public void testCachedInstance() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            TeiidVersion teiidVersion = entry.getKey();
            DataTypeManagerService dataTypeManager = entry.getValue();

            assertSame(dataTypeManager, DataTypeManagerService.getInstance(teiidVersion));

            // Using old teiid version as unlikely ever to match
            assertNotSame(dataTypeManager, DataTypeManagerService.getInstance(new DefaultTeiidVersion("6.0.0"))); //$NON-NLS-1$
        }
    }

    @Test
    public void testGetDefaultDataType() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            // Test for null
            try {
                dataTypeManager.getDefaultDataType(null);
                fail("Should not allow null data type name");
            } catch (IllegalArgumentException ex) {
                // should throw an exception
            }

            // Top of the list
            assertSame(DefaultDataTypes.BIG_DECIMAL.getId(), dataTypeManager.getDefaultDataType(DataTypeName.BIG_DECIMAL));

            // Middle of the list
            assertSame(DefaultDataTypes.BLOB.getId(), dataTypeManager.getDefaultDataType(DataTypeName.BLOB));
        }
    }

    @Test
    public void testGetDataType() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            assertSame(DefaultDataTypes.NULL, dataTypeManager.getDataType((String)null));

            assertSame(DefaultDataTypes.STRING, dataTypeManager.getDataType("string"));
            assertSame(DefaultDataTypes.STRING, dataTypeManager.getDataType("STRING"));
            assertSame(DefaultDataTypes.STRING, dataTypeManager.getDataType("String[]"));

            assertSame(DefaultDataTypes.OBJECT, dataTypeManager.getDataType("NoSuchObject"));
        }
    }

    @Test
    public void testGetDataTypeClass() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            assertSame(NullType.class, dataTypeManager.getDataTypeClass((String)null));

            assertSame(String.class, dataTypeManager.getDataTypeClass("string"));
            assertSame(String.class, dataTypeManager.getDataTypeClass("STRING"));
            assertSame(String[].class, dataTypeManager.getDataTypeClass("String[]"));

            assertSame(Object.class, dataTypeManager.getDataTypeClass("NoSuchObject"));
        }
    }

    @Test
    public void testGetDefaultDataTypeClass() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            try {
                dataTypeManager.getDefaultDataClass(null);
                fail("Should not allow null data type name");
            } catch (IllegalArgumentException ex) {
                // should throw an exception
            }

            assertSame(String.class, dataTypeManager.getDefaultDataClass(DataTypeName.STRING));
            assertSame(Object.class, dataTypeManager.getDefaultDataClass(DataTypeName.OBJECT));
        }
    }

    @Test
    public void testGetDataType4Class() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            try {
                dataTypeManager.getDataType((Class<?>)null);
                fail("Should not allow null class parameter");
            } catch (IllegalArgumentException ex) {
                // should throw an exception
            }

            assertSame(DefaultDataTypes.LONG, dataTypeManager.getDataType(Long.class));
            assertSame(DefaultDataTypes.STRING, dataTypeManager.getDataType(String.class));
        }
    }

    @Test
    public void testGetDataSourceType() {
        for (Entry<TeiidVersion, DataTypeManagerService> entry : dataTypeManagerCache.entrySet()) {
            DataTypeManagerService dataTypeManager = entry.getValue();

            assertSame(DataSourceTypes.JDBC.id(), dataTypeManager.getDataSourceType(DataSourceTypes.JDBC));
            assertSame(DataSourceTypes.UNKNOWN.id(), dataTypeManager.getDataSourceType(DataSourceTypes.UNKNOWN));
        }
    }
}
