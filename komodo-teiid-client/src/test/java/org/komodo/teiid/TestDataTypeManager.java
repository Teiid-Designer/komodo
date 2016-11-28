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
 * 021101301 USA.
 */
package org.komodo.teiid;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Set;
import org.junit.Test;
import org.komodo.spi.query.TeiidService;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersionProvider;
import org.komodo.spi.type.DataTypeManager;
import org.komodo.spi.type.DataTypeManager.DataSourceTypes;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class TestDataTypeManager {

    private TeiidVersion version = TeiidVersionProvider.getInstance().getTeiidVersion();

    protected DataTypeManager getDataTypeManager(TeiidVersion version) throws Exception {
        TeiidService teiidService = TeiidServiceProvider.getInstance().getTeiidService(version);
        assertNotNull(teiidService);
        return teiidService.getDataTypeManager();
    }

    @Test
    public void testGetDataType() throws Exception {
        DataTypeManager manager = getDataTypeManager(version);

        assertEquals(DataTypeName.NULL, manager.getDataTypeName((String)null));

        assertEquals(DataTypeName.STRING, manager.getDataTypeName("string"));
        assertEquals(DataTypeName.STRING, manager.getDataTypeName("STRING"));
        assertEquals(DataTypeName.STRING_ARRAY, manager.getDataTypeName("String[]"));

        assertEquals(DataTypeName.BIGDECIMAL, manager.getDataTypeName("bigdecimal"));
        assertEquals(DataTypeName.BIGDECIMAL, manager.getDataTypeName("BIG_DECIMAL"));
        assertEquals(DataTypeName.BIGDECIMAL_ARRAY, manager.getDataTypeName("BIG_DECIMAL[]"));

        assertEquals(DataTypeName.OBJECT, manager.getDataTypeName("NoSuchObject"));
    }

    @Test
    public void testGetDataTypeClass() throws Exception {
        DataTypeManager manager = getDataTypeManager(version);

        assertEquals("NullType", manager.getDataTypeClass((String)null).getSimpleName());

        assertSame(String.class, manager.getDataTypeClass("string"));
        assertSame(String.class, manager.getDataTypeClass("STRING"));
        assertSame(String[].class, manager.getDataTypeClass("String[]"));

        assertSame(Object.class, manager.getDataTypeClass("NoSuchObject"));
    }

    @Test
    public void testGetDefaultDataTypeClass() throws Exception {
        DataTypeManager manager = getDataTypeManager(version);

        assertEquals("NullType", manager.getDefaultDataClass(null).getSimpleName());
        assertSame(String.class, manager.getDefaultDataClass(DataTypeName.STRING));
        assertSame(Object.class, manager.getDefaultDataClass(DataTypeName.OBJECT));
    }

    @Test
    public void testGetDataType4Class() throws Exception {
        DataTypeManager manager = getDataTypeManager(version);

        assertEquals(DataTypeName.LONG.getId(), manager.getDataTypeName(Long.class));
        assertEquals(DataTypeName.STRING.getId(), manager.getDataTypeName(String.class));
    }

    @Test
    public void testGetDataSourceType() throws Exception {
        DataTypeManager manager = getDataTypeManager(version);

        assertSame(DataSourceTypes.JDBC.id(), manager.getDataSourceType(DataSourceTypes.JDBC));
        assertSame(DataSourceTypes.UNKNOWN.id(), manager.getDataSourceType(DataSourceTypes.UNKNOWN));
    }

    @Test
    public void testGetDataType1() throws Exception {
        DataTypeManager manager = getDataTypeManager(version);

        assertSame(DataTypeName.VARBINARY, manager.getDataTypeName("varbinary"));
    }

    @Test
    public void testGetDataTypeClass1() throws Exception {
        DataTypeManager manager = getDataTypeManager(version);

        assertEquals("BinaryType", manager.getDataTypeClass("varbinary").getSimpleName());
    }

    @Test
    public void testGetDefaultDataTypeClass1() throws Exception {
        DataTypeManager manager = getDataTypeManager(version);

        assertEquals("BinaryType", manager.getDefaultDataClass(DataTypeName.VARBINARY).getSimpleName());
    }

    @Test
    public void testGetAllDataTypeNames() throws Exception {
        DataTypeManager manager = getDataTypeManager(version);

        Set<String> names = manager.getAllDataTypeNames();
        assertTrue(!names.isEmpty());
        assertTrue(names.contains(DataTypeName.BIGDECIMAL.getId()));
        assertTrue(names.contains(DataTypeName.STRING.getId()));
        assertTrue(names.contains(DataTypeName.VARBINARY.getId()));
    }

    @Test
    public void testGetDataSourceType1() throws Exception {
        DataTypeManager manager = getDataTypeManager(version);

        assertEquals(DataSourceTypes.SALESFORCE.id(), manager.getDataSourceType(DataSourceTypes.SALESFORCE));
        assertEquals(DataSourceTypes.LDAP.id(), manager.getDataSourceType(DataSourceTypes.LDAP));
        assertEquals(DataSourceTypes.FILE.id(), manager.getDataSourceType(DataSourceTypes.FILE));
        assertEquals(DataSourceTypes.WS.id(), manager.getDataSourceType(DataSourceTypes.WS));
    }
}
