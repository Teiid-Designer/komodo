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
package org.komodo.relational;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

@SuppressWarnings({ "javadoc", "nls" })
public class ViewDdlBuilderTest {

    @Test
    public void shouldGeneratedODataViewDDL() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView (RowId integer PRIMARY KEY, Column1 ColumnType1, Column2 ColumnType2) AS \n"
        + "SELECT ROW_NUMBER() OVER (ORDER BY Column1), Column1, Column2 \n"
        + "FROM MySrc;";

        String VIEW_NAME = "MyView";
        String SOURCE_NAME = "MySrc";
        List<String> COL_NAMES = new ArrayList<String>();
        COL_NAMES.add("Column1");
        COL_NAMES.add("Column2");
        List<String> COL_TYPES = new ArrayList<String>();
        COL_TYPES.add("ColumnType1");
        COL_TYPES.add("ColumnType2");

        String viewDdl = ViewDdlBuilder.getODataViewDdl(VIEW_NAME, SOURCE_NAME, COL_NAMES, COL_TYPES);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

}
