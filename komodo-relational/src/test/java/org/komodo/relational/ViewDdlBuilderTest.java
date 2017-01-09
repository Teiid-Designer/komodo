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
import org.junit.Test;
import org.komodo.relational.model.Column;
import org.komodo.relational.model.PrimaryKey;
import org.komodo.relational.model.Table;
import org.komodo.relational.model.UniqueConstraint;

@SuppressWarnings({ "javadoc", "nls" })
public class ViewDdlBuilderTest extends RelationalModelTest {

    @Test
    public void shouldGeneratedODataViewDDLFromTableWithNoPK() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView (RowId integer PRIMARY KEY, Col1 string, Col2 string) AS \n"
        + "SELECT ROW_NUMBER() OVER (ORDER BY Col1), Col1, Col2 \n"
        + "FROM MyTable;";

        Table aTable = createTable("MyVDB", VDB_PATH, "MyModel", "MyTable");
        Column col1 = aTable.addColumn(getTransaction(), "Col1");
        col1.setDatatypeName(getTransaction(), "string");
        Column col2 = aTable.addColumn(getTransaction(), "Col2");
        col2.setDatatypeName(getTransaction(), "string");
        
        String viewDdl = ViewDdlBuilder.getODataViewDdl(getTransaction(), "MyView", aTable);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

    @Test
    public void shouldGeneratedODataViewDDLFromTableWithPK() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView ( Col1 string, Col2 string, CONSTRAINT pk PRIMARY KEY (Col1)) AS \n"
        + "SELECT  Col1, Col2 \n"
        + "FROM MyTable;";

        Table aTable = createTable("MyVDB", VDB_PATH, "MyModel", "MyTable");
        Column col1 = aTable.addColumn(getTransaction(), "Col1");
        col1.setDatatypeName(getTransaction(), "string");
        Column col2 = aTable.addColumn(getTransaction(), "Col2");
        col2.setDatatypeName(getTransaction(), "string");
        PrimaryKey pk = aTable.setPrimaryKey(getTransaction(), "pk");
        pk.addColumn(getTransaction(), col1);
        commit();
        
        String viewDdl = ViewDdlBuilder.getODataViewDdl(getTransaction(), "MyView", aTable);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

    @Test
    public void shouldGeneratedODataViewDDLFromTableWithUC() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView ( Col1 string, Col2 string, CONSTRAINT uc UNIQUE (Col1)) AS \n"
        + "SELECT  Col1, Col2 \n"
        + "FROM MyTable;";

        Table aTable = createTable("MyVDB", VDB_PATH, "MyModel", "MyTable");
        Column col1 = aTable.addColumn(getTransaction(), "Col1");
        col1.setDatatypeName(getTransaction(), "string");
        Column col2 = aTable.addColumn(getTransaction(), "Col2");
        col2.setDatatypeName(getTransaction(), "string");
        UniqueConstraint uc = aTable.addUniqueConstraint(getTransaction(), "uc");
        uc.addColumn(getTransaction(), col1);
        commit();
        
        String viewDdl = ViewDdlBuilder.getODataViewDdl(getTransaction(), "MyView", aTable);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

    @Test
    public void shouldGeneratedODataViewDDLFromTableWithMultipleUC() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView ( Col1 string, Col2 string, Col3 long, CONSTRAINT uc1 UNIQUE (Col1),CONSTRAINT uc2 UNIQUE (Col2, Col3)) AS \n"
        + "SELECT  Col1, Col2, Col3 \n"
        + "FROM MyTable;";

        Table aTable = createTable("MyVDB", VDB_PATH, "MyModel", "MyTable");
        Column col1 = aTable.addColumn(getTransaction(), "Col1");
        col1.setDatatypeName(getTransaction(), "string");
        Column col2 = aTable.addColumn(getTransaction(), "Col2");
        col2.setDatatypeName(getTransaction(), "string");
        Column col3 = aTable.addColumn(getTransaction(), "Col3");
        col3.setDatatypeName(getTransaction(), "long");
        UniqueConstraint uc1 = aTable.addUniqueConstraint(getTransaction(), "uc1");
        uc1.addColumn(getTransaction(), col1);
        UniqueConstraint uc2 = aTable.addUniqueConstraint(getTransaction(), "uc2");
        uc2.addColumn(getTransaction(), col2);
        uc2.addColumn(getTransaction(), col3);
        commit();
        
        String viewDdl = ViewDdlBuilder.getODataViewDdl(getTransaction(), "MyView", aTable);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

}
