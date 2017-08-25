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
        + "FROM MyVDB.MyTable;";

        Table aTable = createTable("MyVDB", VDB_PATH, "MyModel", "MyTable");
        Column col1 = aTable.addColumn(getTransaction(), "Col1");
        col1.setDatatypeName(getTransaction(), "string");
        Column col2 = aTable.addColumn(getTransaction(), "Col2");
        col2.setDatatypeName(getTransaction(), "string");
        
        List<String> colNames = new ArrayList<String>();
        colNames.add("Col1");
        colNames.add("Col2");
        
        String viewDdl = ViewDdlBuilder.getODataViewDdl(getTransaction(), "MyView", aTable, colNames);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

    @Test
    public void shouldGeneratedODataViewDDLFromTableWithPK() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView ( Col1 string, Col2 string, CONSTRAINT pk PRIMARY KEY (Col1)) AS \n"
        + "SELECT  Col1, Col2 \n"
        + "FROM MyVDB.MyTable;";

        Table aTable = createTable("MyVDB", VDB_PATH, "MyModel", "MyTable");
        Column col1 = aTable.addColumn(getTransaction(), "Col1");
        col1.setDatatypeName(getTransaction(), "string");
        Column col2 = aTable.addColumn(getTransaction(), "Col2");
        col2.setDatatypeName(getTransaction(), "string");
        PrimaryKey pk = aTable.setPrimaryKey(getTransaction(), "pk");
        pk.addColumn(getTransaction(), col1);
        commit();
        
        List<String> colNames = new ArrayList<String>();
        colNames.add("Col1");
        colNames.add("Col2");
        
        String viewDdl = ViewDdlBuilder.getODataViewDdl(getTransaction(), "MyView", aTable, colNames);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

    @Test
    public void shouldGeneratedODataViewDDLFromTableWithUC() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView ( Col1 string, Col2 string, CONSTRAINT uc UNIQUE (Col1)) AS \n"
        + "SELECT  Col1, Col2 \n"
        + "FROM MyVDB.MyTable;";

        Table aTable = createTable("MyVDB", VDB_PATH, "MyModel", "MyTable");
        Column col1 = aTable.addColumn(getTransaction(), "Col1");
        col1.setDatatypeName(getTransaction(), "string");
        Column col2 = aTable.addColumn(getTransaction(), "Col2");
        col2.setDatatypeName(getTransaction(), "string");
        UniqueConstraint uc = aTable.addUniqueConstraint(getTransaction(), "uc");
        uc.addColumn(getTransaction(), col1);
        commit();
        
        List<String> colNames = new ArrayList<String>();
        colNames.add("Col1");
        colNames.add("Col2");
        
        String viewDdl = ViewDdlBuilder.getODataViewDdl(getTransaction(), "MyView", aTable, colNames);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

    @Test
    public void shouldGeneratedODataViewDDLFromTableWithMultipleUC() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView ( Col1 string, Col2 string, Col3 long, CONSTRAINT uc1 UNIQUE (Col1),CONSTRAINT uc2 UNIQUE (Col2, Col3)) AS \n"
        + "SELECT  Col1, Col2, Col3 \n"
        + "FROM MyVDB.MyTable;";

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
        
        List<String> colNames = new ArrayList<String>();
        colNames.add("Col1");
        colNames.add("Col2");
        colNames.add("Col3");
        
        String viewDdl = ViewDdlBuilder.getODataViewDdl(getTransaction(), "MyView", aTable, colNames);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGeneratedODataViewInnerJoinDDL() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView (RowId integer PRIMARY KEY,  LHCol1 string, LHCol2 string,  RHCol1 string, RHCol2 string) AS \n"
        + "SELECT ROW_NUMBER() OVER (ORDER BY A.LHCol1), A.LHCol1, A.LHCol2, B.RHCol1, B.RHCol2 \n"
        + "FROM \n"
        + "MyVDB.lhTable AS A \n"
        + "INNER JOIN \n"
        + "MyVDB.rhTable AS B \n"
        + "ON \n"
        + "A.LHCol1 = B.RHCol2;";

        String lhTableAlias = "A";
        String rhTableAlias = "B";
        
        Table lhTable = createTable("MyVDB", VDB_PATH, "MyModel", "lhTable");
        Column lhCol1 = lhTable.addColumn(getTransaction(), "LHCol1");
        lhCol1.setDatatypeName(getTransaction(), "string");
        Column lhCol2 = lhTable.addColumn(getTransaction(), "LHCol2");
        lhCol2.setDatatypeName(getTransaction(), "string");
        
        Table rhTable = createTable("MyVDB", VDB_PATH, "MyModel", "rhTable");
        Column rhCol1 = rhTable.addColumn(getTransaction(), "RHCol1");
        rhCol1.setDatatypeName(getTransaction(), "string");
        Column rhCol2 = rhTable.addColumn(getTransaction(), "RHCol2");
        rhCol2.setDatatypeName(getTransaction(), "string");
                
        List<String> lhColNames = new ArrayList<String>();
        lhColNames.add("LHCol1");
        lhColNames.add("LHCol2");
        
        List<String> rhColNames = new ArrayList<String>();
        rhColNames.add("RHCol1");
        rhColNames.add("RHCol2");
        
        String lhCriteriaCol = "LHCol1";
        String rhCriteriaCol = "RHCol2";
        
        List<ViewBuilderCriteriaPredicate> criteriaPredicates = new ArrayList<ViewBuilderCriteriaPredicate>();
        ViewBuilderCriteriaPredicate predicate = new ViewBuilderCriteriaPredicate();
        predicate.setLhColumn(lhCriteriaCol);
        predicate.setRhColumn(rhCriteriaCol);
        predicate.setOperator("=");
        criteriaPredicates.add(predicate);
        String viewDdl = ViewDdlBuilder.getODataViewJoinDdl(getTransaction(), "MyView", 
                                                            lhTable, lhTableAlias, lhColNames, 
                                                            rhTable, rhTableAlias, rhColNames, 
                                                            ViewDdlBuilder.JOIN_INNER, criteriaPredicates);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

    @Test
    public void shouldGeneratedODataViewLeftOuterJoinDDL() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView (RowId integer PRIMARY KEY,  LHCol1 string, LHCol2 string,  RHCol1 string, RHCol2 string) AS \n"
        + "SELECT ROW_NUMBER() OVER (ORDER BY A.LHCol1), A.LHCol1, A.LHCol2, B.RHCol1, B.RHCol2 \n"
        + "FROM \n"
        + "MyVDB.lhTable AS A \n"
        + "LEFT OUTER JOIN \n"
        + "MyVDB.rhTable AS B \n"
        + "ON \n"
        + "A.LHCol1 > B.RHCol2;";
        
        String lhTableAlias = "A";
        String rhTableAlias = "B";
        
        Table lhTable = createTable("MyVDB", VDB_PATH, "MyModel", "lhTable");
        Column lhCol1 = lhTable.addColumn(getTransaction(), "LHCol1");
        lhCol1.setDatatypeName(getTransaction(), "string");
        Column lhCol2 = lhTable.addColumn(getTransaction(), "LHCol2");
        lhCol2.setDatatypeName(getTransaction(), "string");
        
        Table rhTable = createTable("MyVDB", VDB_PATH, "MyModel", "rhTable");
        Column rhCol1 = rhTable.addColumn(getTransaction(), "RHCol1");
        rhCol1.setDatatypeName(getTransaction(), "string");
        Column rhCol2 = rhTable.addColumn(getTransaction(), "RHCol2");
        rhCol2.setDatatypeName(getTransaction(), "string");
                
        List<String> lhColNames = new ArrayList<String>();
        lhColNames.add("LHCol1");
        lhColNames.add("LHCol2");
        
        List<String> rhColNames = new ArrayList<String>();
        rhColNames.add("RHCol1");
        rhColNames.add("RHCol2");
        
        String lhCriteriaCol = "LHCol1";
        String rhCriteriaCol = "RHCol2";
        
        List<ViewBuilderCriteriaPredicate> criteriaPredicates = new ArrayList<ViewBuilderCriteriaPredicate>();
        ViewBuilderCriteriaPredicate predicate = new ViewBuilderCriteriaPredicate();
        predicate.setLhColumn(lhCriteriaCol);
        predicate.setRhColumn(rhCriteriaCol);
        predicate.setOperator(">");
        criteriaPredicates.add(predicate);
        String viewDdl = ViewDdlBuilder.getODataViewJoinDdl(getTransaction(), "MyView", 
                                                            lhTable, lhTableAlias, lhColNames, 
                                                            rhTable, rhTableAlias, rhColNames, 
                                                            ViewDdlBuilder.JOIN_LEFT_OUTER, criteriaPredicates);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

    @Test
    public void shouldGeneratedODataViewRightOuterJoinDDL() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView (RowId integer PRIMARY KEY,  LHCol1 string, LHCol2 string,  RHCol1 string, RHCol2 string) AS \n"
        + "SELECT ROW_NUMBER() OVER (ORDER BY A.LHCol1), A.LHCol1, A.LHCol2, B.RHCol1, B.RHCol2 \n"
        + "FROM \n"
        + "MyVDB.lhTable AS A \n"
        + "RIGHT OUTER JOIN \n"
        + "MyVDB.rhTable AS B \n"
        + "ON \n"
        + "A.LHCol1 < B.RHCol2;";
        
        String lhTableAlias = "A";
        String rhTableAlias = "B";
        
        Table lhTable = createTable("MyVDB", VDB_PATH, "MyModel", "lhTable");
        Column lhCol1 = lhTable.addColumn(getTransaction(), "LHCol1");
        lhCol1.setDatatypeName(getTransaction(), "string");
        Column lhCol2 = lhTable.addColumn(getTransaction(), "LHCol2");
        lhCol2.setDatatypeName(getTransaction(), "string");
        
        Table rhTable = createTable("MyVDB", VDB_PATH, "MyModel", "rhTable");
        Column rhCol1 = rhTable.addColumn(getTransaction(), "RHCol1");
        rhCol1.setDatatypeName(getTransaction(), "string");
        Column rhCol2 = rhTable.addColumn(getTransaction(), "RHCol2");
        rhCol2.setDatatypeName(getTransaction(), "string");
                
        List<String> lhColNames = new ArrayList<String>();
        lhColNames.add("LHCol1");
        lhColNames.add("LHCol2");
        
        List<String> rhColNames = new ArrayList<String>();
        rhColNames.add("RHCol1");
        rhColNames.add("RHCol2");
        
        String lhCriteriaCol = "LHCol1";
        String rhCriteriaCol = "RHCol2";
        
        List<ViewBuilderCriteriaPredicate> criteriaPredicates = new ArrayList<ViewBuilderCriteriaPredicate>();
        ViewBuilderCriteriaPredicate predicate = new ViewBuilderCriteriaPredicate();
        predicate.setLhColumn(lhCriteriaCol);
        predicate.setRhColumn(rhCriteriaCol);
        predicate.setOperator("<");
        criteriaPredicates.add(predicate);
        String viewDdl = ViewDdlBuilder.getODataViewJoinDdl(getTransaction(), "MyView", 
                                                            lhTable, lhTableAlias, lhColNames, 
                                                            rhTable, rhTableAlias, rhColNames, 
                                                            ViewDdlBuilder.JOIN_RIGHT_OUTER, criteriaPredicates);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

    @Test
    public void shouldGeneratedODataViewFullOuterJoinDDL() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView (RowId integer PRIMARY KEY,  LHCol1 string, LHCol2 string,  RHCol1 string, RHCol2 string) AS \n"
        + "SELECT ROW_NUMBER() OVER (ORDER BY A.LHCol1), A.LHCol1, A.LHCol2, B.RHCol1, B.RHCol2 \n"
        + "FROM \n"
        + "MyVDB.lhTable AS A \n"
        + "FULL OUTER JOIN \n"
        + "MyVDB.rhTable AS B \n"
        + "ON \n"
        + "A.LHCol1 <= B.RHCol2;";
        
        String lhTableAlias = "A";
        String rhTableAlias = "B";
        
        Table lhTable = createTable("MyVDB", VDB_PATH, "MyModel", "lhTable");
        Column lhCol1 = lhTable.addColumn(getTransaction(), "LHCol1");
        lhCol1.setDatatypeName(getTransaction(), "string");
        Column lhCol2 = lhTable.addColumn(getTransaction(), "LHCol2");
        lhCol2.setDatatypeName(getTransaction(), "string");
        
        Table rhTable = createTable("MyVDB", VDB_PATH, "MyModel", "rhTable");
        Column rhCol1 = rhTable.addColumn(getTransaction(), "RHCol1");
        rhCol1.setDatatypeName(getTransaction(), "string");
        Column rhCol2 = rhTable.addColumn(getTransaction(), "RHCol2");
        rhCol2.setDatatypeName(getTransaction(), "string");
                
        List<String> lhColNames = new ArrayList<String>();
        lhColNames.add("LHCol1");
        lhColNames.add("LHCol2");
        
        List<String> rhColNames = new ArrayList<String>();
        rhColNames.add("RHCol1");
        rhColNames.add("RHCol2");
        
        String lhCriteriaCol = "LHCol1";
        String rhCriteriaCol = "RHCol2";
        
        List<ViewBuilderCriteriaPredicate> criteriaPredicates = new ArrayList<ViewBuilderCriteriaPredicate>();
        ViewBuilderCriteriaPredicate predicate = new ViewBuilderCriteriaPredicate();
        predicate.setLhColumn(lhCriteriaCol);
        predicate.setRhColumn(rhCriteriaCol);
        predicate.setOperator("<=");
        criteriaPredicates.add(predicate);
        String viewDdl = ViewDdlBuilder.getODataViewJoinDdl(getTransaction(), "MyView", 
                                                            lhTable, lhTableAlias, lhColNames, 
                                                            rhTable, rhTableAlias, rhColNames, 
                                                            ViewDdlBuilder.JOIN_FULL_OUTER, criteriaPredicates);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGeneratedODataViewJoinNoCriteriaDDL() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView (RowId integer PRIMARY KEY,  LHCol1 string, LHCol2 string,  RHCol1 string, RHCol2 string) AS \n"
        + "SELECT ROW_NUMBER() OVER (ORDER BY A.LHCol1), A.LHCol1, A.LHCol2, B.RHCol1, B.RHCol2 \n"
        + "FROM \n"
        + "MyVDB.lhTable AS A \n"
        + "INNER JOIN \n"
        + "MyVDB.rhTable AS B ;";

        String lhTableAlias = "A";
        String rhTableAlias = "B";
        
        Table lhTable = createTable("MyVDB", VDB_PATH, "MyModel", "lhTable");
        Column lhCol1 = lhTable.addColumn(getTransaction(), "LHCol1");
        lhCol1.setDatatypeName(getTransaction(), "string");
        Column lhCol2 = lhTable.addColumn(getTransaction(), "LHCol2");
        lhCol2.setDatatypeName(getTransaction(), "string");
        
        Table rhTable = createTable("MyVDB", VDB_PATH, "MyModel", "rhTable");
        Column rhCol1 = rhTable.addColumn(getTransaction(), "RHCol1");
        rhCol1.setDatatypeName(getTransaction(), "string");
        Column rhCol2 = rhTable.addColumn(getTransaction(), "RHCol2");
        rhCol2.setDatatypeName(getTransaction(), "string");
                
        List<String> lhColNames = new ArrayList<String>();
        lhColNames.add("LHCol1");
        lhColNames.add("LHCol2");
        
        List<String> rhColNames = new ArrayList<String>();
        rhColNames.add("RHCol1");
        rhColNames.add("RHCol2");
        
        String viewDdl = ViewDdlBuilder.getODataViewJoinDdl(getTransaction(), "MyView", 
                                                            lhTable, lhTableAlias, lhColNames, 
                                                            rhTable, rhTableAlias, rhColNames, 
                                                            ViewDdlBuilder.JOIN_INNER, null);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }
    
    @Test
    public void shouldGeneratedODataViewInnerJoinMulipleCriteriaDDL() throws Exception {
        String EXPECTED_DDL = "CREATE VIEW MyView (RowId integer PRIMARY KEY,  LHCol1 string, LHCol2 string,  RHCol1 string, RHCol2 string) AS \n"
        + "SELECT ROW_NUMBER() OVER (ORDER BY A.LHCol1), A.LHCol1, A.LHCol2, B.RHCol1, B.RHCol2 \n"
        + "FROM \n"
        + "MyVDB.lhTable AS A \n"
        + "INNER JOIN \n"
        + "MyVDB.rhTable AS B \n"
        + "ON \n"
        + "A.LHCol1 = B.RHCol2 AND A.LHCol3 > B.RHCol4;";

        String lhTableAlias = "A";
        String rhTableAlias = "B";
        
        Table lhTable = createTable("MyVDB", VDB_PATH, "MyModel", "lhTable");
        Column lhCol1 = lhTable.addColumn(getTransaction(), "LHCol1");
        lhCol1.setDatatypeName(getTransaction(), "string");
        Column lhCol2 = lhTable.addColumn(getTransaction(), "LHCol2");
        lhCol2.setDatatypeName(getTransaction(), "string");
        
        Table rhTable = createTable("MyVDB", VDB_PATH, "MyModel", "rhTable");
        Column rhCol1 = rhTable.addColumn(getTransaction(), "RHCol1");
        rhCol1.setDatatypeName(getTransaction(), "string");
        Column rhCol2 = rhTable.addColumn(getTransaction(), "RHCol2");
        rhCol2.setDatatypeName(getTransaction(), "string");
                
        List<String> lhColNames = new ArrayList<String>();
        lhColNames.add("LHCol1");
        lhColNames.add("LHCol2");
        
        List<String> rhColNames = new ArrayList<String>();
        rhColNames.add("RHCol1");
        rhColNames.add("RHCol2");
        
        List<ViewBuilderCriteriaPredicate> criteriaPredicates = new ArrayList<ViewBuilderCriteriaPredicate>();
        ViewBuilderCriteriaPredicate predicate1 = new ViewBuilderCriteriaPredicate();
        predicate1.setLhColumn("LHCol1");
        predicate1.setRhColumn("RHCol2");
        predicate1.setOperator("=");
        predicate1.setCombineKeyword("AND");
        criteriaPredicates.add(predicate1);
        ViewBuilderCriteriaPredicate predicate2 = new ViewBuilderCriteriaPredicate();
        predicate2.setLhColumn("LHCol3");
        predicate2.setRhColumn("RHCol4");
        predicate2.setOperator(">");
        criteriaPredicates.add(predicate2);
        String viewDdl = ViewDdlBuilder.getODataViewJoinDdl(getTransaction(), "MyView", 
                                                            lhTable, lhTableAlias, lhColNames, 
                                                            rhTable, rhTableAlias, rhColNames, 
                                                            ViewDdlBuilder.JOIN_INNER, criteriaPredicates);
        assertThat(viewDdl, is(EXPECTED_DDL));
    }

}
