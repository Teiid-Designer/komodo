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
package org.komodo.repository.search;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.core.KomodoLexicon.Search;
import org.komodo.core.KomodoLexicon.Search.WhereClause;
import org.komodo.repository.RepositoryImpl;
import org.komodo.spi.KException;
import org.komodo.spi.query.LogicalOperator;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository.KeywordCriteria;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.modeshape.jcr.JcrNtLexicon;
import org.modeshape.jcr.api.JcrConstants;

@SuppressWarnings( {"nls", "javadoc"} )
public class TestObjectSearcher extends AbstractLocalRepositoryTest {

    private static final String TEIIDSQL = "TeiidSQL";
    private static final String DDL = "DDL";
    private String userWksp;

    private KomodoObject[] createTestData() throws Exception {
        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        for (int i = 1; i < 6; ++i) {
            KomodoObject child = workspace.addChild(getTransaction(), "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION, DDL);
        }

        KomodoObject[] testNodes = workspace.getChildrenOfType(getTransaction(), KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(5, testNodes.length);
        for (KomodoObject testKO : testNodes) {
            Property property = testKO.getProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertEquals(DDL, property.getStringValue(getTransaction()));
        }

        commit(); // must commit for search queries to work

        return testNodes;
    }

    @Before
    public void setup() {
        this.userWksp = RepositoryImpl.komodoWorkspacePath(getTransaction());
    }

    @Test
    public void shouldHaveRepository() {
        try {
            new ObjectSearcher(null);
            fail("Repository expected");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void noParameters() {
        try {
            ObjectSearcher os = new ObjectSearcher(_repo);
            os.toString(getTransaction());
            fail("ObjectSearcher should require at least 1 From Clause");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }

        try {
            ObjectSearcher os = new ObjectSearcher(_repo);
            os.toString(getTransaction());
            fail("ObjectSearcher should require at least 1 From Clause");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void oneFrom() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [tko:workspace] " +
                                        "WHERE ISDESCENDANTNODE('" + userWksp + "')";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(KomodoLexicon.Komodo.WORKSPACE, null);
        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereClauseWithNullType() {
        try {
            ObjectSearcher os = new ObjectSearcher(_repo);
            os.addWhereSetClause(null, "p1", "property1", (String[]) null);
            os.toString(getTransaction());
            fail("Type cannot be null");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void addWhereClauseWithNullAliasNoFrom() {
        try {
            ObjectSearcher os = new ObjectSearcher(_repo);
            os.addWhereSetClause(null, null, "property1", "value1");
            os.toString(getTransaction());
            fail("Alias cannot be null if there are no from clauses yet!");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void addWhereClauseWithNullAliases() {
        try {
            ObjectSearcher os = new ObjectSearcher(_repo);
            os.setFromType(KomodoLexicon.Komodo.WORKSPACE);
            os.addWhereSetClause(null, null, "property1", "value1");
            os.toString(getTransaction());
        } catch (Exception ex) {
            ex.printStackTrace();
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void addWhereClauseWithNullAliasWithOneFrom() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                        "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                        "p1.[property1] IN ('value1')" +
                                        " )";

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, null, "property1", "value1");

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereClauseWithInvalidAlias() throws Exception {
        try {
            ObjectSearcher os = new ObjectSearcher(_repo);
            os.setFromType(KomodoLexicon.Komodo.WORKSPACE);
            os.addWhereSetClause(null, "p2", "property1", "value1");
            os.toString(getTransaction());
            fail("Alias of where clause must refer to existing alias");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }

        try {
            ObjectSearcher os = new ObjectSearcher(_repo);
            os.setFromType(KomodoLexicon.Komodo.WORKSPACE, "p1");
            os.addWhereSetClause(null, "p2", "property1", "value1");
            os.toString(getTransaction());
            fail("Alias of where clause must refer to existing alias");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void addWhereClauseTwoPropertyValues() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                        "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                        "p1.[property1] IN ('value1', 'value2')" +
                                        " )";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, "p1", "property1", "value1", "value2");

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereClauseWildcardProperty() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                         "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                         "p1.* IN ('value1', 'value2')" +
                                         " )";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, "p1", STAR, "value1", "value2");

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereClauseTwoDifferentProperties() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                         "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                         "p1.[property1] IN ('value1', 'value2') " +
                                         "OR p1.[name] IN ('bob', 'bryan')" +
                                         " )";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, "p1", "property1", "value1", "value2");
        os.addWhereSetClause(LogicalOperator.OR, "p1", "name", "bob", "bryan");

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereClauseOneSetPropertyOneContainsProperty() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                         "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                         "p1.[property1] IN ('value1', 'value2') " +
                                         "OR CONTAINS(p1.[name], 'bob')" +
                                         " )";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, "p1", "property1", "value1", "value2");
        os.addWhereContainsClause(LogicalOperator.OR, "p1", "name", "bob");

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereClauseContainsWildcardProperty() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                         "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                         "p1.* IN ('value1', 'value2')" +
                                         " )";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, "p1", STAR, "value1", "value2");

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereClauseContainsPropertyWithTwoKeywords() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                    "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                     "p1.[property1] IN ('value1', 'value2') " +
                                     "OR CONTAINS(p1.[name], 'bob OR chris')" +
                                     " )";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, "p1", "property1", "value1", "value2");
        os.addWhereContainsClause(LogicalOperator.OR, "p1", "name", KeywordCriteria.ANY, "bob", "chris");

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereClauseComparisonProperty() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                        "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                        "p1.[property1] = 'value1'" +
                                        " )";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereCompareClause(null, "p1", "property1", ComparisonOperator.EQUALS, "value1");

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereClauseComparisonProperty2() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                        "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                        "p1.[property1] LIKE 'value%1'" +
                                        " )";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereCompareClause(null, "p1", "property1", ComparisonOperator.LIKE, "value%1");

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereClauseComparisonWithFunction() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                        "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                        "LOWER(NAME(p1)) LIKE 'value%1'" +
                                        " )";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereCompareClause(null, null, "NAME(p1)", ComparisonOperator.LIKE, "value%1", true);

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void addWhereParanthesisClause() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                        "WHERE ISDESCENDANTNODE('p1', '" + userWksp + "') AND ( " +
                                        "(p1.[property1] LIKE 'value%1' OR p1.[property1] LIKE 'value%2')" +
                                        " )";
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "p1");

        CompareClause clause1 = new CompareClause(null, "p1", "property1", ComparisonOperator.LIKE, "value%1");
        CompareClause clause2 = new CompareClause(null, "p1", "property1", ComparisonOperator.LIKE, "value%2");

        os.addWhereParanthesisClause(null, clause1, clause2);

        assertEquals(expected, os.toString(getTransaction()));
    }

    @Test
    public void executeSingleFromQuery() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        workspace.addChild(getTransaction(), "test1", KomodoLexicon.VdbModel.NODE_TYPE);
        workspace.addChild(getTransaction(), "test2", KomodoLexicon.VdbModel.NODE_TYPE);
        workspace.addChild(getTransaction(), "test3", KomodoLexicon.VdbModel.NODE_TYPE);
        workspace.addChild(getTransaction(), "test4", KomodoLexicon.VdbModel.NODE_TYPE);
        workspace.addChild(getTransaction(), "test5", KomodoLexicon.VdbModel.NODE_TYPE);

        KomodoObject[] testNodes = workspace.getChildrenOfType(getTransaction(), KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(5, testNodes.length);

        commit(); // must commit for search queries to work

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(KomodoLexicon.VdbModel.NODE_TYPE);
        List<KomodoObject> searchObjects = os.searchObjects(getTransaction());
        assertEquals(testNodes.length, searchObjects.size());
    }

    @Test
    public void executeFromQueryWithWhere() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        KomodoObject[] testNodes = createTestData();

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "nt");
        os.addWhereContainsClause(null, "nt", KomodoLexicon.VdbModel.MODEL_DEFINITION, DDL);

        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS nt " +
                                         "WHERE ISDESCENDANTNODE('nt', '" + workspace.getAbsolutePath() + "') AND ( " +
                                         "CONTAINS(nt.[vdb:modelDefinition], 'DDL')" +
                                         " )";
        assertEquals(expected, os.toString(getTransaction()));

        List<KomodoObject> searchObjects = os.searchObjects(getTransaction());
        assertEquals(testNodes.length, searchObjects.size());
        for (KomodoObject searchObject : searchObjects) {
            String name = searchObject.getName(getTransaction());
            assertTrue(name.startsWith("test"));

            Property property = searchObject.getProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertEquals(DDL, property.getStringValue(getTransaction()));
        }
    }

    @Test
    public void executeFromQueryWithParentWhere() throws Exception {
        int sourceTotal = 5;
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        KomodoObject testModel1 = workspace.addChild(getTransaction(), "TestModel1", KomodoLexicon.VdbModel.NODE_TYPE);
        KomodoObject testModel2 = workspace.addChild(getTransaction(), "TestModel2", KomodoLexicon.VdbModel.NODE_TYPE);

        for (int i = 1; i <= sourceTotal; ++i) {
            testModel1.addChild(getTransaction(), "TestModelSource1-" + i, KomodoLexicon.VdbModelSource.NODE_TYPE);
        }

        KomodoObject[] testNodes = testModel1.getChildrenOfType(getTransaction(), KomodoLexicon.VdbModelSource.NODE_TYPE);
        assertEquals(sourceTotal, testNodes.length);

        for (int i = 1; i <= sourceTotal; ++i) {
            testModel2.addChild(getTransaction(), "TestModelSource2-" + i, KomodoLexicon.VdbModelSource.NODE_TYPE);
        }

        testNodes = testModel2.getChildrenOfType(getTransaction(), KomodoLexicon.VdbModelSource.NODE_TYPE);
        assertEquals(sourceTotal, testNodes.length);

        commit(); // must commit for search queries to work

        //
        // Test object searcher for immediate descendents beneath testModel1
        //
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(KomodoLexicon.VdbModelSource.NODE_TYPE);
        os.addWhereParentClause(null, null, testModel1.getAbsolutePath(), false);

        String expected = "SELECT [jcr:path], [mode:localName] FROM " +
                                     OPEN_SQUARE_BRACKET +
                                     KomodoLexicon.VdbModelSource.NODE_TYPE +
                                     CLOSE_SQUARE_BRACKET +
                                     " WHERE ISDESCENDANTNODE('" + userWksp + "') AND ( " +
                                     "[" + JcrConstants.JCR_PATH + "] LIKE '" +
                                     testModel1.getAbsolutePath() + "/%'" +
                                     " )";
        assertEquals(expected, os.toString(getTransaction()));

        List<KomodoObject> searchObjects = os.searchObjects(getTransaction());
        assertEquals(sourceTotal, searchObjects.size());

        for (KomodoObject searchObject : searchObjects) {
            String name = searchObject.getName(getTransaction());
            assertTrue(name.startsWith("TestModelSource1-"));
        }

        //
        // Find all the model source nodes beneath the workspace
        //
        os = new ObjectSearcher(_repo);
        os.setFromType(KomodoLexicon.VdbModelSource.NODE_TYPE);
        os.addWhereParentClause(null, null, workspace.getAbsolutePath(), false);
        searchObjects = os.searchObjects(getTransaction());
        assertEquals(sourceTotal * 2, searchObjects.size());
    }

    @Test
    public void executeFromQueryWithParentWhereDirectChildren() throws Exception {
        int sourceTotal = 5;
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        KomodoObject testModel1 = workspace.addChild(getTransaction(), "TestModel1", KomodoLexicon.VdbModel.NODE_TYPE);
        KomodoObject testModel2 = workspace.addChild(getTransaction(), "TestModel2", KomodoLexicon.VdbModel.NODE_TYPE);

        for (int i = 1; i <= sourceTotal; ++i) {
            KomodoObject child = testModel1.addChild(getTransaction(), "TestModelSource1-" + i, KomodoLexicon.VdbModelSource.NODE_TYPE);
            child.addChild(getTransaction(), "TestModelSource1-Child" + i, KomodoLexicon.VdbModelSource.NODE_TYPE);
        }

        KomodoObject[] testNodes = testModel1.getChildrenOfType(getTransaction(), KomodoLexicon.VdbModelSource.NODE_TYPE);
        assertEquals(sourceTotal, testNodes.length);

        for (int i = 1; i <= sourceTotal; ++i) {
            KomodoObject child = testModel2.addChild(getTransaction(), "TestModelSource2-" + i, KomodoLexicon.VdbModelSource.NODE_TYPE);
            child.addChild(getTransaction(), "TestModelSource2-Child" + i, KomodoLexicon.VdbModelSource.NODE_TYPE);
        }

        testNodes = testModel2.getChildrenOfType(getTransaction(), KomodoLexicon.VdbModelSource.NODE_TYPE);
        assertEquals(sourceTotal, testNodes.length);

        commit(); // must commit for search queries to work

        //
        // Test object searcher for immediate children beneath testModel1
        //
        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(KomodoLexicon.VdbModelSource.NODE_TYPE);
        os.addWhereParentClause(null, null, testModel1.getAbsolutePath(), true);

        String expected = "SELECT [jcr:path], [mode:localName] FROM " +
                                     OPEN_SQUARE_BRACKET +
                                     KomodoLexicon.VdbModelSource.NODE_TYPE +
                                     CLOSE_SQUARE_BRACKET +
                                     " WHERE ISDESCENDANTNODE('" + userWksp + "') AND ( " +
                                     "[" + JcrConstants.JCR_PATH + "] LIKE '" +
                                     testModel1.getAbsolutePath() + "/%'" +
                                     " AND ISCHILDNODE('" + testModel1.getAbsolutePath() + "')" +
                                     " )";
        assertEquals(expected, os.toString(getTransaction()));
        System.out.println(expected);

        List<KomodoObject> searchObjects = os.searchObjects(getTransaction());

        // Only 5 children below testModel1
        assertEquals(5, searchObjects.size());

        for (KomodoObject searchObject : searchObjects) {
            String name = searchObject.getName(getTransaction());
            assertTrue(name.startsWith("TestModelSource1-"));
        }

        //
        // Test object searcher for all descendents beneath testModel1
        //
        os = new ObjectSearcher(_repo);
        os.setFromType(KomodoLexicon.VdbModelSource.NODE_TYPE);
        os.addWhereParentClause(null, null, testModel1.getAbsolutePath(), false);

        expected = "SELECT [jcr:path], [mode:localName] FROM " +
                                     OPEN_SQUARE_BRACKET +
                                     KomodoLexicon.VdbModelSource.NODE_TYPE +
                                     CLOSE_SQUARE_BRACKET +
                                     " WHERE ISDESCENDANTNODE('" + userWksp + "') AND ( " +
                                     "[" + JcrConstants.JCR_PATH + "] LIKE '" +
                                     testModel1.getAbsolutePath() + "/%'" +
                                     " )";
        assertEquals(expected, os.toString(getTransaction()));
        System.out.println(expected);

        searchObjects = os.searchObjects(getTransaction());

        // 5 children and 5 grandchildren below testModel1
        assertEquals(10, searchObjects.size());

        for (KomodoObject searchObject : searchObjects) {
            String name = searchObject.getName(getTransaction());
            System.out.println(searchObject.getAbsolutePath());
            assertTrue(name.startsWith("TestModelSource1-"));
        }
    }

    @Test
    public void executeFromQueryWithWhereContainsORExpression() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        for (int i = 1; i < 6; ++i) {
            KomodoObject child = workspace.addChild(getTransaction(), "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION, DDL);
        }

        for (int i = 6; i < 11; ++i) {
            KomodoObject child = workspace.addChild(getTransaction(), "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION, TEIIDSQL);
        }

        KomodoObject[] testNodes = workspace.getChildrenOfType(getTransaction(), KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(10, testNodes.length);
        for (KomodoObject testKO : testNodes) {
            Property property = testKO.getProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertTrue(property.getStringValue(getTransaction()).equals(DDL) || property.getStringValue(getTransaction()).equals(TEIIDSQL));
        }

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "nt");
        os.addWhereContainsClause(null, "nt",
                                                     KomodoLexicon.VdbModel.MODEL_DEFINITION,
                                                     KeywordCriteria.ANY,
                                                     DDL, TEIIDSQL);

        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS nt " +
                                        "WHERE ISDESCENDANTNODE('nt', '" + userWksp + "') AND ( " +
                                        "CONTAINS(nt.[vdb:modelDefinition], '" + DDL + " OR " + TEIIDSQL + "')" +
                                        " )";
        assertEquals(expected, os.toString(getTransaction()));

        commit(); // must commit for search queries to work

        List<KomodoObject> searchObjects = os.searchObjects(getTransaction());
        assertEquals(testNodes.length, searchObjects.size());
        for (KomodoObject searchObject : searchObjects) {
            String name = searchObject.getName(getTransaction());
            assertTrue(name.startsWith("test"));

            String indexStr = name.substring(4);
            int index = Integer.parseInt(indexStr);
            assertTrue(index > 0 && index < 11);

            Property property = searchObject.getProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION);
            if (index < 6)
                assertEquals(DDL, property.getStringValue(getTransaction()));
            else
                assertEquals(TEIIDSQL, property.getStringValue(getTransaction()));
        }
    }

    @Test
    public void executeFromQueryWithParanthesisWhereClause() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        for (int i = 1; i < 6; ++i) {
            KomodoObject child = workspace.addChild(getTransaction(), "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION, DDL);
        }

        for (int i = 6; i < 11; ++i) {
            KomodoObject child = workspace.addChild(getTransaction(), "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION, TEIIDSQL);
        }

        KomodoObject[] testNodes = workspace.getChildrenOfType(getTransaction(), KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(10, testNodes.length);
        for (KomodoObject testKO : testNodes) {
            Property property = testKO.getProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertTrue(property.getStringValue(getTransaction()).equals(DDL) || property.getStringValue(getTransaction()).equals(TEIIDSQL));
        }

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "nt");

        //
        // Clause 1 is set to be case sensitive
        //
        CompareClause clause1 = new CompareClause(null, "nt", KomodoLexicon.VdbModel.MODEL_DEFINITION,
                                                                                      ComparisonOperator.LIKE, DDL);
        //
        // Clause 2 is case insensitive with the literal purposely not the
        // same case as the property being tested.
        //
        CompareClause clause2 = new CompareClause(null, "nt", KomodoLexicon.VdbModel.MODEL_DEFINITION,
                                                                                      ComparisonOperator.LIKE, "TEIIDSQL", true);

        os.addWhereParanthesisClause(null, clause1, clause2);

        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS nt " +
                                        "WHERE ISDESCENDANTNODE('nt', '" + userWksp + "') AND ( " +
                                        "(nt.[vdb:modelDefinition] LIKE 'DDL' OR LOWER(nt.[vdb:modelDefinition]) LIKE 'teiidsql')" +
                                        " )";
        assertEquals(expected, os.toString(getTransaction()));

        commit(); // must commit for search queries to work

        List<KomodoObject> searchObjects = os.searchObjects(getTransaction());
        assertEquals(testNodes.length, searchObjects.size());
        for (KomodoObject searchObject : searchObjects) {
            String name = searchObject.getName(getTransaction());
            assertTrue(name.startsWith("test"));

            String indexStr = name.substring(4);
            int index = Integer.parseInt(indexStr);
            assertTrue(index > 0 && index < 11);

            Property property = searchObject.getProperty(getTransaction(), KomodoLexicon.VdbModel.MODEL_DEFINITION);
            if (index < 6)
                assertEquals(DDL, property.getStringValue(getTransaction()));
            else
                assertEquals(TEIIDSQL, property.getStringValue(getTransaction()));
        }
    }

    @Test
    public void executePathQueryToFindWorkspace() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        commit(); // must commit for search queries to work

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "nt");
        os.addWherePathClause(null, "nt", RepositoryImpl.komodoWorkspacePath(getTransaction()));

        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS nt " +
                                        "WHERE ISDESCENDANTNODE('nt', '" + userWksp + "') AND ( " +
                                        "PATH(nt) LIKE '" + workspace.getAbsolutePath() + "'" +
                                        " )";
        assertEquals(expected, os.toString(getTransaction()));

        //
        // This will only work with a sys transaction since a user transaction
        // only searches from their user home directory and below
        //
        UnitOfWork sysTx = sysTx();
        List<KomodoObject> searchObjects = os.searchObjects(sysTx);
        assertEquals(1, searchObjects.size());

        assertEquals(workspace.getAbsolutePath(), searchObjects.iterator().next().getAbsolutePath());

        //
        // Test with a wildcard path
        //
        os = new ObjectSearcher(_repo);
        os.setFromType(JcrConstants.NT_UNSTRUCTURED, "nt");
        os.addWherePathClause(null, "nt", "/tko:komodo/%");
        searchObjects = os.searchObjects(sysTx);

        // /tko:komodo/tko:workspace
        // /tko:komodo/tko:workspace/$USER
        // /tko:komodo/tko:library
        // /tko:komodo/tko:environment
        // /tko:komodo/tko:environment/tko:servers
        // /tko:komodo/tko:environment/tko:servers/DefaultServer
        // /tko:komodo/tko:environment/tko:validation
        assertEquals(7, searchObjects.size());
    }

    @Test
    public void shouldWriteToRepository() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        commit(); // must commit for search queries to work

        String alias = JcrNtLexicon.Namespace.PREFIX;
        String fromTypeType = JcrConstants.NT_UNSTRUCTURED;
        String wherePath = RepositoryImpl.komodoWorkspacePath(getTransaction());

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(fromTypeType, alias);
        os.addWherePathClause(null, JcrNtLexicon.Namespace.PREFIX, wherePath);

        String searchName = "WorkspaceSearch";
        KomodoObject searchObject = os.write(getTransaction(), searchName);
        assertNotNull(searchObject);

        String searchesPath = _repo.komodoSearches(getTransaction()).getAbsolutePath();
        String searchPath = searchesPath + FORWARD_SLASH + searchName;
        assertEquals(searchPath, searchObject.getAbsolutePath());

        assertEquals(Search.NODE_TYPE, searchObject.getPrimaryType(getTransaction()).getName());
        assertNotNull(searchObject.getProperty(getTransaction(), Search.SEARCH_DATE));

        KomodoObject fromTypeObject = searchObject.getChild(getTransaction(), Search.FROM_TYPE);
        assertEquals(Search.FromType.NODE_TYPE, fromTypeObject.getPrimaryType(getTransaction()).getName());

        assertEquals(fromTypeType,
                     fromTypeObject.getProperty(getTransaction(), Search.FromType.TYPE).getStringValue(getTransaction()));
        assertEquals(alias,
                     fromTypeObject.getProperty(getTransaction(), Search.FromType.ALIAS).getStringValue(getTransaction()));

        KomodoObject whereObject = searchObject.getChild(getTransaction(), Search.WHERE_CLAUSE);
        assertEquals(Search.WherePathClause.NODE_TYPE, whereObject.getPrimaryType(getTransaction()).getName());

        assertEquals(wherePath,
                     whereObject.getProperty(getTransaction(), Search.WherePathClause.PATH).getStringValue(getTransaction()));
        assertEquals(alias,
                     whereObject.getProperty(getTransaction(), Search.WhereClause.ALIAS).getStringValue(getTransaction()));

        ObjectSearcher testOS = new ObjectSearcher(_repo);
        testOS.read(getTransaction(), searchName);

        assertEquals(os, testOS);
    }

    @Test
    public void shouldReplaceSearch() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        commit(); // must commit for search queries to work

        String alias = JcrNtLexicon.Namespace.PREFIX;
        String fromTypeType = JcrConstants.NT_UNSTRUCTURED;
        String wherePath = RepositoryImpl.komodoWorkspacePath(getTransaction());

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(fromTypeType, alias);
        os.addWherePathClause(null, JcrNtLexicon.Namespace.PREFIX, wherePath);

        String searchName = "WorkspaceSearch";
        KomodoObject searchObject = os.write(getTransaction(), searchName);
        assertNotNull(searchObject);

        alias = "ms";
        fromTypeType = KomodoLexicon.VdbModelSource.NODE_TYPE;
        os = new ObjectSearcher(_repo);
        os.setFromType(fromTypeType, alias);
        String whereProperty = KomodoLexicon.VdbModelSource.JNDI_NAME;
        ComparisonOperator compareOperator = ComparisonOperator.EQUALS;
        String whereValue = "oracle";
        os.addWhereCompareClause(null, alias,
                                 whereProperty,
                                 compareOperator,
                                 whereValue);

        // Should replace WorkspaceSearch with new search
        os.write(getTransaction(), searchName);

        String searchesPath = _repo.komodoSearches(getTransaction()).getAbsolutePath();
        String searchPath = searchesPath + FORWARD_SLASH + searchName;
        assertEquals(searchPath, searchObject.getAbsolutePath());

        assertEquals(Search.NODE_TYPE, searchObject.getPrimaryType(getTransaction()).getName());
        assertNotNull(searchObject.getProperty(getTransaction(), Search.SEARCH_DATE));

        KomodoObject fromTypeObject = searchObject.getChild(getTransaction(), Search.FROM_TYPE);
        assertEquals(Search.FromType.NODE_TYPE, fromTypeObject.getPrimaryType(getTransaction()).getName());

        assertEquals(fromTypeType,
                     fromTypeObject.getProperty(getTransaction(), Search.FromType.TYPE).getStringValue(getTransaction()));
        assertEquals(alias,
                     fromTypeObject.getProperty(getTransaction(), Search.FromType.ALIAS).getStringValue(getTransaction()));

        KomodoObject whereObject = searchObject.getChild(getTransaction(), Search.WHERE_CLAUSE);
        assertEquals(Search.WhereCompareClause.NODE_TYPE, whereObject.getPrimaryType(getTransaction()).getName());

        assertEquals(alias,
                     whereObject.getProperty(getTransaction(), WhereClause.ALIAS).getStringValue(getTransaction()));
        assertEquals(whereProperty,
                     whereObject.getProperty(getTransaction(), Search.WhereCompareClause.PROPERTY).getStringValue(getTransaction()));
        assertEquals(compareOperator.toString(),
                     whereObject.getProperty(getTransaction(), Search.WhereCompareClause.COMPARE_OPERATOR).getStringValue(getTransaction()));
        assertEquals(whereValue,
                     whereObject.getProperty(getTransaction(), Search.WhereCompareClause.VALUE).getStringValue(getTransaction()));

        ObjectSearcher testOS = new ObjectSearcher(_repo);
        testOS.read(getTransaction(), searchName);

        assertEquals(os, testOS);
    }

    @Test
    public void shouldWriteToRepositoryMultipleWhereClauses() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        commit(); // must commit for search queries to work

        String alias = JcrNtLexicon.Namespace.PREFIX;
        String fromTypeType = JcrConstants.NT_UNSTRUCTURED;
        String wherePath = RepositoryImpl.komodoWorkspacePath(getTransaction());
        String whereProperty = KomodoLexicon.VdbModelSource.JNDI_NAME;
        ComparisonOperator compareOperator = ComparisonOperator.EQUALS;
        String whereValue = "oracle";

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(fromTypeType, alias);
        os.addWherePathClause(null, JcrNtLexicon.Namespace.PREFIX, wherePath);
        os.addWhereCompareClause(LogicalOperator.OR, alias,
                                 whereProperty,
                                 compareOperator,
                                 whereValue);

        String searchName = "WorkspaceSearch";
        KomodoObject searchObject = os.write(getTransaction(), searchName);
        assertNotNull(searchObject);

        String searchesPath = _repo.komodoSearches(getTransaction()).getAbsolutePath();
        String searchPath = searchesPath + FORWARD_SLASH + searchName;
        assertEquals(searchPath, searchObject.getAbsolutePath());

        assertEquals(Search.NODE_TYPE, searchObject.getPrimaryType(getTransaction()).getName());
        assertNotNull(searchObject.getProperty(getTransaction(), Search.SEARCH_DATE));

        KomodoObject fromTypeObject = searchObject.getChild(getTransaction(), Search.FROM_TYPE);
        assertEquals(Search.FromType.NODE_TYPE, fromTypeObject.getPrimaryType(getTransaction()).getName());

        assertEquals(fromTypeType,
                     fromTypeObject.getProperty(getTransaction(), Search.FromType.TYPE).getStringValue(getTransaction()));
        assertEquals(alias,
                     fromTypeObject.getProperty(getTransaction(), Search.FromType.ALIAS).getStringValue(getTransaction()));

        KomodoObject[] whereObjects = searchObject.getChildren(getTransaction(), Search.WHERE_CLAUSE);

        assertEquals(Search.WherePathClause.NODE_TYPE, whereObjects[0].getPrimaryType(getTransaction()).getName());
        assertEquals(Search.WhereCompareClause.NODE_TYPE, whereObjects[1].getPrimaryType(getTransaction()).getName());
        assertEquals(wherePath,
                     whereObjects[0].getProperty(getTransaction(), Search.WherePathClause.PATH).getStringValue(getTransaction()));
        assertEquals(alias,
                     whereObjects[0].getProperty(getTransaction(), Search.WhereClause.ALIAS).getStringValue(getTransaction()));

        assertEquals(Search.WhereCompareClause.NODE_TYPE, whereObjects[1].getPrimaryType(getTransaction()).getName());
        assertEquals(LogicalOperator.OR.toString(),
                     whereObjects[1].getProperty(getTransaction(), Search.WhereClause.PRE_CLAUSE_OPERATOR).getStringValue(getTransaction()));
        assertEquals(alias,
                     whereObjects[1].getProperty(getTransaction(), WhereClause.ALIAS).getStringValue(getTransaction()));
        assertEquals(whereProperty,
                     whereObjects[1].getProperty(getTransaction(), Search.WhereCompareClause.PROPERTY).getStringValue(getTransaction()));
        assertEquals(compareOperator.toString(),
                     whereObjects[1].getProperty(getTransaction(), Search.WhereCompareClause.COMPARE_OPERATOR).getStringValue(getTransaction()));
        assertEquals(whereValue,
                     whereObjects[1].getProperty(getTransaction(), Search.WhereCompareClause.VALUE).getStringValue(getTransaction()));

        ObjectSearcher testOS = new ObjectSearcher(_repo);
        testOS.read(getTransaction(), searchName);

        assertEquals(os, testOS);
    }

    @Test
    public void shouldWriteToRepositoryParathensisWhereClauses() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        commit(); // must commit for search queries to work

        String alias = JcrNtLexicon.Namespace.PREFIX;
        String fromTypeType = JcrConstants.NT_UNSTRUCTURED;
        String wherePath = RepositoryImpl.komodoWorkspacePath(getTransaction());
        String whereProperty = KomodoLexicon.VdbModelSource.JNDI_NAME;
        ComparisonOperator compareOperator = ComparisonOperator.EQUALS;
        String whereValue = "oracle";

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(fromTypeType, alias);

        PathClause clause1 = new PathClause(null, JcrNtLexicon.Namespace.PREFIX, wherePath);
        CompareClause clause2 = new CompareClause(LogicalOperator.OR, alias, whereProperty,
                                                                                      compareOperator, whereValue);

        os.addWhereParanthesisClause(null, clause1, clause2);

        String searchName = "WorkspaceSearch";
        KomodoObject searchObject = os.write(getTransaction(), searchName);
        assertNotNull(searchObject);

        String searchesPath = _repo.komodoSearches(getTransaction()).getAbsolutePath();
        String searchPath = searchesPath + FORWARD_SLASH + searchName;
        assertEquals(searchPath, searchObject.getAbsolutePath());

        assertEquals(Search.NODE_TYPE, searchObject.getPrimaryType(getTransaction()).getName());
        assertNotNull(searchObject.getProperty(getTransaction(), Search.SEARCH_DATE));

        KomodoObject fromTypeObject = searchObject.getChild(getTransaction(), Search.FROM_TYPE);
        assertEquals(Search.FromType.NODE_TYPE, fromTypeObject.getPrimaryType(getTransaction()).getName());

        assertEquals(fromTypeType,
                     fromTypeObject.getProperty(getTransaction(), Search.FromType.TYPE).getStringValue(getTransaction()));
        assertEquals(alias,
                     fromTypeObject.getProperty(getTransaction(), Search.FromType.ALIAS).getStringValue(getTransaction()));

        KomodoObject[] whereObjects = searchObject.getChildren(getTransaction(), Search.WHERE_CLAUSE);
        assertEquals(1, whereObjects.length);
        assertEquals(Search.WhereParanthesisClause.NODE_TYPE, whereObjects[0].getPrimaryType(getTransaction()).getName());

        KomodoObject[] subWhereObjects = whereObjects[0].getChildren(getTransaction(), Search.WHERE_CLAUSE);
        assertEquals(2, subWhereObjects.length);

        assertEquals(Search.WherePathClause.NODE_TYPE, subWhereObjects[0].getPrimaryType(getTransaction()).getName());
        assertEquals(Search.WhereCompareClause.NODE_TYPE, subWhereObjects[1].getPrimaryType(getTransaction()).getName());
        assertEquals(wherePath,
                     subWhereObjects[0].getProperty(getTransaction(), Search.WherePathClause.PATH).getStringValue(getTransaction()));
        assertEquals(alias,
                     subWhereObjects[0].getProperty(getTransaction(), Search.WhereClause.ALIAS).getStringValue(getTransaction()));

        assertEquals(Search.WhereCompareClause.NODE_TYPE, subWhereObjects[1].getPrimaryType(getTransaction()).getName());
        assertEquals(LogicalOperator.OR.toString(),
                     subWhereObjects[1].getProperty(getTransaction(), Search.WhereClause.PRE_CLAUSE_OPERATOR).getStringValue(getTransaction()));
        assertEquals(alias,
                     subWhereObjects[1].getProperty(getTransaction(), WhereClause.ALIAS).getStringValue(getTransaction()));
        assertEquals(whereProperty,
                     subWhereObjects[1].getProperty(getTransaction(), Search.WhereCompareClause.PROPERTY).getStringValue(getTransaction()));
        assertEquals(compareOperator.toString(),
                     subWhereObjects[1].getProperty(getTransaction(), Search.WhereCompareClause.COMPARE_OPERATOR).getStringValue(getTransaction()));
        assertEquals(whereValue,
                     subWhereObjects[1].getProperty(getTransaction(), Search.WhereCompareClause.VALUE).getStringValue(getTransaction()));

        ObjectSearcher testOS = new ObjectSearcher(_repo);
        testOS.read(getTransaction(), searchName);

        assertEquals(os, testOS);
    }

    @Test
    public void shouldWriteToRepositoryWhereContainsClause() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        commit(); // must commit for search queries to work

        String alias = JcrNtLexicon.Namespace.PREFIX;
        String fromTypeType = JcrConstants.NT_UNSTRUCTURED;
        String whereProperty = KomodoLexicon.VdbModelSource.JNDI_NAME;
        String[] keywords = {"jndi1", "jndi2", "jndi3"};
        KeywordCriteria criteria = KeywordCriteria.ANY;

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(fromTypeType, alias);
        os.addWhereContainsClause(null, alias, whereProperty, criteria, keywords[0], keywords[1], keywords[2]);

        String searchName = "WorkspaceSearch";
        KomodoObject searchObject = os.write(getTransaction(), searchName);
        assertNotNull(searchObject);

        String searchesPath = _repo.komodoSearches(getTransaction()).getAbsolutePath();
        String searchPath = searchesPath + FORWARD_SLASH + searchName;
        assertEquals(searchPath, searchObject.getAbsolutePath());

        assertEquals(Search.NODE_TYPE, searchObject.getPrimaryType(getTransaction()).getName());
        assertNotNull(searchObject.getProperty(getTransaction(), Search.SEARCH_DATE));

        KomodoObject whereObject = searchObject.getChild(getTransaction(), Search.WHERE_CLAUSE);
        assertEquals(Search.WhereContainsClause.NODE_TYPE, whereObject.getPrimaryType(getTransaction()).getName());

        assertEquals(whereProperty,
                     whereObject.getProperty(getTransaction(), Search.WhereContainsClause.PROPERTY).getStringValue(getTransaction()));
        assertArrayEquals(keywords,
                     whereObject.getProperty(getTransaction(), Search.WhereContainsClause.KEYWORDS).getStringValues(getTransaction()));
        assertEquals(alias,
                     whereObject.getProperty(getTransaction(), Search.WhereClause.ALIAS).getStringValue(getTransaction()));

        ObjectSearcher testOS = new ObjectSearcher(_repo);
        testOS.read(getTransaction(), searchName);

        assertEquals(os, testOS);
    }

    @Test
    public void shouldWriteToRepositoryWhereSetClause() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(getTransaction());
        assertNotNull(workspace);

        commit(); // must commit for search queries to work

        String alias = JcrNtLexicon.Namespace.PREFIX;
        String fromTypeType = JcrConstants.NT_UNSTRUCTURED;
        String whereProperty = KomodoLexicon.VdbModelSource.JNDI_NAME;
        String[] values = {"jndi1", "jndi2", "jndi3"};

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(fromTypeType, alias);
        os.addWhereSetClause(null, alias, whereProperty, values[0], values[1], values[2]);

        String searchName = "WorkspaceSearch";
        KomodoObject searchObject = os.write(getTransaction(), searchName);
        assertNotNull(searchObject);

        String searchesPath = _repo.komodoSearches(getTransaction()).getAbsolutePath();
        String searchPath = searchesPath + FORWARD_SLASH + searchName;
        assertEquals(searchPath, searchObject.getAbsolutePath());

        assertEquals(Search.NODE_TYPE, searchObject.getPrimaryType(getTransaction()).getName());
        assertNotNull(searchObject.getProperty(getTransaction(), Search.SEARCH_DATE));

        KomodoObject whereObject = searchObject.getChild(getTransaction(), Search.WHERE_CLAUSE);
        assertEquals(Search.WhereSetClause.NODE_TYPE, whereObject.getPrimaryType(getTransaction()).getName());

        assertEquals(whereProperty,
                     whereObject.getProperty(getTransaction(), Search.WhereSetClause.PROPERTY).getStringValue(getTransaction()));
        assertArrayEquals(values,
                     whereObject.getProperty(getTransaction(), Search.WhereSetClause.VALUES).getStringValues(getTransaction()));
        assertEquals(alias,
                     whereObject.getProperty(getTransaction(), Search.WhereClause.ALIAS).getStringValue(getTransaction()));

        ObjectSearcher testOS = new ObjectSearcher(_repo);
        testOS.read(getTransaction(), searchName);

        assertEquals(os, testOS);
    }

    @Test
    public void shouldErrorDueToNoParameterValueForFrom() throws Exception {
        String alias = JcrNtLexicon.Namespace.PREFIX;
        String fromTypeType = "{fromTypeParam}";
        String whereProperty = KomodoLexicon.VdbModelSource.JNDI_NAME;
        String[] values = {"jndi1", "jndi2", "jndi3"};

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(fromTypeType, alias);
        os.addWhereSetClause(null, alias, whereProperty, values[0], values[1], values[2]);

        try {
            os.searchObjects(getTransaction());
            fail("Should fail due to no parameter value");
        } catch (KException ex) {
            // Nothing to do
        }
    }

    @Test
    public void shouldErrorDueToNoParameterValueForWhere() throws Exception {
        String alias = JcrNtLexicon.Namespace.PREFIX;
        String fromTypeType = "{fromTypeParam}";
        String whereProperty = KomodoLexicon.VdbModelSource.JNDI_NAME;
        String[] values = {"jndi1", "jndi2", "{value3}"};

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(fromTypeType, alias);
        os.addWhereSetClause(null, alias, whereProperty, values[0], values[1], values[2]);

        os.setParameterValue("fromTypeParam", JcrConstants.NT_UNSTRUCTURED);

        try {
            os.searchObjects(getTransaction());
            fail("Should fail due to no parameter value");
        } catch (KException ex) {
            // Nothing to do
        }
    }

    @Test
    public void shouldSubstituteParameterValues() throws Exception {
        createTestData();

        String alias = JcrNtLexicon.Namespace.PREFIX;
        String fromTypeType = "{fromTypeParam}";
        String containsValue = "{containsValueParam}";

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.setFromType(fromTypeType, alias);
        os.addWhereContainsClause(null, alias, KomodoLexicon.VdbModel.MODEL_DEFINITION, containsValue);

        os.setParameterValue("fromTypeParam", JcrConstants.NT_UNSTRUCTURED);
        os.setParameterValue(containsValue, DDL);

        try {
            List<KomodoObject> results = os.searchObjects(getTransaction());
            assertEquals(5, results.size());
        } catch (KException ex) {
            // Nothing to do
        }
    }
}
