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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import java.util.List;
import org.junit.Test;
import org.komodo.core.KomodoLexicon;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.repository.RepositoryImpl;
import org.komodo.repository.RepositoryTools;
import org.komodo.repository.search.Clause.LogicalOperator;
import org.komodo.spi.KException;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Property;
import org.komodo.spi.repository.Repository;
import org.komodo.spi.repository.Repository.KeywordCriteria;
import org.komodo.spi.repository.Repository.UnitOfWork;
import org.komodo.spi.repository.Repository.UnitOfWorkListener;
import org.komodo.test.utils.AbstractLocalRepositoryTest;
import org.modeshape.jcr.api.JcrConstants;

@SuppressWarnings( {"nls", "javadoc"} )
public class TestObjectSearcher extends AbstractLocalRepositoryTest {

    private Repository mockRepository() throws KException {
        Repository repository = mock(Repository.class);
        UnitOfWork transaction = mock(UnitOfWork.class);
        when(repository.createTransaction(
                                          any(String.class),
                                          any(Boolean.class),
                                          any(UnitOfWorkListener.class))).thenReturn(transaction);

        return repository;
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
            Repository repository = mockRepository();
            ObjectSearcher os = new ObjectSearcher(repository);
            os.toString();
            fail("ObjectSearcher should require at least 1 From Clause");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }

        try {
            Repository repository = mockRepository();
            ObjectSearcher os = new ObjectSearcher(repository);
            os.toString();
            fail("ObjectSearcher should require at least 1 From Clause");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void oneFrom() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [tko:workspace]";
        Repository repository = mockRepository();
        ObjectSearcher os = new ObjectSearcher(repository);
        os.addFromType(KomodoLexicon.Komodo.WORKSPACE, null);
        assertEquals(expected, os.toString());
    }

    @Test
    public void twoFroms() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [tko:workspace], [tko:library]";
        Repository repository = mockRepository();
        ObjectSearcher os = new ObjectSearcher(repository);
        os.addFromType(KomodoLexicon.Komodo.WORKSPACE);
        os.addFromType(KomodoLexicon.Komodo.LIBRARY);
        assertEquals(expected, os.toString());
    }

    @Test
    public void addWhereClauseWithNullType() {
        try {
            Repository repository = mockRepository();
            ObjectSearcher os = new ObjectSearcher(repository);
            os.addWhereSetClause(null, "p1", "property1", (String[]) null);
            os.toString();
            fail("Type cannot be null");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void addWhereClauseWithNullAliasNoFrom() {
        try {
            Repository repository = mockRepository();
            ObjectSearcher os = new ObjectSearcher(repository);
            os.addWhereSetClause(null, null, "property1", "value1");
            os.toString();
            fail("Alias cannot be null if there are no from clauses yet!");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void addWhereClauseWithNullAliases() {
        try {
            Repository repository = mockRepository();
            ObjectSearcher os = new ObjectSearcher(repository);
            os.addFromType(KomodoLexicon.Komodo.WORKSPACE);
            os.addWhereSetClause(null, null, "property1", "value1");
            os.toString();
        } catch (Exception ex) {
            ex.printStackTrace();
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void addWhereClauseWithNullAliasWithOneFrom() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                     "WHERE p1.[property1] IN ('value1')";

        Repository repository = mockRepository();
        ObjectSearcher os = new ObjectSearcher(repository);
        os.addFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, null, "property1", "value1");

        assertEquals(expected, os.toString());
    }

    @Test
    public void addWhereClauseWithInvalidAlias() throws Exception {
        try {
            Repository repository = mockRepository();
            ObjectSearcher os = new ObjectSearcher(repository);
            os.addFromType(KomodoLexicon.Komodo.WORKSPACE);
            os.addWhereSetClause(null, "p2", "property1", "value1");
            os.toString();
            fail("Alias of where clause must refer to existing alias");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }

        try {
            Repository repository = mockRepository();
            ObjectSearcher os = new ObjectSearcher(repository);
            os.addFromType(KomodoLexicon.Komodo.WORKSPACE, "p1");
            os.addWhereSetClause(null, "p2", "property1", "value1");
            os.toString();
            fail("Alias of where clause must refer to existing alias");
        } catch (Exception ex) {
            assertTrue(ex instanceof IllegalArgumentException);
        }
    }

    @Test
    public void addWhereClauseTwoPropertyValues() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                    "WHERE p1.[property1] IN ('value1', 'value2')";
        Repository repository = mockRepository();
        ObjectSearcher os = new ObjectSearcher(repository);
        os.addFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, "p1", "property1", "value1", "value2");

        assertEquals(expected, os.toString());
    }

    @Test
    public void addTwoFromTypes() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [tko:vdb], [tsql:notCriteria]";
        Repository repository = mockRepository();
        ObjectSearcher os = new ObjectSearcher(repository);
        os.addFromType(KomodoLexicon.Vdb.NODE_TYPE);
        os.addFromType(TeiidSqlLexicon.NotCriteria.ID);

        assertEquals(expected, os.toString());
    }

    @Test
    public void addWhereClauseTwoDifferentProperties() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                     "WHERE p1.[property1] IN ('value1', 'value2') " +
                                     "OR p1.[name] IN ('bob', 'bryan')";
        Repository repository = mockRepository();
        ObjectSearcher os = new ObjectSearcher(repository);
        os.addFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, "p1", "property1", "value1", "value2");
        os.addWhereSetClause(LogicalOperator.OR, "p1", "name", "bob", "bryan");

        assertEquals(expected, os.toString());
    }

    @Test
    public void addWhereClauseOneSetPropertyOneContainsProperty() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                     "WHERE p1.[property1] IN ('value1', 'value2') " +
                                     "OR CONTAINS(p1.[name], 'bob')";
        Repository repository = mockRepository();
        ObjectSearcher os = new ObjectSearcher(repository);
        os.addFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, "p1", "property1", "value1", "value2");
        os.addWhereContainsClause(LogicalOperator.OR, "p1", "name", "bob");

        assertEquals(expected, os.toString());
    }

    @Test
    public void addWhereClauseContainsPropertyWithTwoKeywords() throws Exception {
        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS p1 " +
                                     "WHERE p1.[property1] IN ('value1', 'value2') " +
                                     "OR CONTAINS(p1.[name], 'bob OR chris')";
        Repository repository = mockRepository();
        ObjectSearcher os = new ObjectSearcher(repository);
        os.addFromType(JcrConstants.NT_UNSTRUCTURED, "p1");
        os.addWhereSetClause(null, "p1", "property1", "value1", "value2");
        os.addWhereContainsClause(LogicalOperator.OR, "p1", "name", KeywordCriteria.ANY, "bob", "chris");

        assertEquals(expected, os.toString());
    }

    @Test
    public void executeSingleFromQuery() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(null);
        assertNotNull(workspace);

        workspace.addChild(null, "test1", KomodoLexicon.VdbModel.NODE_TYPE);
        workspace.addChild(null, "test2", KomodoLexicon.VdbModel.NODE_TYPE);
        workspace.addChild(null, "test3", KomodoLexicon.VdbModel.NODE_TYPE);
        workspace.addChild(null, "test4", KomodoLexicon.VdbModel.NODE_TYPE);
        workspace.addChild(null, "test5", KomodoLexicon.VdbModel.NODE_TYPE);

        KomodoObject[] testNodes = workspace.getChildrenOfType(null, KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(5, testNodes.length);

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.addFromType(KomodoLexicon.VdbModel.NODE_TYPE);
        List<KomodoObject> searchObjects = os.searchObjects(null);
        assertEquals(testNodes.length, searchObjects.size());
    }

    @Test
    public void executeFromQueryWithWhere() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(null);
        assertNotNull(workspace);

        for (int i = 1; i < 6; ++i) {
            KomodoObject child = workspace.addChild(null, "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(null, KomodoLexicon.VdbModel.MODEL_DEFINITION, "DDL");
        }

        KomodoObject[] testNodes = workspace.getChildrenOfType(null, KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(5, testNodes.length);
        for (KomodoObject testKO : testNodes) {
            Property property = testKO.getProperty(null, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertEquals("DDL", property.getStringValue(null));
        }

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.addFromType(JcrConstants.NT_UNSTRUCTURED, "nt");
        os.addWhereContainsClause(null, "nt", KomodoLexicon.VdbModel.MODEL_DEFINITION, "DDL");

        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS nt " +
                                     "WHERE CONTAINS(nt.[vdb:modelDefinition], 'DDL')";
        assertEquals(expected, os.toString());
        
        List<KomodoObject> searchObjects = os.searchObjects(null);
        assertEquals(testNodes.length, searchObjects.size());
        for (KomodoObject searchObject : searchObjects) {
            String name = searchObject.getName(null);
            assertTrue(name.startsWith("test"));

            Property property = searchObject.getProperty(null, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertEquals("DDL", property.getStringValue(null));
        }
    }

    @Test
    public void executeFromQueryWithWhereContainsORExpression() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(null);
        assertNotNull(workspace);

        for (int i = 1; i < 6; ++i) {
            KomodoObject child = workspace.addChild(null, "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(null, KomodoLexicon.VdbModel.MODEL_DEFINITION, "DDL");
        }

        for (int i = 6; i < 11; ++i) {
            KomodoObject child = workspace.addChild(null, "test" + i, KomodoLexicon.VdbModel.NODE_TYPE);
            child.setProperty(null, KomodoLexicon.VdbModel.MODEL_DEFINITION, "TEIIDSQL");
        }

        KomodoObject[] testNodes = workspace.getChildrenOfType(null, KomodoLexicon.VdbModel.NODE_TYPE);
        assertEquals(10, testNodes.length);
        for (KomodoObject testKO : testNodes) {
            Property property = testKO.getProperty(null, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            assertTrue(property.getStringValue(null).equals("DDL") || property.getStringValue(null).equals("TEIIDSQL"));
        }

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.addFromType(JcrConstants.NT_UNSTRUCTURED, "nt");
        os.addWhereContainsClause(null, "nt",
                                                     KomodoLexicon.VdbModel.MODEL_DEFINITION,
                                                     KeywordCriteria.ANY,
                                                     "DDL", "TEIIDSQL");

        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS nt " +
                                     "WHERE CONTAINS(nt.[vdb:modelDefinition], 'DDL OR TEIIDSQL')";
        assertEquals(expected, os.toString());

        List<KomodoObject> searchObjects = os.searchObjects(null);
        assertEquals(testNodes.length, searchObjects.size());
        for (KomodoObject searchObject : searchObjects) {
            String name = searchObject.getName(null);
            assertTrue(name.startsWith("test"));

            String indexStr = name.substring(4);
            int index = Integer.parseInt(indexStr);
            assertTrue(index > 0 && index < 11);

            Property property = searchObject.getProperty(null, KomodoLexicon.VdbModel.MODEL_DEFINITION);
            if (index < 6)
                assertEquals("DDL", property.getStringValue(null));
            else
                assertEquals("TEIIDSQL", property.getStringValue(null));
        }
    }

    @Test
    public void executePathQueryToFindWorkspace() throws Exception {
        assertNotNull(_repo);

        // Create the komodo workspace
        KomodoObject workspace = _repo.komodoWorkspace(null);
        assertNotNull(workspace);

        System.out.println(RepositoryTools.traverse(workspace.getParent(null)));

        ObjectSearcher os = new ObjectSearcher(_repo);
        os.addFromType(JcrConstants.NT_UNSTRUCTURED, "nt");
        os.addWherePathClause(null, "nt", RepositoryImpl.WORKSPACE_ROOT);

        String expected = "SELECT [jcr:path], [mode:localName] FROM [nt:unstructured] AS nt " +
                                     "WHERE PATH(nt) = '" + workspace.getAbsolutePath() + "'";
        assertEquals(expected, os.toString());

        List<KomodoObject> searchObjects = os.searchObjects(null);
        assertEquals(1, searchObjects.size());

        assertEquals(workspace.getAbsolutePath(), searchObjects.iterator().next().getAbsolutePath());
    }
}
