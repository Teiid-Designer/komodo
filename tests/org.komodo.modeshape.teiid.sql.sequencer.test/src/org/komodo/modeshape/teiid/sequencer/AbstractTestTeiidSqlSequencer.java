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
package org.komodo.modeshape.teiid.sequencer;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.modeshape.jcr.api.JcrConstants.JCR_MIXIN_TYPES;
import static org.modeshape.jcr.api.JcrConstants.JCR_PRIMARY_TYPE;
import java.io.File;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.Property;
import javax.jcr.PropertyIterator;
import javax.jcr.RepositoryException;
import javax.jcr.Value;
import org.junit.Test;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AbstractCompareCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AbstractSetCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AggregateSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AliasSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.AssignmentStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.BetweenCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Block;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.CommandStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.CompareCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Constant;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.CreateProcedureCommand;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.DeclareStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.DerivedColumn;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.DynamicCommand;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ElementSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Expression;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ExpressionSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.From;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Function;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.GroupBy;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.GroupSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.IfStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Insert;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.IsNullCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.JoinPredicate;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.JoinType;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.LoopStatement;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.MatchCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.MultipleElementSymbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.NotCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.OrderBy;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.OrderByItem;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.ProjectedColumn;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Query;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.QueryCommand;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Reference;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Select;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SetCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SetQuery;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.StoredProcedure;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SubqueryContainer;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SubqueryFromClause;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.SubquerySetCriteria;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Symbol;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TableFunctionReference;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TargetedCommand;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TextColumn;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.TextTable;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.UnaryFromClause;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.WindowFunction;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.WindowSpecification;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLAttributes;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.XMLElement;
import org.komodo.modeshape.teiid.language.SortSpecification.NullOrdering;
import org.komodo.modeshape.teiid.sql.lang.CriteriaOperator;
import org.komodo.spi.query.sql.lang.IJoinType;
import org.komodo.spi.query.sql.lang.ISetQuery;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.IDataTypeManagerService;
import org.komodo.spi.type.IDataTypeManagerService.DataTypeName;
import org.komodo.utils.KLog;
import org.modeshape.jcr.api.JcrConstants;
import org.teiid.runtime.client.admin.factory.ExecutionAdminFactory;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractTestTeiidSqlSequencer extends AbstractSequencerTest {

    protected ITeiidVersion teiidVersion;

    /**
     * @param teiidVersion
     */
    public AbstractTestTeiidSqlSequencer(ITeiidVersion teiidVersion) {
        this.teiidVersion = teiidVersion;
    }

    @Override
    protected ITeiidVersion getTeiidVersion() {
        return teiidVersion;
    }

    protected IDataTypeManagerService getDataTypeService() {
        ExecutionAdminFactory factory = new ExecutionAdminFactory();
        return factory.getDataTypeManagerService(teiidVersion); 
    }

    protected void verifyProperty( Node node, String propertyName, String expectedValue ) throws RepositoryException {
        Property property = node.getProperty(propertyName);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertEquals(expectedValue, value.getString());
    }

    protected void verifyProperty( Node node, String propertyName, long expectedValue ) throws RepositoryException {
        Property property = node.getProperty(propertyName);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertEquals(expectedValue, value.getLong());
    }

    protected void verifyProperty( Node node, String propertyName, boolean expectedValue ) throws RepositoryException {
        Property property = node.getProperty(propertyName);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertEquals(expectedValue, value.getBoolean());
    }

    protected void verifyProperty( Node node, String propertyName, java.sql.Date expectedValue ) throws RepositoryException {
        Property property = node.getProperty(propertyName);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertEquals(expectedValue, java.sql.Date.valueOf(value.getString()));
    }

    protected void verifyProperty( Node node, String propertyName, java.sql.Time expectedValue ) throws RepositoryException {
        Property property = node.getProperty(propertyName);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertEquals(expectedValue, java.sql.Time.valueOf(value.getString()));
    }

    protected boolean verifyHasProperty( Node node, String propNameStr ) throws RepositoryException {
        return node.hasProperty(propNameStr);
    }

    protected void verifyPrimaryType( Node node, String expectedValue ) throws RepositoryException {
        verifyProperty(node, JCR_PRIMARY_TYPE, expectedValue);
    }

    protected void verifyMixinType( Node node, String expectedValue ) throws RepositoryException {
        verifyProperty(node, JCR_MIXIN_TYPES, expectedValue);
    }
    
    protected void verifyMixinTypes( Node node, String... expectedValues ) throws RepositoryException {
        Value[] values = node.getProperty(JCR_MIXIN_TYPES).getValues();
        Set<String> valuesSet = new TreeSet<String>();
        for (Value value : values) {
            valuesSet.add(value.getString());
        }
        List<String> expectedValuesList = new ArrayList<String>(Arrays.asList(expectedValues));
        for (Iterator<String> expectedValuesIterator = expectedValuesList.iterator(); expectedValuesIterator.hasNext();) {
            assertTrue(valuesSet.contains(expectedValuesIterator.next()));
            expectedValuesIterator.remove();
        }
        assertTrue(expectedValuesList.isEmpty());
    }

    protected void verifyExpression( Node node, String expectedValue ) throws Exception {
//        verifyProperty(node, DDL_EXPRESSION, expectedValue);
    }

    protected void verifyBaseProperties( Node node, String primaryType, String mixinType) throws RepositoryException {
        verifyPrimaryType(node, primaryType);
        verifyMixinType(node, mixinType);
    }

    protected Node findNode( Node parent, String nodePath, String... mixinTypes ) throws Exception {
        Node child = parent.getNode(nodePath);
        assertNotNull(child);
        verifyMixinTypes(child, mixinTypes);
        return child;
    }

    private void traverse(String prefix, Node node, StringBuffer buffer) throws Exception {
        buffer.append(prefix + node.getName() + NEW_LINE);

        PropertyIterator propertyIterator = node.getProperties();
        while(propertyIterator.hasNext()) {
            Property property = propertyIterator.nextProperty();
            buffer.append(prefix + prefix + property.toString() + NEW_LINE);
        }

        NodeIterator children = node.getNodes();
        while(children.hasNext()) {
            traverse(prefix + TAB, children.nextNode(), buffer);
        }
    }

    protected void traverse(Node node) throws Exception {
        StringBuffer buffer = new StringBuffer();
        traverse(EMPTY_STRING, node, buffer);
        KLog.getLogger().info(buffer.toString());
    }

    protected File wrapSQLText(String sql) throws Exception {
        File tmpFile = File.createTempFile(TeiidSqlLexicon.Namespace.PREFIX, DOT + "tsql");
        tmpFile.deleteOnExit();
        FileWriter fw = new FileWriter(tmpFile);
        fw.write(sql);
        fw.close();
        return tmpFile;
    }

    protected Node sequenceSql( File sqlFile ) throws Exception {
        String fileName = sqlFile.getName();
        createNodeWithContentFromFile(fileName, sqlFile);

        Node fileNode = session().getNode(FORWARD_SLASH + fileName);
        assertNotNull(fileNode);

        Node contentNode = fileNode.getNode(JcrConstants.JCR_CONTENT);
        assertNotNull(contentNode);

        Property teiidVersion = fileNode.getProperty(TeiidSqlLexicon.TEIID_VERSION_PROPERTY);
        assertNotNull(teiidVersion);

        Property content = contentNode.getProperty(JcrConstants.JCR_DATA);
        assertNotNull(content);

        boolean success = session().sequence("Teiid SQL Sequencer", content, fileNode);
        assertTrue(success);

        return fileNode;
    }

    protected Node sequenceSql(String sqlText) throws Exception {
        File sqlFile = wrapSQLText(sqlText);
        Node fileNode = sequenceSql(sqlFile);
        assertNotNull(fileNode);
        return fileNode;
    }

    protected String enc(String input) {
        return session().encode(input);
    }

    protected Node verify(Node parentNode, String relativePath, int index, String mixinType) throws Exception {
        String indexExp = EMPTY_STRING;
        if (index > -1)
            indexExp = OPEN_SQUARE_BRACKET + index + CLOSE_SQUARE_BRACKET;

        Node childNode = null;
        if (parentNode.hasNode(relativePath)) {
            childNode = parentNode.getNode(relativePath + indexExp);
        } else
            childNode = parentNode.getNode(enc(relativePath ) + indexExp);
        assertNotNull(childNode);

        verifyBaseProperties(childNode, JcrConstants.NT_UNSTRUCTURED, mixinType);
        return childNode;
    }

    protected Node verify(Node parentNode, String relativePath, String mixinType) throws Exception {
        return verify(parentNode, relativePath, -1, mixinType);
    }

    protected void verifyJoin(Node joinPredicate, IJoinType.Types joinType) throws Exception {
        Node joinNode = verify(joinPredicate, JoinPredicate.JOIN_TYPE_REF_NAME, JoinType.ID);
        verifyProperty(joinNode, TeiidSqlLexicon.JoinType.KIND_PROP_NAME, joinType.name());
    }

    protected void verifyUnaryFromClauseGroup(Node jpNode, String refName, int refIndex, String... gSymbolProps) throws Exception {
        Node refNode = verify(jpNode, refName, refIndex, UnaryFromClause.ID);
        Node groupNode = verify(refNode, UnaryFromClause.GROUP_REF_NAME, GroupSymbol.ID);

        String name = gSymbolProps[0];
        verifyProperty(groupNode, Symbol.NAME_PROP_NAME, name);

        if (gSymbolProps.length > 1) {
            String definition = gSymbolProps[1];
            verifyProperty(groupNode, GroupSymbol.DEFINITION_PROP_NAME, definition);
        }
    }

    protected void verifyUnaryFromClauseGroup(Node jpNode, String refName, String... gSymbolProps) throws Exception {
        verifyUnaryFromClauseGroup(jpNode, refName, -1, gSymbolProps);
    }

    protected void verifyConstant(Node parentNode, String refName, int refIndex, String literal) throws Exception {
        Node constantNode = verify(parentNode, refName, refIndex, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, literal);
    }

    protected void verifyConstant(Node parentNode, String refName, String literal) throws Exception {
        verifyConstant(parentNode, refName, -1, literal);
    }

    protected void verifyConstant(Node parentNode, String refName, int refIndex, int literal) throws Exception {
        Node constantNode = verify(parentNode, refName, refIndex, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, literal);
    }

    protected void verifyConstant(Node parentNode, String refName, int literal) throws Exception {
        verifyConstant(parentNode, refName, -1, literal);
    }

    protected void verifyElementSymbol(Node parentNode, String refName, int refIndex, String elementSymbolName) throws Exception {
        Node elementSymbolNode = verify(parentNode, refName, refIndex, ElementSymbol.ID);
        verifyProperty(elementSymbolNode, Symbol.NAME_PROP_NAME, elementSymbolName);
    }

    protected void verifyElementSymbol(Node parentNode, String refName, String elementSymbolName) throws Exception {
        verifyElementSymbol(parentNode, refName, -1, elementSymbolName);
    }

    protected Node verifyAliasSymbol(Node parentNode, String refName, int refIndex, String aliasName, String symbolId) throws Exception {
        Node aliasNode = verify(parentNode, refName, refIndex, AliasSymbol.ID);
        verifyProperty(aliasNode, Symbol.NAME_PROP_NAME, aliasName);
        return verify(aliasNode, AliasSymbol.SYMBOL_REF_NAME, symbolId);
    }

    protected Node verifyAliasSymbol(Node parentNode, String refName, String aliasName, String symbolId) throws Exception {
        return verifyAliasSymbol(parentNode, refName, -1, aliasName, symbolId);
    }

    protected void verifyAliasSymbolWithElementSymbol(Node parentNode, String refName, int refIndex, String aliasName, String elementSymbolName) throws Exception {
        Node aliasNode = verify(parentNode, refName, refIndex, AliasSymbol.ID);
        verifyProperty(aliasNode, Symbol.NAME_PROP_NAME, aliasName);
        Node elementSymbolNode = verify(aliasNode, AliasSymbol.SYMBOL_REF_NAME, ElementSymbol.ID);
        verifyProperty(elementSymbolNode, Symbol.NAME_PROP_NAME, elementSymbolName);
    }

    protected Node verifyExpressionSymbol(Node parentNode, String refName, int refIndex, String expSymbolExpressionId) throws Exception {
        Node expSymbolNode = verify(parentNode, refName, refIndex, ExpressionSymbol.ID);

        Property property = expSymbolNode.getProperty(Symbol.NAME_PROP_NAME);
        Value value = property.isMultiple() ? property.getValues()[0] : property.getValue();
        assertTrue(value.toString().startsWith("expr"));

        return verify(expSymbolNode, ExpressionSymbol.EXPRESSION_REF_NAME, expSymbolExpressionId);
    }

    protected Node verifyExpressionSymbol(Node parentNode, String refName, String expSymbolExpressionId) throws Exception {
        return verifyExpressionSymbol(parentNode, refName, -1, expSymbolExpressionId);
    }

    protected String deriveProcPrefix() {
        String procPrefix = "BEGIN ";
        if (getTeiidVersion().isLessThan(TeiidVersion.Version.TEIID_8_4.get()))
            procPrefix = "CREATE VIRTUAL PROCEDURE " + procPrefix;

        return procPrefix;
    }

    @Test
    public void testInnerJoin() throws Exception {
        String sql =  "SELECT * FROM g1 inner join g2 on g1.a1=g2.a2";
        Node fileNode = sequenceSql(sql);
        
        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        // Query should have a SELECT
        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        // Select should have a symbols collection
        // Select has a * so symbolsNode should be a MultipleElementSymbol
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        // Should have a FROM
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        // Clause node in FROM is actually a JoinPredicate
        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);

        // JoinPredicate should have a JOIN
        verifyJoin(jpNode, IJoinType.Types.JOIN_INNER);

        // Join Predicate should have a left clause
        // UnaryFromClause should have a group
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g2");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "g1.a1");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "g2.a2");
    }

    /** SELECT * FROM g1 cross join g2 */
    @Test
    public void testCrossJoin() throws Exception {
        String sql = "SELECT * FROM g1 cross join g2";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);
        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, IJoinType.Types.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g2");
    }

    /** SELECT * FROM (g1 cross join g2), g3 */
    @Test
    public void testFromClauses() throws Exception {
        String sql = "SELECT * FROM (g1 cross join g2), g3";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);
        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, 1, JoinPredicate.ID);
        verifyJoin(jpNode, IJoinType.Types.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g2");

        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "g3");
    }

    /** SELECT * FROM (g1 cross join g2) cross join g3 */
    @Test
    public void testMultiCrossJoin() throws Exception {
        String sql = "SELECT * FROM (g1 cross join g2) cross join g3";
        Node fileNode = sequenceSql(sql);
        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode1 = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode1, IJoinType.Types.JOIN_CROSS);

        Node jpNode2 = verify(jpNode1, JoinPredicate.LEFT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode2, IJoinType.Types.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g2");
        verifyUnaryFromClauseGroup(jpNode1, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g3");
    }

    /** SELECT * FROM (g1 cross join g2) cross join (g3 cross join g4) */
    @Test
    public void testMultiCrossJoin2() throws Exception {
        String sql = "SELECT * FROM (g1 cross join g2) cross join (g3 cross join g4)";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode1 = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode1, IJoinType.Types.JOIN_CROSS);

        Node jpNode2 = verify(jpNode1, JoinPredicate.LEFT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode2, IJoinType.Types.JOIN_CROSS);

        Node jpNode3 = verify(jpNode1, JoinPredicate.RIGHT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode3, IJoinType.Types.JOIN_CROSS);
        
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g2");

        verifyUnaryFromClauseGroup(jpNode3, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g3");
        verifyUnaryFromClauseGroup(jpNode3, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g4");
    }

    /** SELECT * FROM g1 cross join (g2 cross join g3) */
    @Test
    public void testMultiCrossJoin3() throws Exception {
        String sql = "SELECT * FROM g1 cross join (g2 cross join g3)";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode1 = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode1, IJoinType.Types.JOIN_CROSS);
        
        Node jpNode2 = verify(jpNode1, JoinPredicate.RIGHT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode2, IJoinType.Types.JOIN_CROSS);
        
        verifyUnaryFromClauseGroup(jpNode1, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");

        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g2");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g3");
    }

    /** SELECT * FROM g1 cross join (g2 cross join g3), g4 */
    @Test
    public void testMixedJoin() throws Exception {
        String sql = "SELECT * FROM g1 cross join (g2 cross join g3), g4";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode1 = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode1, IJoinType.Types.JOIN_CROSS);

        Node jpNode2 = verify(jpNode1, JoinPredicate.RIGHT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode2, IJoinType.Types.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode1, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g2");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g3");
        
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "g4");
    }

    /** SELECT * FROM g1 cross join (g2 cross join g3), g4, g5 cross join g6 */
    @Test
    public void testMixedJoin2() throws Exception {
        String sql = "SELECT * FROM g1 cross join (g2 cross join g3), g4, g5 cross join g6";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode1 = verify(fromNode, From.CLAUSES_REF_NAME, 1, JoinPredicate.ID);
        verifyJoin(jpNode1, IJoinType.Types.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode1, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        
        Node jpNode2 = verify(jpNode1, JoinPredicate.RIGHT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode2, IJoinType.Types.JOIN_CROSS);
        
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g2");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g3");

        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "g4");

        Node jpNode3 = verify(fromNode, From.CLAUSES_REF_NAME, 3, JoinPredicate.ID);
        verifyJoin(jpNode3, IJoinType.Types.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode3, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g5");
        verifyUnaryFromClauseGroup(jpNode3, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g6");
    }

    /** SELECT * FROM g1, g2 inner join g3 on g2.a=g3.a */
    @Test
    public void testMixedJoin3() throws Exception {
        String sql = "SELECT * FROM g1, g2 inner join g3 on g2.a=g3.a";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g1");

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, 2, JoinPredicate.ID);
        verifyJoin(jpNode, IJoinType.Types.JOIN_INNER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g2");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g3");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "g2.a");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "g3.a");
    }

    /** Select myG.a myA, myH.b from g myG right outer join h myH on myG.x=myH.x */
    @Test
    public void testRightOuterJoinWithAliases() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG right outer join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, IJoinType.Types.JOIN_RIGHT_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    }

    /** Select myG.x myX, myH.y from g myG right join h myH on myG.x=myH.x */
    @Test
    public void testRightJoinWithAliases() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG right join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, IJoinType.Types.JOIN_RIGHT_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    }

    /** Select myG.a myA, myH.b from g myG left outer join h myH on myG.x=myH.x */
    @Test
    public void testLeftOuterJoinWithAliases() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG left outer join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, IJoinType.Types.JOIN_LEFT_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    }

    /** Select myG.a myA, myH.b from g myG left join h myH on myG.x=myH.x */
    @Test
    public void testLeftJoinWithAliases() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG left outer join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, IJoinType.Types.JOIN_LEFT_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    }

    /** Select myG.a myA, myH.b from g myG full outer join h myH on myG.x=myH.x */
    @Test
    public void testFullOuterJoinWithAliases() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG full outer join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, IJoinType.Types.JOIN_FULL_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    }

    /** Select g.a, h.b from g full join h on g.x=h.x */
    @Test
    public void testFullJoin() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG full join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, IJoinType.Types.JOIN_FULL_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    }

    // ======================= Convert ==============================================

    /** SELECT CONVERT(a, string) FROM g */
    @Test
    public void testConversionFunction() throws Exception {
        String sql = "SELECT CONVERT(a, string) FROM g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node functionNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Function.ID);
        verifyProperty(functionNode, Function.NAME_PROP_NAME, "CONVERT");

        verifyElementSymbol(functionNode, Function.ARGS_REF_NAME, 1, "a");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 2, "string");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

    /** SELECT CONVERT(CONVERT(a, timestamp), string) FROM g */
    @Test
    public void testConversionFunction2() throws Exception {
        String sql = "SELECT CONVERT(CONVERT(a, timestamp), string) FROM g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node function1Node = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Function.ID);
        verifyProperty(function1Node, Function.NAME_PROP_NAME, "CONVERT");

        Node function2Node = verify(function1Node, Function.ARGS_REF_NAME, 1, Function.ID);
        verifyProperty(function2Node, Function.NAME_PROP_NAME, "CONVERT");

        verifyElementSymbol(function2Node, Function.ARGS_REF_NAME, 1, "a");
        verifyConstant(function2Node, Function.ARGS_REF_NAME, 2, "timestamp");

        verifyConstant(function1Node, Function.ARGS_REF_NAME, 2, "string");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

    // ======================= Functions ==============================================

    /** SELECT 5 + length(concat(a, 'x')) FROM g */
    @Test
    public void testMultiFunction() throws Exception {
        String sql = "SELECT 5 + length(concat(a, 'x')) FROM g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node function1Node = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Function.ID);
        verifyProperty(function1Node, Function.NAME_PROP_NAME, "+");
        verifyConstant(function1Node, Function.ARGS_REF_NAME, 1, 5);
        
        Node function2Node = verify(function1Node, Function.ARGS_REF_NAME, 2, Function.ID);
        verifyProperty(function2Node, Function.NAME_PROP_NAME, "length");
        
        Node function3Node = verify(function2Node, Function.ARGS_REF_NAME, Function.ID);
        verifyProperty(function3Node, Function.NAME_PROP_NAME, "concat");
        verifyElementSymbol(function3Node, Function.ARGS_REF_NAME, 1, "a");
        verifyConstant(function3Node, Function.ARGS_REF_NAME, 2, "x");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

    /** SELECT REPLACE(a, 'x', 'y') AS y FROM g */
    @Test
    public void testAliasedFunction() throws Exception {
        String sql = "SELECT REPLACE(a, 'x', 'y') AS y FROM g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node functionNode = verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, "y", Function.ID);
        verifyProperty(functionNode, Function.NAME_PROP_NAME, "REPLACE");
        verifyElementSymbol(functionNode, Function.ARGS_REF_NAME,  1, "a");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 2, "x");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 3, "y");
        
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

    /** SELECT cast(a as string) FROM g */
    @Test
    public void testCastFunction() throws Exception {
        String sql = "SELECT cast(a as string) FROM g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node functionNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Function.ID);
        verifyProperty(functionNode, Function.NAME_PROP_NAME, "cast");
        verifyElementSymbol(functionNode, Function.ARGS_REF_NAME,  1, "a");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 2, "string");
        
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

    /** SELECT cast(cast(a as timestamp) as string) FROM g */
    @Test
    public void testMultiCastFunction() throws Exception {
        String sql = "SELECT cast(cast(a as timestamp) as string) FROM g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node function1Node = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Function.ID);
        verifyProperty(function1Node, Function.NAME_PROP_NAME, "cast");
        Node function2Node = verify(function1Node, Function.ARGS_REF_NAME, 1, Function.ID);
        verifyConstant(function1Node, Function.ARGS_REF_NAME, 2, "string");

        verifyElementSymbol(function2Node, Function.ARGS_REF_NAME, 1, "a");
        verifyConstant(function2Node, Function.ARGS_REF_NAME, 2, "timestamp");
        
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

    /** SELECT left(fullname, 3) as x FROM sys.groups */
    @Test
    public void testLeftFunction() throws Exception {
        String sql = "SELECT left(fullname, 3) as x FROM sys.groups";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        Node functionNode = verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, "x", Function.ID);
        verifyProperty(functionNode, Function.NAME_PROP_NAME, "left");
        verifyElementSymbol(functionNode, Function.ARGS_REF_NAME,  1, "fullname");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 2, 3);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "sys.groups");
    }

    /** SELECT right(fullname, 3) as x FROM sys.groups */
    @Test
    public void testRightFunction() throws Exception {
        String sql = "SELECT right(fullname, 3) as x FROM sys.groups";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        Node functionNode = verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, "x", Function.ID);
        verifyProperty(functionNode, Function.NAME_PROP_NAME, "right");
        verifyElementSymbol(functionNode, Function.ARGS_REF_NAME,  1, "fullname");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 2, 3);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "sys.groups");
    }

    @Test
    public void testInsertIntoSelect() throws Exception {
        String sql = "insert into tempA SELECT 1";
        Node fileNode = sequenceSql(sql);

        Node insertNode = verify(fileNode, Insert.ID, Insert.ID);
        Node gsNode = verify(insertNode, TargetedCommand.GROUP_REF_NAME, GroupSymbol.ID);
        verifyProperty(gsNode, Symbol.NAME_PROP_NAME, "tempA");

        Node queryNode = verify(insertNode, Insert.QUERY_EXPRESSION_REF_NAME, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, 1);
    }

    // ======================= Group By ==============================================

    /** SELECT a FROM m.g GROUP BY b, c HAVING b=5*/
    @Test
    public void testGroupByHaving() throws Exception {
        String sql = "SELECT a FROM m.g GROUP BY b, c HAVING b=5";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "m.g");

        Node groupByNode = verify(queryNode, Query.GROUP_BY_REF_NAME, GroupBy.ID);
        verifyElementSymbol(groupByNode, GroupBy.SYMBOLS_REF_NAME,  1, "b");
        verifyElementSymbol(groupByNode, GroupBy.SYMBOLS_REF_NAME,  2, "c");

        Node havingNode = verify(queryNode, Query.HAVING_REF_NAME, CompareCriteria.ID);
        verifyProperty(havingNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());
        verifyElementSymbol(havingNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "b");
        verifyConstant(havingNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, 5);
    }

    /** SELECT COUNT(a) AS c FROM m.g */
    @Test
    public void testAggregateFunction() throws Exception {
        String sql = "SELECT COUNT(a) AS c FROM m.g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node aggregateNode = verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, "c", AggregateSymbol.ID);
        verifyProperty(aggregateNode, AggregateSymbol.AGGREGATE_FUNCTION_PROP_NAME, "COUNT");
        verifyProperty(aggregateNode, AggregateSymbol.DISTINCT_PROP_NAME, false);
        verifyElementSymbol(aggregateNode, AggregateSymbol.ARGS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "m.g");
    }

    /** SELECT a FROM m.g GROUP BY a HAVING COUNT(b) > 0*/
    @Test
    public void testHavingFunction() throws Exception {
        String sql = "SELECT a FROM m.g GROUP BY a HAVING COUNT(b) > 0";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "m.g");

        Node groupByNode = verify(queryNode, Query.GROUP_BY_REF_NAME, GroupBy.ID);
        verifyElementSymbol(groupByNode, GroupBy.SYMBOLS_REF_NAME,  1, "a");

        Node havingNode = verify(queryNode, Query.HAVING_REF_NAME, CompareCriteria.ID);
        verifyProperty(havingNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.GT.name());

        Node aggregateNode = verify(havingNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, AggregateSymbol.ID);
        verifyProperty(aggregateNode, AggregateSymbol.AGGREGATE_FUNCTION_PROP_NAME, "COUNT");
        verifyProperty(aggregateNode, AggregateSymbol.DISTINCT_PROP_NAME, false);
        verifyElementSymbol(aggregateNode, AggregateSymbol.ARGS_REF_NAME, "b");
        
        verifyConstant(havingNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, 0);
    }

    /** SELECT 5-null, a.g1.c1 FROM a.g1 */
    @Test
    public void testArithmeticNullFunction() throws Exception {
        String sql = "SELECT 5-null, a.g1.c1 FROM a.g1";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node functionNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Function.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "a.g1.c1");

        verifyProperty(functionNode, Function.NAME_PROP_NAME, "-");
        verifyConstant(functionNode, Function.ARGS_REF_NAME,  1, 5);
        
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    }

    /** SELECT 'abc' FROM a.g1 */
    @Test
    public void testStringLiteral() throws Exception {
        String sql = "SELECT 'abc' FROM a.g1";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, "abc");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    }

    /** SELECT 'O''Leary' FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick() throws Exception {
        String sql = "SELECT 'O''Leary' FROM a.g1";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, "O'Leary");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    }

    /** SELECT '''abc''' FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick2() throws Exception {
        String sql = "SELECT '''abc''' FROM a.g1";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, "'abc'");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    }

    /** SELECT 'a''b''c' FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick3() throws Exception {
        String sql = "SELECT 'a''b''c' FROM a.g1";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, "a'b'c");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    }

    /** SELECT " "" " FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick4() throws Exception {
        String sql = "SELECT \" \"\" \" FROM a.g1";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, " \" ");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    }

    /** SELECT 123456789012 FROM a.g1 */
    @Test
    public void testLongLiteral() throws Exception {
        String sql = "SELECT 123456789012 FROM a.g1";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, 123456789012L);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    }

    /** SELECT {d'2002-10-02'} FROM m.g1 */
    @Test
    public void testDateLiteral1() throws Exception {
        String sql = "SELECT {d'2002-10-02'} FROM m.g1";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, java.sql.Date.valueOf("2002-10-02"));

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "m.g1");
    }

    /** SELECT {t '11:10:00' } FROM m.g1 */
    @Test
    public void testTimeLiteral1() throws Exception {
        String sql = "SELECT {t '11:10:00' } FROM m.g1";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, java.sql.Time.valueOf("11:10:00"));

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "m.g1");
    }

    /** SELECT {b'true'} FROM m.g1 */
    @Test
    public void testBooleanLiteralTrue() throws Exception {
        Boolean expected = Boolean.TRUE;
        String sql = "SELECT {b'true'}";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, expected);
        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.BOOLEAN.name());
    }

    /** SELECT TRUE FROM m.g1 */
    @Test
    public void testBooleanLiteralTrue2() throws Exception {
        Boolean expected = Boolean.TRUE;
        String sql = "SELECT TRUE";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, expected);
        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.BOOLEAN.name());
    }

    /** SELECT {b'false'} FROM m.g1 */
    @Test
    public void testBooleanLiteralFalse() throws Exception {
        Boolean expected = Boolean.FALSE;
        String sql = "SELECT {b'false'}";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, expected);
        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.BOOLEAN.name());
    }

    /** SELECT FALSE FROM m.g1 */
    @Test
    public void testBooleanLiteralFalse2() throws Exception {
        Boolean expected = Boolean.FALSE;
        String sql = "SELECT {b'false'}";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, expected);
        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.BOOLEAN.name());
    }

    @Test
    public void testBooleanLiteralUnknown() throws Exception {
        String sql = "SELECT {b'unknown'}";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        assertFalse(constantNode.hasProperty(Constant.VALUE_PROP_NAME));

        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.BOOLEAN.name());
    }

    /** SELECT DISTINCT a FROM g */
    @Test
    public void testSelectDistinct() throws Exception {
        String sql = "SELECT DISTINCT a FROM g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");
        verifyProperty(selectNode, Select.DISTINCT_PROP_NAME, true);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

    /** SELECT ALL a FROM g */
    @Test
    public void testSelectAll() throws Exception {
        String sql = "SELECT ALL a FROM g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");
        verifyProperty(selectNode, Select.DISTINCT_PROP_NAME, false);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

    //=========================Aliasing==============================================


    /** SELECT myG.a FROM g AS myG */
    @Test
    public void testAliasInFrom() throws Exception {
        String sql = "SELECT myG.a FROM g AS myG";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "myG.a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "myG", "g");
    }

    /** SELECT myG.*, myH.b FROM g AS myG, h AS myH */
    @Test
    public void testAliasesInFrom() throws Exception {
        String sql = "SELECT myG.*, myH.b FROM g AS myG, h AS myH";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node meSymbolNode = verify(selectNode, Select.SYMBOLS_REF_NAME, 1, MultipleElementSymbol.ID);
        verifyProperty(meSymbolNode, Symbol.NAME_PROP_NAME, "myG");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "myG", "g");
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "myH", "h");
    }

    /** SELECT myG.a, myH.b FROM g myG, h myH */
    @Test
    public void testHiddenAliasesInFrom() throws Exception {
        String sql = "SELECT myG.*, myH.b FROM g myG, h myH";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node meSymbolNode = verify(selectNode, Select.SYMBOLS_REF_NAME, 1, MultipleElementSymbol.ID);
        verifyProperty(meSymbolNode, Symbol.NAME_PROP_NAME, "myG");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "myG", "g");
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "myH", "h");
    }

    // ======================= Misc ==============================================

    /** Select a From db.g Where a IS NULL */
    @Test
    public void testIsNullCriteria1() throws Exception {
        String sql = "Select a From db.g Where a IS NULL";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, IsNullCriteria.ID);
        verifyElementSymbol(criteriaNode, IsNullCriteria.EXPRESSION_REF_NAME, "a");
    }

    /** Select a From db.g Where a IS NOT NULL */
    @Test
    public void testIsNullCriteria2() throws Exception {
        String sql = "Select a From db.g Where a IS NOT NULL";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, IsNullCriteria.ID);
        verifyElementSymbol(criteriaNode, IsNullCriteria.EXPRESSION_REF_NAME, "a");
        verifyProperty(criteriaNode, IsNullCriteria.NEGATED_PROP_NAME, true);
    }

    /** Select a From db.g Where Not a IS NULL */
    @Test
    public void testNotIsNullCriteria() throws Exception {
        String sql = "Select a From db.g Where Not a IS NULL";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node notCriteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, NotCriteria.ID);
        
        Node isNullCriteriaNode = verify(notCriteriaNode, NotCriteria.CRITERIA_REF_NAME, IsNullCriteria.ID);
        verifyElementSymbol(isNullCriteriaNode, IsNullCriteria.EXPRESSION_REF_NAME, "a");
    }

    /** SELECT a from db.g where a <> "value" */
    @Test
    public void testStringNotEqualDoubleTicks() throws Exception {
        String sql = "SELECT a from db.g where a <> \"value\"";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.NE.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "a");
        verifyElementSymbol(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "value");
    }

    /** SELECT a from db.g where a != "value" */
    @Test
    public void testNotEquals2() throws Exception {
        String sql = "SELECT a from db.g where a != 'value'";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.NE.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "a");
        verifyConstant(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "value");
    }

    /** SELECT a from db."g" where a = 5 */
    @Test
    public void testPartlyQuotedGroup() throws Exception {
        String sql = "SELECT a from db.\"g\" where a = 5";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "a");
        verifyConstant(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, 5);
    }

    /** SELECT * FROM model.doc WHERE ab.cd.@ef = 'abc' */
    @Test
    public void testXMLCriteriaWithAttribute() throws Exception {
        String sql = "SELECT * FROM model.doc WHERE ab.cd.@ef = 'abc'";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "model.doc");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "ab.cd.@ef");
        verifyConstant(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "abc");
    }

    /** SELECT a from db.g where a BETWEEN 1000 AND 2000 */
    @Test
    public void testBetween1() throws Exception {
        String sql = "SELECT a from db.g where a BETWEEN 1000 AND 2000";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, BetweenCriteria.ID);
        verifyElementSymbol(criteriaNode, BetweenCriteria.EXPRESSION_REF_NAME, "a");
        verifyConstant(criteriaNode, BetweenCriteria.LOWER_EXPRESSION_REF_NAME, 1000);
        verifyConstant(criteriaNode, BetweenCriteria.UPPER_EXPRESSION_REF_NAME, 2000);
    }

    /** SELECT a FROM db.g WHERE b IN (1000,5000)*/
    @Test
    public void testSetCriteria0() throws Exception {
        String sql = "SELECT a FROM db.g WHERE b IN (1000,5000)";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, SetCriteria.ID);
        verifyElementSymbol(criteriaNode, AbstractSetCriteria.EXPRESSION_REF_NAME, "b");
        verifyConstant(criteriaNode, SetCriteria.VALUES_REF_NAME, 1, 1000);
        verifyConstant(criteriaNode, SetCriteria.VALUES_REF_NAME, 2, 5000);
    }

    // ================================== order by ==================================

    /** SELECT a FROM db.g WHERE b = aString order by c desc*/
    @Test
    public void testOrderByDesc() throws Exception {
        String sql = "SELECT a FROM db.g WHERE b = aString ORDER BY c desc";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "b");
        verifyElementSymbol(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "aString");    

        Node orderByNode = verify(queryNode, QueryCommand.ORDER_BY_REF_NAME, OrderBy.ID);
        Node obItemNode = verify(orderByNode, OrderBy.ORDER_BY_ITEMS_REF_NAME, OrderByItem.ID);
        verifyElementSymbol(obItemNode, OrderByItem.SYMBOL_REF_NAME, "c");
        verifyProperty(obItemNode, OrderByItem.ASCENDING_PROP_NAME, false);
    }

    @Test
    public void testOrderByNullOrdering() throws Exception {
        String sql = "SELECT a FROM db.g WHERE b = aString ORDER BY c NULLS FIRST,d desc nulls last";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "b");
        verifyElementSymbol(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "aString");

        Node orderByNode = verify(queryNode, QueryCommand.ORDER_BY_REF_NAME, OrderBy.ID);
        Node obItem1Node = verify(orderByNode, OrderBy.ORDER_BY_ITEMS_REF_NAME, 1, OrderByItem.ID);
        verifyElementSymbol(obItem1Node, OrderByItem.SYMBOL_REF_NAME, "c");
        verifyProperty(obItem1Node, OrderByItem.ASCENDING_PROP_NAME, true);
        verifyProperty(obItem1Node, OrderByItem.NULL_ORDERING_PROP_NAME, NullOrdering.FIRST.name());
        Node obItem2Node = verify(orderByNode, OrderBy.ORDER_BY_ITEMS_REF_NAME, 2, OrderByItem.ID);
        verifyElementSymbol(obItem2Node, OrderByItem.SYMBOL_REF_NAME, "d");
        verifyProperty(obItem2Node, OrderByItem.ASCENDING_PROP_NAME, false);
        verifyProperty(obItem2Node, OrderByItem.NULL_ORDERING_PROP_NAME, NullOrdering.LAST.name());
    }

//    // ================================== match ====================================


    /** SELECT a from db.g where b like '#String' escape '#'*/
    @Test
    public void testLikeWithEscape() throws Exception {
        String sql = "SELECT a from db.g where b like '#String' escape '#'";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, MatchCriteria.ID);
        verifyProperty(criteriaNode, MatchCriteria.ESCAPE_CHAR_PROP_NAME, "#");
        verifyElementSymbol(criteriaNode, MatchCriteria.LEFT_EXPRESSION_REF_NAME, "b");
        verifyConstant(criteriaNode, MatchCriteria.RIGHT_EXPRESSION_REF_NAME, "#String");
    }

    /** SELECT a */
    @Test
    public void testNoFromClause() throws Exception {
        String sql = "SELECT a, 5";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, 5);
        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.INTEGER.name());
    }

    /** INSERT INTO m.g (a) VALUES (?) */
    @Test
    public void testInsertWithReference() throws Exception {
        String sql = "INSERT INTO m.g (a) VALUES (?)";
        Node fileNode = sequenceSql(sql);

        Node insertNode = verify(fileNode, Insert.ID, Insert.ID);
        Node gsNode = verify(insertNode, TargetedCommand.GROUP_REF_NAME, GroupSymbol.ID);
        verifyProperty(gsNode, Symbol.NAME_PROP_NAME, "m.g");
        verifyElementSymbol(insertNode, Insert.VARIABLES_REF_NAME, "a");
        Node refNode = verify(insertNode, Insert.VALUES_REF_NAME, Reference.ID);
        verifyProperty(refNode, Reference.INDEX_PROP_NAME, 0);
        verifyProperty(refNode, Reference.POSITIONAL_PROP_NAME, true);
    }

    @Test
    public void testIfStatement() throws Exception {
        String sql = deriveProcPrefix() + "IF(c = 5) BEGIN DECLARE short a; END ELSE BEGIN DECLARE short b; END END";
        Node fileNode = sequenceSql(sql);

        Node procNode = verify(fileNode, CreateProcedureCommand.ID, CreateProcedureCommand.ID);
        Node outerBlkNode = verify(procNode, CreateProcedureCommand.BLOCK_REF_NAME, Block.ID);
        Node stmtNode = verify(outerBlkNode, Block.STATEMENTS_REF_NAME, IfStatement.ID);

        Node ifBlockNode = verify(stmtNode, IfStatement.IF_BLOCK_REF_NAME, Block.ID);
        Node ifDecStmtNode = verify(ifBlockNode, Block.STATEMENTS_REF_NAME, DeclareStatement.ID);
        verifyElementSymbol(ifDecStmtNode, AssignmentStatement.VARIABLE_REF_NAME, "a");
        verifyProperty(ifDecStmtNode, DeclareStatement.VARIABLE_TYPE_PROP_NAME, "short");

        Node elseBlockNode = verify(stmtNode, IfStatement.ELSE_BLOCK_REF_NAME, Block.ID);
        Node elseDecStmtNode = verify(elseBlockNode, Block.STATEMENTS_REF_NAME, DeclareStatement.ID);
        verifyElementSymbol(elseDecStmtNode, AssignmentStatement.VARIABLE_REF_NAME, "b");
        verifyProperty(ifDecStmtNode, DeclareStatement.VARIABLE_TYPE_PROP_NAME, "short");
        
        Node criteriaNode = verify(stmtNode, IfStatement.CONDITION_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "c");
        verifyConstant(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, 5);
    }

    @Test
    public void testDynamicCommandStatement() throws Exception {
        String sql = deriveProcPrefix() + "exec string 'SELECT a1 FROM g WHERE a2 = 5' as a1 string into #g; END";
        Node fileNode = sequenceSql(sql);

        Node procNode = verify(fileNode, CreateProcedureCommand.ID, CreateProcedureCommand.ID);
        Node outerBlkNode = verify(procNode, CreateProcedureCommand.BLOCK_REF_NAME, Block.ID);
        Node cmdStmtNode = verify(outerBlkNode, Block.STATEMENTS_REF_NAME, CommandStatement.ID);

        Node dynCmdNode = verify(cmdStmtNode, SubqueryContainer.COMMAND_REF_NAME, DynamicCommand.ID);
        verifyConstant(dynCmdNode, DynamicCommand.SQL_REF_NAME, "SELECT a1 FROM g WHERE a2 = 5");
        verifyProperty(dynCmdNode, DynamicCommand.AS_CLAUSE_SET_PROP_NAME, true);
        verifyElementSymbol(dynCmdNode, DynamicCommand.AS_COLUMNS_REF_NAME, "a1");
        Node intoGroupNode = verify(dynCmdNode, DynamicCommand.INTO_GROUP_REF_NAME, GroupSymbol.ID);
        verifyProperty(intoGroupNode, Symbol.NAME_PROP_NAME, "#g");
    }

    @Test
    public void testSubquerySetCriteriaWithExec() throws Exception {
        String sql = "SELECT a FROM db.g WHERE b IN (EXEC m.sq1())";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, SubquerySetCriteria.ID);
        verifyElementSymbol(criteriaNode, AbstractSetCriteria.EXPRESSION_REF_NAME, "b");

        Node innerQueryNode = verify(criteriaNode, SubqueryContainer.COMMAND_REF_NAME, Query.ID);
        Node innerSelectNode = verify(innerQueryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(innerSelectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node innerFromNode = verify(innerQueryNode, Query.FROM_REF_NAME, From.ID);
        Node sqFromClause = verify(innerFromNode, From.CLAUSES_REF_NAME, SubqueryFromClause.ID);
        verifyProperty(sqFromClause, SubqueryFromClause.NAME_PROP_NAME, "x");

        Node storedProcNode = verify(sqFromClause, SubqueryContainer.COMMAND_REF_NAME, StoredProcedure.ID);
        verifyProperty(storedProcNode, StoredProcedure.PROCEDURE_NAME_PROP_NAME, "m.sq1");
    }

    @Test
    public void testSubquerySetCriteriaWithUnion() throws Exception {
        String sql = "SELECT a FROM db.g WHERE b IN (SELECT x1 FROM db.g2 UNION ALL SELECT x2 FROM db.g3)";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, SubquerySetCriteria.ID);
        verifyElementSymbol(criteriaNode, AbstractSetCriteria.EXPRESSION_REF_NAME, "b");

        Node unionQueryNode = verify(criteriaNode, SubqueryContainer.COMMAND_REF_NAME, SetQuery.ID);
        verifyProperty(unionQueryNode, SetQuery.ALL_PROP_NAME, true);
        verifyProperty(unionQueryNode, SetQuery.OPERATION_PROP_NAME, ISetQuery.Operation.UNION.name());

        Node u1QueryNode = verify(unionQueryNode, SetQuery.LEFT_QUERY_REF_NAME, Query.ID);
        Node u1SelectNode = verify(u1QueryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(u1SelectNode, Select.SYMBOLS_REF_NAME, "x1");
        Node u1FromNode = verify(u1QueryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(u1FromNode, From.CLAUSES_REF_NAME, "db.g2");

        Node u2QueryNode = verify(unionQueryNode, SetQuery.RIGHT_QUERY_REF_NAME, Query.ID);
        Node u2SelectNode = verify(u2QueryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(u2SelectNode, Select.SYMBOLS_REF_NAME, "x2");
        Node u2FromNode = verify(u2QueryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(u2FromNode, From.CLAUSES_REF_NAME, "db.g3");
    }

    @Test
    public void testLoopStatement() throws Exception {
        String sql = deriveProcPrefix() + "LOOP ON (SELECT c1, c2 FROM m.g) AS mycursor BEGIN DECLARE integer x; x=mycursor.c1; END END";
        Node fileNode = sequenceSql(sql);

        Node procNode = verify(fileNode, CreateProcedureCommand.ID, CreateProcedureCommand.ID);
        Node outerBlkNode = verify(procNode, CreateProcedureCommand.BLOCK_REF_NAME, Block.ID);
        Node loopStmtNode = verify(outerBlkNode, Block.STATEMENTS_REF_NAME, LoopStatement.ID);
        verifyProperty(loopStmtNode, LoopStatement.CURSOR_NAME_PROP_NAME, "mycursor");
        
        Node queryNode = verify(loopStmtNode, SubqueryContainer.COMMAND_REF_NAME, Query.ID);
        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "c1");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "c2");
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "m.g");

        Node blockNode = verify(loopStmtNode, LoopStatement.BLOCK_REF_NAME, Block.ID);

        Node decStmtNode = verify(blockNode, Block.STATEMENTS_REF_NAME, 1, DeclareStatement.ID);
        verifyElementSymbol(decStmtNode, AssignmentStatement.VARIABLE_REF_NAME, "x");
        verifyProperty(decStmtNode, DeclareStatement.VARIABLE_TYPE_PROP_NAME, "integer");

        Node assignStmtNode = verify(blockNode, Block.STATEMENTS_REF_NAME, 2, AssignmentStatement.ID);
        verifyElementSymbol(assignStmtNode, AssignmentStatement.VARIABLE_REF_NAME, "x");
        verifyElementSymbol(assignStmtNode, AssignmentStatement.VALUE_REF_NAME, "mycursor.c1");
    }

    @Test
    public void testXmlElement() throws Exception {
        String sql = "SELECT xmlelement(name \"table\", 'x') FROM g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node xmlElemNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, XMLElement.ID);
        verifyProperty(xmlElemNode, XMLElement.NAME_PROP_NAME, "table");
        verifyConstant(xmlElemNode, XMLElement.CONTENT_REF_NAME, "x");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

    @Test
    public void testXmlElementWithAttributes() throws Exception {
        String sql = "SELECT xmlelement(y, xmlattributes('a' as val)) from g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node xmlElemNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, XMLElement.ID);
        verifyProperty(xmlElemNode, XMLElement.NAME_PROP_NAME, "y");
        Node xmlAttrNode = verify(xmlElemNode, XMLElement.ATTRIBUTES_REF_NAME, XMLAttributes.ID);
        Node derivedColNode = verify(xmlAttrNode, XMLAttributes.ARGS_REF_NAME, DerivedColumn.ID);
        verifyProperty(derivedColNode, DerivedColumn.ALIAS_PROP_NAME, "val");
        verifyConstant(derivedColNode, DerivedColumn.EXPRESSION_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

    @Test
    public void testTextTable() throws Exception {
        String sql = "SELECT * from texttable(file columns x string, y date delimiter ',' escape '\"' header skip 10) as x";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        Node txtTblNode = verify(fromNode, From.CLAUSES_REF_NAME, TextTable.ID);
        verifyElementSymbol(txtTblNode, TextTable.FILE_REF_NAME, "file");
        verifyProperty(txtTblNode, TextTable.SKIP_PROP_NAME, 10);
        verifyProperty(txtTblNode, TableFunctionReference.NAME_PROP_NAME, "x");
        verifyProperty(txtTblNode, TextTable.DELIMITER_PROP_NAME, ",");
        verifyProperty(txtTblNode, TextTable.QUOTE_PROP_NAME, "\"");
        verifyProperty(txtTblNode, TextTable.ESCAPE_PROP_NAME, true);
        verifyProperty(txtTblNode, TextTable.HEADER_PROP_NAME, 1);

        Node txtCol1Node = verify(txtTblNode, TextTable.COLUMNS_REF_NAME, 1, TextColumn.ID);
        verifyProperty(txtCol1Node, ProjectedColumn.NAME_PROP_NAME, "x");
        verifyProperty(txtCol1Node, ProjectedColumn.TYPE_PROP_NAME, "string");

        Node txtCol2Node = verify(txtTblNode, TextTable.COLUMNS_REF_NAME, 2, TextColumn.ID);
        verifyProperty(txtCol2Node, ProjectedColumn.NAME_PROP_NAME, "y");
        verifyProperty(txtCol2Node, ProjectedColumn.TYPE_PROP_NAME, "date");        
    }

    @Test
    public void testWindowFunction() throws Exception {
        String sql = "select row_number() over (partition by x order by y) from g";
        Node fileNode = sequenceSql(sql);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        traverse(queryNode);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node winFnNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, WindowFunction.ID);

        Node aggSymbolNode = verify(winFnNode, WindowFunction.FUNCTION_REF_NAME, AggregateSymbol.ID);
        verifyProperty(aggSymbolNode, AggregateSymbol.NAME_PROP_NAME, "ROW_NUMBER");
        verifyProperty(aggSymbolNode, AggregateSymbol.DISTINCT_PROP_NAME, false);

        Node winSpecNode = verify(winFnNode, WindowFunction.WINDOW_SPECIFICATION_REF_NAME, WindowSpecification.ID);
        verifyElementSymbol(winSpecNode, WindowSpecification.PARTITION_REF_NAME, "x");

        Node orderByNode = verify(winSpecNode, QueryCommand.ORDER_BY_REF_NAME, OrderBy.ID);
        Node obItem1Node = verify(orderByNode, OrderBy.ORDER_BY_ITEMS_REF_NAME, 1, OrderByItem.ID);
        verifyElementSymbol(obItem1Node, OrderByItem.SYMBOL_REF_NAME, "y");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    }

}
