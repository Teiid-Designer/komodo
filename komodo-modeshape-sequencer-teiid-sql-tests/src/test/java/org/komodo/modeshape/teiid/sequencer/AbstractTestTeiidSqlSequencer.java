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

import static org.junit.Assert.assertFalse;
import javax.jcr.Node;
import org.junit.Test;
import org.komodo.modeshape.AbstractTSqlSequencerTest;
import org.komodo.modeshape.teiid.language.SortSpecification.NullOrdering;
import org.komodo.repository.KSequencerController.SequencerType;
import org.komodo.spi.lexicon.TeiidSqlLexicon.AbstractCompareCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.AbstractSetCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.AggregateSymbol;
import org.komodo.spi.lexicon.TeiidSqlLexicon.AssignmentStatement;
import org.komodo.spi.lexicon.TeiidSqlLexicon.BetweenCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Block;
import org.komodo.spi.lexicon.TeiidSqlLexicon.CommandStatement;
import org.komodo.spi.lexicon.TeiidSqlLexicon.CompareCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Constant;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Create;
import org.komodo.spi.lexicon.TeiidSqlLexicon.CreateProcedureCommand;
import org.komodo.spi.lexicon.TeiidSqlLexicon.DeclareStatement;
import org.komodo.spi.lexicon.TeiidSqlLexicon.DerivedColumn;
import org.komodo.spi.lexicon.TeiidSqlLexicon.DynamicCommand;
import org.komodo.spi.lexicon.TeiidSqlLexicon.ElementSymbol;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Expression;
import org.komodo.spi.lexicon.TeiidSqlLexicon.From;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Function;
import org.komodo.spi.lexicon.TeiidSqlLexicon.GroupBy;
import org.komodo.spi.lexicon.TeiidSqlLexicon.GroupSymbol;
import org.komodo.spi.lexicon.TeiidSqlLexicon.IfStatement;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Insert;
import org.komodo.spi.lexicon.TeiidSqlLexicon.IsNullCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.JoinPredicate;
import org.komodo.spi.lexicon.TeiidSqlLexicon.LoopStatement;
import org.komodo.spi.lexicon.TeiidSqlLexicon.MatchCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.MultipleElementSymbol;
import org.komodo.spi.lexicon.TeiidSqlLexicon.NotCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.OrderBy;
import org.komodo.spi.lexicon.TeiidSqlLexicon.OrderByItem;
import org.komodo.spi.lexicon.TeiidSqlLexicon.ProjectedColumn;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Query;
import org.komodo.spi.lexicon.TeiidSqlLexicon.QueryCommand;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Reference;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Select;
import org.komodo.spi.lexicon.TeiidSqlLexicon.SetCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.SetQuery;
import org.komodo.spi.lexicon.TeiidSqlLexicon.StoredProcedure;
import org.komodo.spi.lexicon.TeiidSqlLexicon.SubqueryContainer;
import org.komodo.spi.lexicon.TeiidSqlLexicon.SubqueryFromClause;
import org.komodo.spi.lexicon.TeiidSqlLexicon.SubquerySetCriteria;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Symbol;
import org.komodo.spi.lexicon.TeiidSqlLexicon.TableFunctionReference;
import org.komodo.spi.lexicon.TeiidSqlLexicon.TargetedCommand;
import org.komodo.spi.lexicon.TeiidSqlLexicon.TextColumn;
import org.komodo.spi.lexicon.TeiidSqlLexicon.TextTable;
import org.komodo.spi.lexicon.TeiidSqlLexicon.WindowFunction;
import org.komodo.spi.lexicon.TeiidSqlLexicon.WindowSpecification;
import org.komodo.spi.lexicon.TeiidSqlLexicon.XMLAttributes;
import org.komodo.spi.lexicon.TeiidSqlLexicon.XMLElement;
import org.komodo.spi.query.CriteriaOperator;
import org.komodo.spi.query.JoinTypeTypes;
import org.komodo.spi.query.Operation;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractTestTeiidSqlSequencer extends AbstractTSqlSequencerTest {

    protected static final String TSQL_QUERY = "\\/tsql[0-9]+\\.tsql\\/tsql:query";

    protected static final String TSQL_PROC_CMD = "\\/tsql[0-9]+\\.tsql\\/tsql:createProcedureCommand";

    protected static final String TSQL_INSERT = "\\/tsql[0-9]+\\.tsql\\/tsql:insert";

    /**
     * @param teiidVersion
     */
    public AbstractTestTeiidSqlSequencer(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    protected Node sequenceSql(String text, String seqRegEx) throws Exception {
        Node node = prepareSequence(text, SequencerType.TSQL);
        return node;
    }

    @Test
    public void testInnerJoin() throws Exception {
        String sql =  "SELECT * FROM g1 inner join g2 on g1.a1=g2.a2";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);
        
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
        verifyJoin(jpNode, JoinTypeTypes.JOIN_INNER);

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

        verifySql("SELECT * FROM g1 INNER JOIN g2 ON g1.a1 = g2.a2", fileNode);
    }

    /** SELECT * FROM g1 cross join g2 */
    @Test
    public void testCrossJoin() throws Exception {
        String sql = "SELECT * FROM g1 cross join g2";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);
        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, JoinTypeTypes.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g2");
    
        verifySql("SELECT * FROM g1 CROSS JOIN g2", fileNode);
    }

    /** SELECT * FROM (g1 cross join g2), g3 */
    @Test
    public void testFromClauses() throws Exception {
        String sql = "SELECT * FROM (g1 cross join g2), g3";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);
        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, 1, JoinPredicate.ID);
        verifyJoin(jpNode, JoinTypeTypes.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g2");

        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "g3");
    
        verifySql("SELECT * FROM g1 CROSS JOIN g2, g3", fileNode);
    }

    /** SELECT * FROM (g1 cross join g2) cross join g3 */
    @Test
    public void testMultiCrossJoin() throws Exception {
        String sql = "SELECT * FROM (g1 cross join g2) cross join g3";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);
        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode1 = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode1, JoinTypeTypes.JOIN_CROSS);

        Node jpNode2 = verify(jpNode1, JoinPredicate.LEFT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode2, JoinTypeTypes.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g2");
        verifyUnaryFromClauseGroup(jpNode1, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g3");
    
        verifySql("SELECT * FROM (g1 CROSS JOIN g2) CROSS JOIN g3", fileNode);
    }

    /** SELECT * FROM (g1 cross join g2) cross join (g3 cross join g4) */
    @Test
    public void testMultiCrossJoin2() throws Exception {
        String sql = "SELECT * FROM (g1 cross join g2) cross join (g3 cross join g4)";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode1 = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode1, JoinTypeTypes.JOIN_CROSS);

        Node jpNode2 = verify(jpNode1, JoinPredicate.LEFT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode2, JoinTypeTypes.JOIN_CROSS);

        Node jpNode3 = verify(jpNode1, JoinPredicate.RIGHT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode3, JoinTypeTypes.JOIN_CROSS);
        
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g2");

        verifyUnaryFromClauseGroup(jpNode3, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g3");
        verifyUnaryFromClauseGroup(jpNode3, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g4");
    
        verifySql("SELECT * FROM (g1 CROSS JOIN g2) CROSS JOIN (g3 CROSS JOIN g4)", fileNode);
    }

    /** SELECT * FROM g1 cross join (g2 cross join g3) */
    @Test
    public void testMultiCrossJoin3() throws Exception {
        String sql = "SELECT * FROM g1 cross join (g2 cross join g3)";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode1 = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode1, JoinTypeTypes.JOIN_CROSS);
        
        Node jpNode2 = verify(jpNode1, JoinPredicate.RIGHT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode2, JoinTypeTypes.JOIN_CROSS);
        
        verifyUnaryFromClauseGroup(jpNode1, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");

        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g2");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g3");
    
        verifySql("SELECT * FROM g1 CROSS JOIN (g2 CROSS JOIN g3)", fileNode);
    }

    /** SELECT * FROM g1 cross join (g2 cross join g3), g4 */
    @Test
    public void testMixedJoin() throws Exception {
        String sql = "SELECT * FROM g1 cross join (g2 cross join g3), g4";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode1 = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode1, JoinTypeTypes.JOIN_CROSS);

        Node jpNode2 = verify(jpNode1, JoinPredicate.RIGHT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode2, JoinTypeTypes.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode1, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g2");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g3");
        
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "g4");
    
        verifySql("SELECT * FROM g1 CROSS JOIN (g2 CROSS JOIN g3), g4", fileNode);
    }

    /** SELECT * FROM g1 cross join (g2 cross join g3), g4, g5 cross join g6 */
    @Test
    public void testMixedJoin2() throws Exception {
        String sql = "SELECT * FROM g1 cross join (g2 cross join g3), g4, g5 cross join g6";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode1 = verify(fromNode, From.CLAUSES_REF_NAME, 1, JoinPredicate.ID);
        verifyJoin(jpNode1, JoinTypeTypes.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode1, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g1");
        
        Node jpNode2 = verify(jpNode1, JoinPredicate.RIGHT_CLAUSE_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode2, JoinTypeTypes.JOIN_CROSS);
        
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g2");
        verifyUnaryFromClauseGroup(jpNode2, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g3");

        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "g4");

        Node jpNode3 = verify(fromNode, From.CLAUSES_REF_NAME, 3, JoinPredicate.ID);
        verifyJoin(jpNode3, JoinTypeTypes.JOIN_CROSS);

        verifyUnaryFromClauseGroup(jpNode3, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g5");
        verifyUnaryFromClauseGroup(jpNode3, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g6");
    
        verifySql("SELECT * FROM g1 CROSS JOIN (g2 CROSS JOIN g3), g4, g5 CROSS JOIN g6", fileNode);
    }

    /** SELECT * FROM g1, g2 inner join g3 on g2.a=g3.a */
    @Test
    public void testMixedJoin3() throws Exception {
        String sql = "SELECT * FROM g1, g2 inner join g3 on g2.a=g3.a";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g1");

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, 2, JoinPredicate.ID);
        verifyJoin(jpNode, JoinTypeTypes.JOIN_INNER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "g2");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "g3");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "g2.a");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "g3.a");
    
        verifySql("SELECT * FROM g1, g2 INNER JOIN g3 ON g2.a = g3.a", fileNode);
    }

    /** Select myG.a myA, myH.b from g myG right outer join h myH on myG.x=myH.x */
    @Test
    public void testRightOuterJoinWithAliases() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG right outer join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, JoinTypeTypes.JOIN_RIGHT_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    
        verifySql("SELECT myG.a AS myA, myH.b FROM g AS myG RIGHT OUTER JOIN h AS myH ON myG.x = myH.x", fileNode);
    }

    /** Select myG.x myX, myH.y from g myG right join h myH on myG.x=myH.x */
    @Test
    public void testRightJoinWithAliases() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG right join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, JoinTypeTypes.JOIN_RIGHT_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    
        verifySql("SELECT myG.a AS myA, myH.b FROM g AS myG RIGHT OUTER JOIN h AS myH ON myG.x = myH.x", fileNode);
    }

    /** Select myG.a myA, myH.b from g myG left outer join h myH on myG.x=myH.x */
    @Test
    public void testLeftOuterJoinWithAliases() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG left outer join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, JoinTypeTypes.JOIN_LEFT_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    
        verifySql("SELECT myG.a AS myA, myH.b FROM g AS myG LEFT OUTER JOIN h AS myH ON myG.x = myH.x", fileNode);
    }

    /** Select myG.a myA, myH.b from g myG left join h myH on myG.x=myH.x */
    @Test
    public void testLeftJoinWithAliases() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG left outer join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, JoinTypeTypes.JOIN_LEFT_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    
        verifySql("SELECT myG.a AS myA, myH.b FROM g AS myG LEFT OUTER JOIN h AS myH ON myG.x = myH.x", fileNode);
    }

    /** Select myG.a myA, myH.b from g myG full outer join h myH on myG.x=myH.x */
    @Test
    public void testFullOuterJoinWithAliases() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG full outer join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, JoinTypeTypes.JOIN_FULL_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");
    
        verifySql("SELECT myG.a AS myA, myH.b FROM g AS myG FULL OUTER JOIN h AS myH ON myG.x = myH.x", fileNode);
    }

    /** Select g.a, h.b from g full join h on g.x=h.x */
    @Test
    public void testFullJoin() throws Exception {
        String sql = "Select myG.a myA, myH.b from g myG full join h myH on myG.x=myH.x";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyAliasSymbolWithElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 1, "myA", "myG.a");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);

        Node jpNode = verify(fromNode, From.CLAUSES_REF_NAME, JoinPredicate.ID);
        verifyJoin(jpNode, JoinTypeTypes.JOIN_FULL_OUTER);

        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.LEFT_CLAUSE_REF_NAME, "myG", "g");
        verifyUnaryFromClauseGroup(jpNode, JoinPredicate.RIGHT_CLAUSE_REF_NAME, "myH", "h");

        Node criteriaNode = verify(jpNode, JoinPredicate.JOIN_CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());

        Node leftExpression = verify(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(leftExpression, Symbol.NAME_PROP_NAME, "myG.x");

        Node rightExpression = verify(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, ElementSymbol.ID);
        verifyProperty(rightExpression, Symbol.NAME_PROP_NAME, "myH.x");

        verifySql("SELECT myG.a AS myA, myH.b FROM g AS myG FULL OUTER JOIN h AS myH ON myG.x = myH.x", fileNode);
    }

    // ======================= Convert ==============================================

    /** SELECT CONVERT(a, string) FROM g */
    @Test
    public void testConversionFunction() throws Exception {
        String sql = "SELECT CONVERT(a, string) FROM g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node functionNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Function.ID);
        verifyProperty(functionNode, Function.NAME_PROP_NAME, "CONVERT");

        verifyElementSymbol(functionNode, Function.ARGS_REF_NAME, 1, "a");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 2, "string");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    
        verifySql(sql, fileNode);
    }

    /** SELECT CONVERT(CONVERT(a, timestamp), string) FROM g */
    @Test
    public void testConversionFunction2() throws Exception {
        String sql = "SELECT CONVERT(CONVERT(a, timestamp), string) FROM g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

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

        verifySql(sql, fileNode);
    }

    // ======================= Functions ==============================================

    /** SELECT 5 + length(concat(a, 'x')) FROM g */
    @Test
    public void testMultiFunction() throws Exception {
        String sql = "SELECT 5 + length(concat(a, 'x')) FROM g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

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
    
        verifySql("SELECT (5 + length(concat(a, 'x'))) FROM g", fileNode);
    }

    /** SELECT REPLACE(a, 'x', 'y') AS y FROM g */
    @Test
    public void testAliasedFunction() throws Exception {
        String sql = "SELECT REPLACE(a, 'x', 'y') AS y FROM g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node functionNode = verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, "y", Function.ID);
        verifyProperty(functionNode, Function.NAME_PROP_NAME, "REPLACE");
        verifyElementSymbol(functionNode, Function.ARGS_REF_NAME,  1, "a");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 2, "x");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 3, "y");
        
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    
        verifySql(sql, fileNode);
    }

    /** SELECT cast(a as string) FROM g */
    @Test
    public void testCastFunction() throws Exception {
        String sql = "SELECT cast(a as string) FROM g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node functionNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Function.ID);
        verifyProperty(functionNode, Function.NAME_PROP_NAME, "cast");
        verifyElementSymbol(functionNode, Function.ARGS_REF_NAME,  1, "a");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 2, "string");
        
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    
        verifySql("SELECT cast(a AS string) FROM g", fileNode);
    }

    /** SELECT cast(cast(a as timestamp) as string) FROM g */
    @Test
    public void testMultiCastFunction() throws Exception {
        String sql = "SELECT cast(cast(a as timestamp) as string) FROM g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

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
    
        verifySql("SELECT cast(cast(a AS timestamp) AS string) FROM g", fileNode);
    }

    /** SELECT left(fullname, 3) as x FROM sys.groups */
    @Test
    public void testLeftFunction() throws Exception {
        String sql = "SELECT left(fullname, 3) as x FROM sys.groups";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        Node functionNode = verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, "x", Function.ID);
        verifyProperty(functionNode, Function.NAME_PROP_NAME, "left");
        verifyElementSymbol(functionNode, Function.ARGS_REF_NAME,  1, "fullname");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 2, 3);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "sys.groups");
    
        verifySql("SELECT left(fullname, 3) AS x FROM sys.groups", fileNode);
    }

    /** SELECT right(fullname, 3) as x FROM sys.groups */
    @Test
    public void testRightFunction() throws Exception {
        String sql = "SELECT right(fullname, 3) AS x FROM sys.groups";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);

        Node functionNode = verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, "x", Function.ID);
        verifyProperty(functionNode, Function.NAME_PROP_NAME, "right");
        verifyElementSymbol(functionNode, Function.ARGS_REF_NAME,  1, "fullname");
        verifyConstant(functionNode, Function.ARGS_REF_NAME, 2, 3);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "sys.groups");

        verifySql(sql, fileNode);
    }


    @Test
    public void testInsertIntoSelect() throws Exception {
        String sql = "insert into tempA SELECT 1";
        Node fileNode = sequenceSql(sql, TSQL_INSERT);

        Node insertNode = verify(fileNode, Insert.ID, Insert.ID);
        Node gsNode = verify(insertNode, TargetedCommand.GROUP_REF_NAME, GroupSymbol.ID);
        verifyProperty(gsNode, Symbol.NAME_PROP_NAME, "tempA");

        Node queryNode = verify(insertNode, Insert.QUERY_EXPRESSION_REF_NAME, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, 1);

        verifySql("INSERT INTO tempA SELECT 1", fileNode);
    }

    // ======================= Group By ==============================================

    /** SELECT a FROM m.g GROUP BY b, c HAVING b=5*/
    @Test
    public void testGroupByHaving() throws Exception {
        String sql = "SELECT a FROM m.g GROUP BY b, c HAVING b=5";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

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
    
        verifySql("SELECT a FROM m.g GROUP BY b, c HAVING b = 5", fileNode);
    }

    /** SELECT COUNT(a) AS c FROM m.g */
    @Test
    public void testAggregateFunction() throws Exception {
        String sql = "SELECT COUNT(a) AS c FROM m.g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node aggregateNode = verifyAliasSymbol(selectNode, Select.SYMBOLS_REF_NAME, "c", AggregateSymbol.ID);
        verifyProperty(aggregateNode, AggregateSymbol.AGGREGATE_FUNCTION_PROP_NAME, "COUNT");
        verifyProperty(aggregateNode, AggregateSymbol.DISTINCT_PROP_NAME, false);
        verifyElementSymbol(aggregateNode, AggregateSymbol.ARGS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "m.g");
    
        verifySql(sql, fileNode);
    }

    /** SELECT a FROM m.g GROUP BY a HAVING COUNT(b) > 0*/
    @Test
    public void testHavingFunction() throws Exception {
        String sql = "SELECT a FROM m.g GROUP BY a HAVING COUNT(b) > 0";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

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
    
        verifySql(sql, fileNode);
    }

    /** SELECT 5-null, a.g1.c1 FROM a.g1 */
    @Test
    public void testArithmeticNullFunction() throws Exception {
        String sql = "SELECT 5-null, a.g1.c1 FROM a.g1";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node functionNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Function.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "a.g1.c1");

        verifyProperty(functionNode, Function.NAME_PROP_NAME, "-");
        verifyConstant(functionNode, Function.ARGS_REF_NAME,  1, 5);
        
        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    
        verifySql("SELECT (5 - null), a.g1.c1 FROM a.g1", fileNode);
    }

    /** SELECT 'abc' FROM a.g1 */
    @Test
    public void testStringLiteral() throws Exception {
        String sql = "SELECT 'abc' FROM a.g1";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, "abc");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    
        verifySql(sql, fileNode);
    }

    /** SELECT 'O''Leary' FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick() throws Exception {
        String sql = "SELECT 'O''Leary' FROM a.g1";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, "O'Leary");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    
        verifySql(sql, fileNode);
    }

    /** SELECT '''abc''' FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick2() throws Exception {
        String sql = "SELECT '''abc''' FROM a.g1";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, "'abc'");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    
        verifySql(sql, fileNode);
    }

    /** SELECT 'a''b''c' FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick3() throws Exception {
        String sql = "SELECT 'a''b''c' FROM a.g1";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, "a'b'c");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    
        verifySql(sql, fileNode);
    }

    /** SELECT " "" " FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick4() throws Exception {
        String sql = "SELECT \" \"\" \" FROM a.g1";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, " \" ");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    
        verifySql(sql, fileNode);
    }

    /** SELECT 123456789012 FROM a.g1 */
    @Test
    public void testLongLiteral() throws Exception {
        String sql = "SELECT 123456789012 FROM a.g1";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, 123456789012L);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "a.g1");
    
        verifySql(sql, fileNode);
    }

    /** SELECT {d'2002-10-02'} FROM m.g1 */
    @Test
    public void testDateLiteral1() throws Exception {
        String sql = "SELECT {d'2002-10-02'} FROM m.g1";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, java.sql.Date.valueOf("2002-10-02"));

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "m.g1");
    
        verifySql(sql, fileNode);
    }

    /** SELECT {t'11:10:00'} FROM m.g1 */
    @Test
    public void testTimeLiteral1() throws Exception {
        String sql = "SELECT {t'11:10:00'} FROM m.g1";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME,  1, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, java.sql.Time.valueOf("11:10:00"));

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "m.g1");
    
        verifySql(sql, fileNode);
    }

    /** SELECT {b'true'} FROM m.g1 */
    @Test
    public void testBooleanLiteralTrue() throws Exception {
        Boolean expected = Boolean.TRUE;
        String sql = "SELECT {b'true'}";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, expected);
        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.BOOLEAN.name());
    
        verifySql("SELECT TRUE", fileNode);
    }

    /** SELECT TRUE FROM m.g1 */
    @Test
    public void testBooleanLiteralTrue2() throws Exception {
        Boolean expected = Boolean.TRUE;
        String sql = "SELECT TRUE";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, expected);
        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.BOOLEAN.name());
    
        verifySql("SELECT TRUE", fileNode);
    }

    /** SELECT {b'false'} FROM m.g1 */
    @Test
    public void testBooleanLiteralFalse() throws Exception {
        Boolean expected = Boolean.FALSE;
        String sql = "SELECT {b'false'}";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, expected);
        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.BOOLEAN.name());
    
        verifySql("SELECT FALSE", fileNode);
    }

    /** SELECT FALSE FROM m.g1 */
    @Test
    public void testBooleanLiteralFalse2() throws Exception {
        Boolean expected = Boolean.FALSE;
        String sql = "SELECT {b'false'}";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, expected);
        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.BOOLEAN.name());

        verifySql("SELECT FALSE", fileNode);
    }


    @Test
    public void testBooleanLiteralUnknown() throws Exception {
        String sql = "SELECT {b'unknown'}";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, Constant.ID);
        assertFalse(constantNode.hasProperty(Constant.VALUE_PROP_NAME));

        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.BOOLEAN.name());
    
        verifySql("SELECT UNKNOWN", fileNode);
    }

    /** SELECT DISTINCT a FROM g */
    @Test
    public void testSelectDistinct() throws Exception {
        String sql = "SELECT DISTINCT a FROM g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");
        verifyProperty(selectNode, Select.DISTINCT_PROP_NAME, true);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");
    
        verifySql(sql, fileNode);
    }

    /** SELECT ALL a FROM g */
    @Test
    public void testSelectAll() throws Exception {
        String sql = "SELECT ALL a FROM g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");
        verifyProperty(selectNode, Select.DISTINCT_PROP_NAME, false);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");

        verifySql("SELECT a FROM g", fileNode);
    }

    //=========================Aliasing==============================================


    /** SELECT myG.a FROM g AS myG */
    @Test
    public void testAliasInFrom() throws Exception {
        String sql = "SELECT myG.a FROM g AS myG";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "myG.a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "myG", "g");
    
        verifySql(sql, fileNode);
    }

    /** SELECT myG.*, myH.b FROM g AS myG, h AS myH */
    @Test
    public void testAliasesInFrom() throws Exception {
        String sql = "SELECT myG.*, myH.b FROM g AS myG, h AS myH";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node meSymbolNode = verify(selectNode, Select.SYMBOLS_REF_NAME, 1, MultipleElementSymbol.ID);
        Node groupSymbolNode = verify(meSymbolNode, MultipleElementSymbol.GROUP_REF_NAME, GroupSymbol.ID);
        verifyProperty(groupSymbolNode, Symbol.NAME_PROP_NAME, "myG");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "myG", "g");
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "myH", "h");
    
        verifySql(sql, fileNode);
    }

    /** SELECT myG.a, myH.b FROM g myG, h myH */
    @Test
    public void testHiddenAliasesInFrom() throws Exception {
        String sql = "SELECT myG.*, myH.b FROM g myG, h myH";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node meSymbolNode = verify(selectNode, Select.SYMBOLS_REF_NAME, 1, MultipleElementSymbol.ID);
        Node groupSymbolNode = verify(meSymbolNode, MultipleElementSymbol.GROUP_REF_NAME, GroupSymbol.ID);
        verifyProperty(groupSymbolNode, Symbol.NAME_PROP_NAME, "myG");
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, "myH.b");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "myG", "g");
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 2, "myH", "h");

        verifySql("SELECT myG.*, myH.b FROM g AS myG, h AS myH", fileNode);
    }

    // ======================= Misc ==============================================

    /** Select a From db.g Where a IS NULL */
    @Test
    public void testIsNullCriteria1() throws Exception {
        String sql = "Select a From db.g Where a IS NULL";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, IsNullCriteria.ID);
        verifyElementSymbol(criteriaNode, IsNullCriteria.EXPRESSION_REF_NAME, "a");
    
        verifySql("SELECT a FROM db.g WHERE a IS NULL", fileNode);
    }

    /** Select a From db.g Where a IS NOT NULL */
    @Test
    public void testIsNullCriteria2() throws Exception {
        String sql = "Select a From db.g Where a IS NOT NULL";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, IsNullCriteria.ID);
        verifyElementSymbol(criteriaNode, IsNullCriteria.EXPRESSION_REF_NAME, "a");
        verifyProperty(criteriaNode, IsNullCriteria.NEGATED_PROP_NAME, true);
    
        verifySql("SELECT a FROM db.g WHERE a IS NOT NULL", fileNode);
    }

    /** Select a From db.g Where Not a IS NULL */
    @Test
    public void testNotIsNullCriteria() throws Exception {
        String sql = "Select a From db.g Where Not a IS NULL";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node notCriteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, NotCriteria.ID);
        
        Node isNullCriteriaNode = verify(notCriteriaNode, NotCriteria.CRITERIA_REF_NAME, IsNullCriteria.ID);
        verifyElementSymbol(isNullCriteriaNode, IsNullCriteria.EXPRESSION_REF_NAME, "a");
    
        verifySql("SELECT a FROM db.g WHERE NOT (a IS NULL)", fileNode);
    }

    /** SELECT a from db.g where a <> "value" */
    @Test
    public void testStringNotEqualDoubleTicks() throws Exception {
        String sql = "SELECT a from db.g where a <> \"value\"";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.NE.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "a");
        verifyElementSymbol(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "value");
    
        verifySql("SELECT a FROM db.g WHERE a <> \"value\"", fileNode);
    }

    /** SELECT a from db.g where a != "value" */
    @Test
    public void testNotEquals2() throws Exception {
        String sql = "SELECT a from db.g where a != 'value'";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.NE.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "a");
        verifyConstant(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "value");
    
        verifySql("SELECT a FROM db.g WHERE a <> 'value'", fileNode);
    }

    /** SELECT a from db."g" where a = 5 */
    @Test
    public void testPartlyQuotedGroup() throws Exception {
        String sql = "SELECT a from db.\"g\" where a = 5";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "a");
        verifyConstant(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, 5);
    
        verifySql("SELECT a FROM db.g WHERE a = 5", fileNode);
    }

    /** SELECT * FROM model.doc WHERE ab.cd.@ef = 'abc' */
    @Test
    public void testXMLCriteriaWithAttribute() throws Exception {
        String sql = "SELECT * FROM model.doc WHERE ab.cd.@ef = 'abc'";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verify(selectNode, Select.SYMBOLS_REF_NAME, MultipleElementSymbol.ID);

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "model.doc");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, CompareCriteria.ID);
        verifyProperty(criteriaNode, AbstractCompareCriteria.OPERATOR_PROP_NAME, CriteriaOperator.Operator.EQ.name());
        verifyElementSymbol(criteriaNode, AbstractCompareCriteria.LEFT_EXPRESSION_REF_NAME, "ab.cd.@ef");
        verifyConstant(criteriaNode, CompareCriteria.RIGHT_EXPRESSION_REF_NAME, "abc");
    
        verifySql(sql, fileNode);
    }

    /** SELECT a from db.g where a BETWEEN 1000 AND 2000 */
    @Test
    public void testBetween1() throws Exception {
        String sql = "SELECT a from db.g where a BETWEEN 1000 AND 2000";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, BetweenCriteria.ID);
        verifyElementSymbol(criteriaNode, BetweenCriteria.EXPRESSION_REF_NAME, "a");
        verifyConstant(criteriaNode, BetweenCriteria.LOWER_EXPRESSION_REF_NAME, 1000);
        verifyConstant(criteriaNode, BetweenCriteria.UPPER_EXPRESSION_REF_NAME, 2000);
    
        verifySql("SELECT a FROM db.g WHERE a BETWEEN 1000 AND 2000", fileNode);
    }

    /** SELECT a FROM db.g WHERE b IN (1000,5000)*/
    @Test
    public void testSetCriteria0() throws Exception {
        String sql = "SELECT a FROM db.g WHERE b IN (1000,5000)";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, SetCriteria.ID);
        verifyElementSymbol(criteriaNode, AbstractSetCriteria.EXPRESSION_REF_NAME, "b");
        verifyConstant(criteriaNode, SetCriteria.VALUES_REF_NAME, 1, 1000);
        verifyConstant(criteriaNode, SetCriteria.VALUES_REF_NAME, 2, 5000);

        verifySql("SELECT a FROM db.g WHERE b IN (1000, 5000)", fileNode);
    }

    // ================================== order by ==================================

    /** SELECT a FROM db.g WHERE b = aString order by c desc*/
    @Test
    public void testOrderByDesc() throws Exception {
        String sql = "SELECT a FROM db.g WHERE b = aString ORDER BY c desc";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

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

        verifySql("SELECT a FROM db.g WHERE b = aString ORDER BY c DESC", fileNode);
    }


    @Test
    public void testOrderByNullOrdering() throws Exception {
        String sql = "SELECT a FROM db.g WHERE b = aString ORDER BY c NULLS FIRST,d desc nulls last";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

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

        verifySql("SELECT a FROM db.g WHERE b = aString ORDER BY c NULLS FIRST, d DESC NULLS LAST", fileNode);
    }

//    // ================================== match ====================================


    /** SELECT a from db.g where b like '#String' escape '#'*/
    @Test
    public void testLikeWithEscape() throws Exception {
        String sql = "SELECT a from db.g where b like '#String' escape '#'";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, MatchCriteria.ID);
        verifyProperty(criteriaNode, MatchCriteria.ESCAPE_CHAR_PROP_NAME, "#");
        verifyElementSymbol(criteriaNode, MatchCriteria.LEFT_EXPRESSION_REF_NAME, "b");
        verifyConstant(criteriaNode, MatchCriteria.RIGHT_EXPRESSION_REF_NAME, "#String");
    
        verifySql("SELECT a FROM db.g WHERE b LIKE '#String' ESCAPE '#'", fileNode);
    }

    /** SELECT a */
    @Test
    public void testNoFromClause() throws Exception {
        String sql = "SELECT a, 5";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");
        Node constantNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, 2, Constant.ID);
        verifyProperty(constantNode, Constant.VALUE_PROP_NAME, 5);
        verifyProperty(constantNode, Expression.TYPE_CLASS_PROP_NAME, DataTypeName.INTEGER.name());
    
        verifySql(sql, fileNode);
    }

    /** INSERT INTO m.g (a) VALUES (?) */
    @Test
    public void testInsertWithReference() throws Exception {
        String sql = "INSERT INTO m.g (a) VALUES (?)";
        Node fileNode = sequenceSql(sql, TSQL_INSERT);

        Node insertNode = verify(fileNode, Insert.ID, Insert.ID);
        Node gsNode = verify(insertNode, TargetedCommand.GROUP_REF_NAME, GroupSymbol.ID);
        verifyProperty(gsNode, Symbol.NAME_PROP_NAME, "m.g");
        verifyElementSymbol(insertNode, Insert.VARIABLES_REF_NAME, "a");
        Node refNode = verify(insertNode, Insert.VALUES_REF_NAME, Reference.ID);
        verifyProperty(refNode, Reference.INDEX_PROP_NAME, 0);
        verifyProperty(refNode, Reference.POSITIONAL_PROP_NAME, true);

        verifySql(sql, fileNode);
    }


    @Test
    public void testIfStatement() throws Exception {
        String sql = deriveProcPrefix(false) + "IF(c = 5) BEGIN DECLARE short a; END ELSE BEGIN DECLARE short b; END END";
        Node fileNode = sequenceSql(sql, TSQL_PROC_CMD);

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

        verifySql(deriveProcPrefix(true) + NEW_LINE
                  + "IF(c = 5)" + NEW_LINE
                  + "BEGIN" + NEW_LINE
                  + "DECLARE short a;" + NEW_LINE
                  + "END" + NEW_LINE
                  + "ELSE" + NEW_LINE
                  + "BEGIN" + NEW_LINE
                  + "DECLARE short b;" + NEW_LINE
                  + "END" + NEW_LINE
                  + "END", fileNode);
    }


    @Test
    public void testDynamicCommandStatement() throws Exception {
        String sql = deriveProcPrefix(false) + "exec string 'SELECT a1 FROM g WHERE a2 = 5' as a1 string into #g; END";
        Node fileNode = sequenceSql(sql, TSQL_PROC_CMD);

        Node procNode = verify(fileNode, CreateProcedureCommand.ID, CreateProcedureCommand.ID);
        Node outerBlkNode = verify(procNode, CreateProcedureCommand.BLOCK_REF_NAME, Block.ID);
        Node cmdStmtNode = verify(outerBlkNode, Block.STATEMENTS_REF_NAME, CommandStatement.ID);

        Node dynCmdNode = verify(cmdStmtNode, SubqueryContainer.COMMAND_REF_NAME, DynamicCommand.ID);
        verifyConstant(dynCmdNode, DynamicCommand.SQL_REF_NAME, "SELECT a1 FROM g WHERE a2 = 5");
        verifyProperty(dynCmdNode, DynamicCommand.AS_CLAUSE_SET_PROP_NAME, true);
        verifyElementSymbol(dynCmdNode, DynamicCommand.AS_COLUMNS_REF_NAME, "a1");
        Node intoGroupNode = verify(dynCmdNode, DynamicCommand.INTO_GROUP_REF_NAME, GroupSymbol.ID);
        verifyProperty(intoGroupNode, Symbol.NAME_PROP_NAME, "#g");

        verifySql(deriveProcPrefix(true) + NEW_LINE
                  + "EXECUTE IMMEDIATE 'SELECT a1 FROM g WHERE a2 = 5' AS a1 string INTO #g;" + NEW_LINE
                  + "END", fileNode);
    }


    @Test
    public void testSubquerySetCriteriaWithExec() throws Exception {
        String sql = "SELECT a FROM db.g WHERE b IN (EXEC m.sq1())";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

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

        verifySql("SELECT a FROM db.g WHERE b IN (SELECT * FROM (EXEC m.sq1()) AS x)", fileNode);
    }


    @Test
    public void testSubquerySetCriteriaWithUnion() throws Exception {
        String sql = "SELECT a FROM db.g WHERE b IN (SELECT x1 FROM db.g2 UNION ALL SELECT x2 FROM db.g3)";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, "db.g");

        Node criteriaNode = verify(queryNode, Query.CRITERIA_REF_NAME, SubquerySetCriteria.ID);
        verifyElementSymbol(criteriaNode, AbstractSetCriteria.EXPRESSION_REF_NAME, "b");

        Node unionQueryNode = verify(criteriaNode, SubqueryContainer.COMMAND_REF_NAME, SetQuery.ID);
        verifyProperty(unionQueryNode, SetQuery.ALL_PROP_NAME, true);
        verifyProperty(unionQueryNode, SetQuery.OPERATION_PROP_NAME, Operation.UNION.name());

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

        verifySql(sql, fileNode);
    }


    @Test
    public void testLoopStatement() throws Exception {
        String sql = deriveProcPrefix(false) + "LOOP ON (SELECT c1, c2 FROM m.g) AS mycursor BEGIN DECLARE integer x; x=mycursor.c1; END END";
        Node fileNode = sequenceSql(sql, TSQL_PROC_CMD);

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

        verifySql(deriveProcPrefix(true) + NEW_LINE
                      + "LOOP ON (SELECT c1, c2 FROM m.g) AS mycursor" + NEW_LINE
                      + "BEGIN" + NEW_LINE
                      + "DECLARE integer x;" + NEW_LINE
                      + "x = mycursor.c1;" + NEW_LINE
                      + "END" + NEW_LINE
                      + "END", fileNode);
    }


    @Test
    public void testXmlElement() throws Exception {
        String sql = "SELECT xmlelement(name \"table\", 'x') FROM g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        Node xmlElemNode = verifyExpressionSymbol(selectNode, Select.SYMBOLS_REF_NAME, XMLElement.ID);
        verifyProperty(xmlElemNode, XMLElement.NAME_PROP_NAME, "table");
        verifyConstant(xmlElemNode, XMLElement.CONTENT_REF_NAME, "x");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "g");

        verifySql("SELECT XMLELEMENT(NAME \"table\", 'x') FROM g", fileNode);
    }


    @Test
    public void testXmlElementWithAttributes() throws Exception {
        String sql = "SELECT xmlelement(y, xmlattributes('a' as val)) from g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

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

        verifySql("SELECT XMLELEMENT(NAME y, XMLATTRIBUTES('a' AS val)) FROM g", fileNode);
    }


    @Test
    public void testTextTable() throws Exception {
        String sql = "SELECT * from texttable(file columns x string, y date delimiter ',' escape '\"' header skip 10) as x";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

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

        verifySql("SELECT * FROM TEXTTABLE(file COLUMNS x string, y date DELIMITER ',' ESCAPE '\"' HEADER SKIP 10) AS x", fileNode);
    }


    @Test
    public void testWindowFunction() throws Exception {
        String sql = "select row_number() over (partition by x order by y) from g";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

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

        verifySql("SELECT ROW_NUMBER() OVER (PARTITION BY x ORDER BY y) FROM g", fileNode);
    }

    @Test
    public void testLocalTable() throws Exception {
        String sql = "Create local TEMPORARY table x(c1 boolean, c2 byte, c3 string);";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node createNode = verify(fileNode, Create.ID, Create.ID);

        Node tableNode = verify(createNode, Create.TABLE_REF_NAME, GroupSymbol.ID);
        verifyProperty(tableNode, Symbol.SHORT_NAME_PROP_NAME, "x");

        Node columnNode=verify(createNode,Create.COLUMNS_REF_NAME,ElementSymbol.ID);
        verifyProperty(columnNode, Symbol.SHORT_NAME_PROP_NAME, "c1");
        verifyProperty(columnNode,Expression.TYPE_CLASS_PROP_NAME,"BOOLEAN");

        verifySql("CREATE LOCAL TEMPORARY TABLE x(c1 BOOLEAN, c2 BYTE, c3 STRING)", fileNode);
    }

    @Test
    public void testLocalTablePreserveRows() throws Exception {
        String sql = "Create local TEMPORARY table x(c1 boolean, c2 byte, c3 string, primary key (c2, c3)) ON COMMIT PRESERVE ROWS;";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node createNode = verify(fileNode, Create.ID, Create.ID);

        Node tableNode = verify(createNode, Create.TABLE_REF_NAME, GroupSymbol.ID);
        verifyProperty(tableNode, Symbol.SHORT_NAME_PROP_NAME, "x");

        Node columnNode=verify(createNode,Create.COLUMNS_REF_NAME,ElementSymbol.ID);
        verifyProperty(columnNode, Symbol.SHORT_NAME_PROP_NAME, "c1");
        verifyProperty(columnNode,Expression.TYPE_CLASS_PROP_NAME,"BOOLEAN");

        verify(createNode,Create.PRIMARY_KEY_COLUMNS_REF_NAME,ElementSymbol.ID);

        verifySql("CREATE LOCAL TEMPORARY TABLE x(c1 BOOLEAN, c2 BYTE, c3 STRING, PRIMARY KEY(c2, c3)) ON COMMIT PRESERVE ROWS", fileNode);
    }

}
