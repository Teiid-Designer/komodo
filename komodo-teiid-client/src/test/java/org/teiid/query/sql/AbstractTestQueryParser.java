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
package org.teiid.query.sql;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.sql.Date;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.Test;
import org.komodo.spi.query.JoinTypeTypes;
import org.komodo.spi.query.sql.lang.SPParameter.ParameterInfo;
import org.komodo.spi.query.sql.lang.SetQuery;
import org.komodo.spi.query.sql.symbol.AggregateSymbol;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.language.SQLConstants.NonReserved;
import org.teiid.language.SQLConstants.Reserved;
import org.teiid.language.SortSpecification;
import org.teiid.query.parser.ParseInfo;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.lang.ArrayTableImpl;
import org.teiid.query.sql.lang.BetweenCriteriaImpl;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.CompoundCriteriaImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.CriteriaOperator.Operator;
import org.teiid.query.sql.lang.DeleteImpl;
import org.teiid.query.sql.lang.DynamicCommandImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.FromClauseImpl;
import org.teiid.query.sql.lang.FromImpl;
import org.teiid.query.sql.lang.GroupByImpl;
import org.teiid.query.sql.lang.InsertImpl;
import org.teiid.query.sql.lang.IntoImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.JoinPredicateImpl;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.NamespaceItem;
import org.teiid.query.sql.lang.OrderByImpl;
import org.teiid.query.sql.lang.OrderByItemImpl;
import org.teiid.query.sql.lang.ProjectedColumnImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.SetClauseImpl;
import org.teiid.query.sql.lang.SetClauseListImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SetQueryImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl.PredicateQuantifier;
import org.teiid.query.sql.lang.SubqueryFromClauseImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.lang.TextColumnImpl;
import org.teiid.query.sql.lang.TextTableImpl;
import org.teiid.query.sql.lang.UnaryFromClauseImpl;
import org.teiid.query.sql.lang.UpdateImpl;
import org.teiid.query.sql.lang.XMLColumnImpl;
import org.teiid.query.sql.lang.XMLTableImpl;
import org.teiid.query.sql.proc.AssignmentStatementImpl;
import org.teiid.query.sql.proc.BlockImpl;
import org.teiid.query.sql.proc.BranchingStatementImpl;
import org.teiid.query.sql.proc.BranchingStatementImpl.BranchingMode;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.DeclareStatementImpl;
import org.teiid.query.sql.proc.IfStatementImpl;
import org.teiid.query.sql.proc.LoopStatementImpl;
import org.teiid.query.sql.proc.StatementImpl;
import org.teiid.query.sql.proc.WhileStatementImpl;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.BaseAggregateSymbol;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.BaseWindowFunction;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.DerivedColumnImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.ExpressionSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.MultipleElementSymbolImpl;
import org.teiid.query.sql.symbol.ReferenceImpl;
import org.teiid.query.sql.symbol.SearchedCaseExpressionImpl;
import org.teiid.query.sql.symbol.TextLineImpl;
import org.teiid.query.sql.symbol.WindowSpecificationImpl;
import org.teiid.query.sql.symbol.XMLElementImpl;
import org.teiid.query.sql.symbol.XMLForestImpl;
import org.teiid.query.sql.symbol.XMLParseImpl;
import org.teiid.query.sql.symbol.XMLQueryImpl;
import org.teiid.query.sql.symbol.XMLSerializeImpl;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractTestQueryParser extends AbstractTest<CommandImpl> {

    /**
     * @param teiidVersion 
     */
    public AbstractTestQueryParser(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    protected void helpTest(String sql, String expectedString, CommandImpl expectedCommand) {
        helpTest(sql, expectedString, expectedCommand, new ParseInfo());
    }

    protected void helpTest(String sql, String expectedString, CommandImpl expectedCommand, ParseInfo info) {
        CommandImpl actualCommand = null;
        String actualString = null;

        try {
            actualCommand = parser.parseCommand(sql, info);
            actualString = actualCommand.toString();
        } catch (Throwable e) {
            fail(e.getMessage());
        }

        assertEquals("Command objects do not match: ", expectedCommand, actualCommand);
        assertEquals("SQL strings do not match: ", expectedString, actualString);
    }

    protected void helpTestLiteral(Boolean expected, Class<?> expectedType, String sql, String expectedSql) {
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(expected, expectedType)));

        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);

        helpTest(sql, expectedSql, query);
    }

    protected void helpCriteriaTest(String crit, String expectedString, CriteriaImpl expectedCrit) {
        CriteriaImpl actualCriteria;
        String actualString;

        try {
            actualCriteria = parser.parseCriteria(crit);
            actualString = actualCriteria.toString();
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }

        assertEquals("Criteria does not match: ", expectedCrit, actualCriteria);
        assertEquals("SQL strings do not match: ", expectedString, actualString);
    }

    protected void helpException(String sql) {
        helpException(sql, null);
    }

    protected void helpException(String sql, String expected) {
        try {
            parser.parseCommand(sql);
            fail("Expected exception for parsing " + sql);
        } catch (Exception e) {
            if (expected != null) {
                assertEquals(expected, e.getMessage());
            }
        } catch (AssertionError e) {
            throw e;
        } catch (Error e) {
            if (expected != null) {
                assertEquals(expected, e.getMessage());
            }
        }
    }

    protected void helpTestExpression(String sql, String expectedString, BaseExpression expected) throws Exception {
        BaseExpression actual = parser.parseExpression(sql);
        String actualString = actual.toString();
        assertEquals("Command objects do not match: ", expected, actual);
        assertEquals("SQL strings do not match: ", expectedString, actualString);
    }

    protected void helpStmtTest(String stmt, String expectedString, StatementImpl expectedStmt) throws Exception {
        StatementImpl actualStmt = parser.getTeiidParser(stmt).statement(new ParseInfo());
        String actualString = actualStmt.toString();
        assertEquals("Language objects do not match: ", expectedStmt, actualStmt);
        assertEquals("SQL strings do not match: ", expectedString, actualString);
    }

 // ======================== Joins ===============================================

    /** SELECT * FROM g1 inner join g2 on g1.a1=g2.a2 */
    @Test
    public void testInnerJoin() {
        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause("g1");
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause("g2");

        CompareCriteriaImpl jcrit = getFactory().newCompareCriteria("g1.a1", Operator.EQ, "g2.a2");
        List<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(jcrit);

        JoinPredicateImpl jp = getFactory().newJoinPredicate(g1, g2, JoinTypeTypes.JOIN_INNER, crits);

        FromImpl from = getFactory().newFrom();
        from.addClause(jp);

        SelectImpl select = getFactory().newSelectWithMultileElementSymbol();

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM g1 inner join g2 on g1.a1=g2.a2", "SELECT * FROM g1 INNER JOIN g2 ON g1.a1 = g2.a2", query);
    }

    /** SELECT * FROM g1 cross join g2 */
    @Test
    public void testCrossJoin() {
        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause("g1");
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause("g2");

        JoinPredicateImpl jp = getFactory().newJoinPredicate(g1, g2, JoinTypeTypes.JOIN_CROSS);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp);

        SelectImpl select = getFactory().newSelectWithMultileElementSymbol();

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM g1 cross join g2", "SELECT * FROM g1 CROSS JOIN g2", query);
    }

    /** SELECT * FROM (g1 cross join g2), g3 */
    @Test
    public void testFromClauses() {
        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause("g1");
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause("g2");

        JoinPredicateImpl jp = getFactory().newJoinPredicate(g1, g2, JoinTypeTypes.JOIN_CROSS);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp);

        UnaryFromClauseImpl g3 = getFactory().newUnaryFromClause("g3");
        from.addClause(g3);

        SelectImpl select = getFactory().newSelectWithMultileElementSymbol();

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM (g1 cross join g2), g3", "SELECT * FROM g1 CROSS JOIN g2, g3", query);
    }

    /** SELECT * FROM (g1 cross join g2) cross join g3 */
    @Test
    public void testMultiCrossJoin() {
        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause("g1");
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause("g2");

        JoinPredicateImpl jp = getFactory().newJoinPredicate(g1, g2, JoinTypeTypes.JOIN_CROSS);
        UnaryFromClauseImpl g3 = getFactory().newUnaryFromClause("g3");
        JoinPredicateImpl jp2 = getFactory().newJoinPredicate(jp, g3, JoinTypeTypes.JOIN_CROSS);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp2);

        SelectImpl select = getFactory().newSelectWithMultileElementSymbol();

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM (g1 cross join g2) cross join g3", "SELECT * FROM (g1 CROSS JOIN g2) CROSS JOIN g3", query);
    }

    /** SELECT * FROM (g1 cross join g2) cross join (g3 cross join g4) */
    @Test
    public void testMultiCrossJoin2() {
        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause("g1");
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause("g2");

        JoinPredicateImpl jp = getFactory().newJoinPredicate(g1, g2, JoinTypeTypes.JOIN_CROSS);
        UnaryFromClauseImpl g3 = getFactory().newUnaryFromClause("g3");
        UnaryFromClauseImpl g4 = getFactory().newUnaryFromClause("g4");
        JoinPredicateImpl jp2 = getFactory().newJoinPredicate(g3, g4, JoinTypeTypes.JOIN_CROSS);
        JoinPredicateImpl jp3 = getFactory().newJoinPredicate(jp, jp2, JoinTypeTypes.JOIN_CROSS);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp3);

        SelectImpl select = getFactory().newSelectWithMultileElementSymbol();

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM (g1 cross join g2) cross join (g3 cross join g4)",
                 "SELECT * FROM (g1 CROSS JOIN g2) CROSS JOIN (g3 CROSS JOIN g4)",
                 query);
    }

    /** SELECT * FROM g1 cross join (g2 cross join g3) */
    @Test
    public void testMultiCrossJoin3() {
        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause("g1");
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause("g2");
        UnaryFromClauseImpl g3 = getFactory().newUnaryFromClause("g3");

        JoinPredicateImpl jp = getFactory().newJoinPredicate(g2, g3, JoinTypeTypes.JOIN_CROSS);
        JoinPredicateImpl jp2 = getFactory().newJoinPredicate(g1, jp, JoinTypeTypes.JOIN_CROSS);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp2);

        SelectImpl select = getFactory().newSelectWithMultileElementSymbol();

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM g1 cross join (g2 cross join g3)", "SELECT * FROM g1 CROSS JOIN (g2 CROSS JOIN g3)", query);
    }

    /** SELECT * FROM g1 cross join (g2 cross join g3), g4 */
    @Test
    public void testMixedJoin() {
        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause("g1");
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause("g2");
        UnaryFromClauseImpl g3 = getFactory().newUnaryFromClause("g3");

        JoinPredicateImpl jp = getFactory().newJoinPredicate(g2, g3, JoinTypeTypes.JOIN_CROSS);
        JoinPredicateImpl jp2 = getFactory().newJoinPredicate(g1, jp, JoinTypeTypes.JOIN_CROSS);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp2);
        UnaryFromClauseImpl g4 = getFactory().newUnaryFromClause("g4");
        from.addClause(g4);

        SelectImpl select = getFactory().newSelectWithMultileElementSymbol();

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM g1 cross join (g2 cross join g3), g4",
                 "SELECT * FROM g1 CROSS JOIN (g2 CROSS JOIN g3), g4",
                 query);
    }

    /** SELECT * FROM g1 cross join (g2 cross join g3), g4, g5 cross join g6 */
    @Test
    public void testMixedJoin2() {
        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause("g1");
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause("g2");
        UnaryFromClauseImpl g3 = getFactory().newUnaryFromClause("g3");
        UnaryFromClauseImpl g4 = getFactory().newUnaryFromClause("g4");
        UnaryFromClauseImpl g5 = getFactory().newUnaryFromClause("g5");
        UnaryFromClauseImpl g6 = getFactory().newUnaryFromClause("g6");

        JoinPredicateImpl jp = getFactory().newJoinPredicate(g2, g3, JoinTypeTypes.JOIN_CROSS);
        JoinPredicateImpl jp2 = getFactory().newJoinPredicate(g1, jp, JoinTypeTypes.JOIN_CROSS);
        JoinPredicateImpl jp3 = getFactory().newJoinPredicate(g5, g6, JoinTypeTypes.JOIN_CROSS);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp2);
        from.addClause(g4);
        from.addClause(jp3);

        SelectImpl select = getFactory().newSelectWithMultileElementSymbol();

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM g1 cross join (g2 cross join g3), g4, g5 cross join g6",
                 "SELECT * FROM g1 CROSS JOIN (g2 CROSS JOIN g3), g4, g5 CROSS JOIN g6",
                 query);
    }

    /** SELECT * FROM g1, g2 inner join g3 on g2.a=g3.a */
    @Test
    public void testMixedJoin3() {
        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause("g1");
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause("g2");
        UnaryFromClauseImpl g3 = getFactory().newUnaryFromClause("g3");

        CompareCriteriaImpl jcrit = getFactory().newCompareCriteria("g2.a", Operator.EQ, "g3.a");

        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(jcrit);
        JoinPredicateImpl jp = getFactory().newJoinPredicate(g2, g3, JoinTypeTypes.JOIN_INNER, crits);
        FromImpl from = getFactory().newFrom();
        from.addClause(g1);
        from.addClause(jp);

        SelectImpl select = getFactory().newSelectWithMultileElementSymbol();

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM g1, g2 inner join g3 on g2.a=g3.a", "SELECT * FROM g1, g2 INNER JOIN g3 ON g2.a = g3.a", query);
    }

    /** Select myG.a myA, myH.b from g myG right outer join h myH on myG.x=myH.x */
    @Test
    public void testRightOuterJoinWithAliases() {
        UnaryFromClauseImpl g = getFactory().newUnaryFromClause("myG", "g");
        UnaryFromClauseImpl h = getFactory().newUnaryFromClause("myH", "h");

        CompareCriteriaImpl jcrit = getFactory().newCompareCriteria("myG.x", Operator.EQ, "myH.x");
        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(jcrit);
        JoinPredicateImpl jp = getFactory().newJoinPredicate(g, h, JoinTypeTypes.JOIN_RIGHT_OUTER, crits);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp);

        AliasSymbolImpl as = getFactory().newAliasSymbolWithElementSymbol("myA", "myG.a");
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);
        select.addSymbol(getFactory().newElementSymbol("myH.b"));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("Select myG.a myA, myH.b from g myG right outer join h myH on myG.x=myH.x",
                 "SELECT myG.a AS myA, myH.b FROM g AS myG RIGHT OUTER JOIN h AS myH ON myG.x = myH.x",
                 query);
    }

    /** Select myG.x myX, myH.y from g myG right join h myH on myG.x=myH.x */
    @Test
    public void testRightJoinWithAliases() {
        UnaryFromClauseImpl g = getFactory().newUnaryFromClause("myG", "g");
        UnaryFromClauseImpl h = getFactory().newUnaryFromClause("myH", "h");

        CompareCriteriaImpl jcrit = getFactory().newCompareCriteria("myG.x", Operator.EQ, "myH.x");
        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(jcrit);
        JoinPredicateImpl jp = getFactory().newJoinPredicate(g, h, JoinTypeTypes.JOIN_RIGHT_OUTER, crits);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp);

        AliasSymbolImpl as = getFactory().newAliasSymbolWithElementSymbol("myA", "myG.a");
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);
        select.addSymbol(getFactory().newElementSymbol("myH.b"));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("Select myG.a myA, myH.b from g myG right join h myH on myG.x=myH.x",
                 "SELECT myG.a AS myA, myH.b FROM g AS myG RIGHT OUTER JOIN h AS myH ON myG.x = myH.x",
                 query);
    }

    /** Select myG.a myA, myH.b from g myG left outer join h myH on myG.x=myH.x */
    @Test
    public void testLeftOuterJoinWithAliases() {
        UnaryFromClauseImpl g = getFactory().newUnaryFromClause("myG", "g");
        UnaryFromClauseImpl h = getFactory().newUnaryFromClause("myH", "h");

        CompareCriteriaImpl jcrit = getFactory().newCompareCriteria("myG.x", Operator.EQ, "myH.x");
        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(jcrit);
        JoinPredicateImpl jp = getFactory().newJoinPredicate(g, h, JoinTypeTypes.JOIN_LEFT_OUTER, crits);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp);

        AliasSymbolImpl as = getFactory().newAliasSymbolWithElementSymbol("myA", "myG.a");
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);
        select.addSymbol(getFactory().newElementSymbol("myH.b"));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("Select myG.a myA, myH.b from g myG left outer join h myH on myG.x=myH.x",
                 "SELECT myG.a AS myA, myH.b FROM g AS myG LEFT OUTER JOIN h AS myH ON myG.x = myH.x",
                 query);
    }

    /** Select myG.a myA, myH.b from g myG left join h myH on myG.x=myH.x */
    @Test
    public void testLeftJoinWithAliases() {
        UnaryFromClauseImpl g = getFactory().newUnaryFromClause("myG", "g");
        UnaryFromClauseImpl h = getFactory().newUnaryFromClause("myH", "h");

        CompareCriteriaImpl jcrit = getFactory().newCompareCriteria("myG.x", Operator.EQ, "myH.x");
        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(jcrit);
        JoinPredicateImpl jp = getFactory().newJoinPredicate(g, h, JoinTypeTypes.JOIN_LEFT_OUTER, crits);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp);

        AliasSymbolImpl as = getFactory().newAliasSymbolWithElementSymbol("myA", "myG.a");
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);
        select.addSymbol(getFactory().newElementSymbol("myH.b"));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("Select myG.a myA, myH.b from g myG left join h myH on myG.x=myH.x",
                 "SELECT myG.a AS myA, myH.b FROM g AS myG LEFT OUTER JOIN h AS myH ON myG.x = myH.x",
                 query);
    }

    /** Select myG.a myA, myH.b from g myG full outer join h myH on myG.x=myH.x */
    @Test
    public void testFullOuterJoinWithAliases() {
        UnaryFromClauseImpl g = getFactory().newUnaryFromClause("myG", "g");
        UnaryFromClauseImpl h = getFactory().newUnaryFromClause("myH", "h");

        CompareCriteriaImpl jcrit = getFactory().newCompareCriteria("myG.x", Operator.EQ, "myH.x");
        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(jcrit);
        JoinPredicateImpl jp = getFactory().newJoinPredicate(g, h, JoinTypeTypes.JOIN_FULL_OUTER, crits);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp);

        AliasSymbolImpl as = getFactory().newAliasSymbolWithElementSymbol("myA", "myG.a");
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);
        select.addSymbol(getFactory().newElementSymbol("myH.b"));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("Select myG.a myA, myH.b from g myG full outer join h myH on myG.x=myH.x",
                 "SELECT myG.a AS myA, myH.b FROM g AS myG FULL OUTER JOIN h AS myH ON myG.x = myH.x",
                 query);
    }

    /** Select g.a, h.b from g full join h on g.x=h.x */
    @Test
    public void testFullJoin() {
        UnaryFromClauseImpl g = getFactory().newUnaryFromClause("g");
        UnaryFromClauseImpl h = getFactory().newUnaryFromClause("h");

        CompareCriteriaImpl jcrit = getFactory().newCompareCriteria("g.x", Operator.EQ, "h.x");
        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(jcrit);
        JoinPredicateImpl jp = getFactory().newJoinPredicate(g, h, JoinTypeTypes.JOIN_FULL_OUTER, crits);
        FromImpl from = getFactory().newFrom();
        from.addClause(jp);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("g.a"));
        select.addSymbol(getFactory().newElementSymbol("h.b"));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("Select g.a, h.b from g full join h on g.x=h.x", "SELECT g.a, h.b FROM g FULL OUTER JOIN h ON g.x = h.x", query);
    }

    // ======================= Convert ==============================================

    /** SELECT CONVERT(a, string) FROM g */
    @Test
    public void testConversionFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("CONVERT", getFactory().newElementSymbol("a"), getFactory().newConstant("string"));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT CONVERT(a, string) FROM g", "SELECT CONVERT(a, string) FROM g", query);
    }

    /** SELECT CONVERT(CONVERT(a, timestamp), string) FROM g */
    @Test
    public void testConversionFunction2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("CONVERT", getFactory().newElementSymbol("a"), getFactory().newConstant("timestamp"));
        FunctionImpl f2 = getFactory().newFunction("CONVERT", f, getFactory().newConstant("string"));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f2));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT CONVERT(CONVERT(a, timestamp), string) FROM g",
                 "SELECT CONVERT(CONVERT(a, timestamp), string) FROM g",
                 query);
    }

    // ======================= Functions ==============================================

    /** SELECT 5 + length(concat(a, 'x')) FROM g */
    @Test
    public void testMultiFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("concat", new BaseExpression[] {getFactory().newElementSymbol("a"), getFactory().newConstant("x")});
        FunctionImpl f2 = getFactory().newFunction("length", new BaseExpression[] {f});
        FunctionImpl f3 = getFactory().newFunction("+", new BaseExpression[] {getFactory().newConstant(new Integer(5)), f2});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f3));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 5 + length(concat(a, 'x')) FROM g", "SELECT (5 + length(concat(a, 'x'))) FROM g", query);
    }

    /** SELECT REPLACE(a, 'x', 'y') AS y FROM g */
    @Test
    public void testAliasedFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("REPLACE", new BaseExpression[] {getFactory().newElementSymbol("a"), getFactory().newConstant("x"), getFactory().newConstant("y")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("y", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT REPLACE(a, 'x', 'y') AS y FROM g", "SELECT REPLACE(a, 'x', 'y') AS y FROM g", query);
    }

    /** SELECT cast(a as string) FROM g */
    @Test
    public void testCastFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("cast", new BaseExpression[] {getFactory().newElementSymbol("a"), getFactory().newConstant("string")});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT cast(a as string) FROM g", "SELECT cast(a AS string) FROM g", query);
    }

    /** SELECT cast(cast(a as timestamp) as string) FROM g */
    @Test
    public void testMultiCastFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("cast", new BaseExpression[] {getFactory().newElementSymbol("a"), getFactory().newConstant("timestamp")});
        FunctionImpl f2 = getFactory().newFunction("cast", new BaseExpression[] {f, getFactory().newConstant("string")});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f2));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT cast(cast(a as timestamp) as string) FROM g",
                 "SELECT cast(cast(a AS timestamp) AS string) FROM g",
                 query);
    }

    /** SELECT left(fullname, 3) as x FROM sys.groups */
    @Test
    public void testLeftFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("sys.groups");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("left", new BaseExpression[] {getFactory().newElementSymbol("fullname"), getFactory().newConstant(new Integer(3))});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT left(fullname, 3) as x FROM sys.groups", "SELECT left(fullname, 3) AS x FROM sys.groups", query);
    }

    /** SELECT right(fullname, 3) as x FROM sys.groups */
    @Test
    public void testRightFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("sys.groups");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("right", new BaseExpression[] {getFactory().newElementSymbol("fullname"), getFactory().newConstant(new Integer(3))});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT right(fullname, 3) as x FROM sys.groups", "SELECT right(fullname, 3) AS x FROM sys.groups", query);
    }

    /** SELECT char('x') AS x FROM sys.groups */
    @Test
    public void testCharFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("sys.groups");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("char", new BaseExpression[] {getFactory().newConstant("x")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT char('x') AS x FROM sys.groups", "SELECT char('x') AS x FROM sys.groups", query);
    }

    /** SELECT insert('x', 1, 'a') as x FROM sys.groups */
    @Test
    public void testInsertFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("sys.groups");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("insert", new BaseExpression[] {getFactory().newConstant("x"), getFactory().newConstant(new Integer(1)), getFactory().newConstant("a")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT insert('x', 1, 'a') AS x FROM sys.groups", "SELECT insert('x', 1, 'a') AS x FROM sys.groups", query);
    }

    @Test
    public void testInsertIntoSelect() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("sys.groups");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        InsertImpl insert = getFactory().newNode(ASTNodes.INSERT);
        GroupSymbolImpl groupSymbol = getFactory().newGroupSymbol("tempA");
        insert.setGroup(groupSymbol);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(new Integer(1))));

        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);

        insert.setQueryExpression(query);

        helpTest("insert into tempA SELECT 1", "INSERT INTO tempA SELECT 1", insert);
    }

    /** SELECT translate('x', 'x', 'y') FROM sys.groups */
    @Test
    public void testTranslateFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("sys.groups");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("translate", new BaseExpression[] {getFactory().newConstant("x"), getFactory().newConstant("x"), getFactory().newConstant("y")});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT translate('x', 'x', 'y') FROM sys.groups", "SELECT translate('x', 'x', 'y') FROM sys.groups", query);
    }

    /** SELECT timestampadd(SQL_TSI_FRAC_SECOND, 10, '2003-05-01 10:20:30') as x FROM my.group1 */
    @Test
    public void testTimestampaddFunctionFracSecond() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("my.group1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("timestampadd", new BaseExpression[] {getFactory().newConstant("SQL_TSI_FRAC_SECOND"),
            getFactory().newConstant(new Integer(10)), getFactory().newConstant("2003-05-01 10:20:30")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT timestampadd(SQL_TSI_FRAC_SECOND, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 "SELECT timestampadd(SQL_TSI_FRAC_SECOND, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 query);
    }

    /** SELECT timestampadd(SQL_TSI_SECOND, 10, '2003-05-01 10:20:30') as x FROM my.group1 */
    @Test
    public void testTimestampaddFunctionSecond() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("my.group1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("timestampadd", new BaseExpression[] {getFactory().newConstant("SQL_TSI_SECOND"), getFactory().newConstant(new Integer(10)),
            getFactory().newConstant("2003-05-01 10:20:30")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT timestampadd(SQL_TSI_SECOND, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 "SELECT timestampadd(SQL_TSI_SECOND, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 query);
    }

    /** SELECT timestampadd(SQL_TSI_MINUTE, 10, '2003-05-01 10:20:30') as x FROM my.group1 */
    @Test
    public void testTimestampaddFunctionMinute() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("my.group1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("timestampadd", new BaseExpression[] {getFactory().newConstant("SQL_TSI_MINUTE"), getFactory().newConstant(new Integer(10)),
            getFactory().newConstant("2003-05-01 10:20:30")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT timestampadd(SQL_TSI_MINUTE, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 "SELECT timestampadd(SQL_TSI_MINUTE, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 query);
    }

    /** SELECT timestampadd(SQL_TSI_HOUR, 10, '2003-05-01 10:20:30') as x FROM my.group1 */
    @Test
    public void testTimestampaddFunctionHour() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("my.group1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("timestampadd", new BaseExpression[] {getFactory().newConstant("SQL_TSI_HOUR"), getFactory().newConstant(new Integer(10)),
            getFactory().newConstant("2003-05-01 10:20:30")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT timestampadd(SQL_TSI_HOUR, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 "SELECT timestampadd(SQL_TSI_HOUR, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 query);
    }

    /** SELECT timestampadd(SQL_TSI_DAY, 10, '2003-05-01 10:20:30') as x FROM my.group1 */
    @Test
    public void testTimestampaddFunctionDay() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("my.group1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("timestampadd", new BaseExpression[] {getFactory().newConstant("SQL_TSI_DAY"), getFactory().newConstant(new Integer(10)),
            getFactory().newConstant("2003-05-01 10:20:30")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT timestampadd(SQL_TSI_DAY, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 "SELECT timestampadd(SQL_TSI_DAY, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 query);
    }

    /** SELECT timestampadd(SQL_TSI_WEEK, 10, '2003-05-01 10:20:30') as x FROM my.group1 */
    @Test
    public void testTimestampaddFunctionWeek() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("my.group1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("timestampadd", new BaseExpression[] {getFactory().newConstant("SQL_TSI_WEEK"), getFactory().newConstant(new Integer(10)),
            getFactory().newConstant("2003-05-01 10:20:30")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT timestampadd(SQL_TSI_WEEK, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 "SELECT timestampadd(SQL_TSI_WEEK, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 query);
    }

    /** SELECT timestampadd(SQL_TSI_QUARTER, 10, '2003-05-01 10:20:30') as x FROM my.group1 */
    @Test
    public void testTimestampaddFunctionQuarter() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("my.group1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("timestampadd", new BaseExpression[] {getFactory().newConstant("SQL_TSI_QUARTER"), getFactory().newConstant(new Integer(10)),
            getFactory().newConstant("2003-05-01 10:20:30")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT timestampadd(SQL_TSI_QUARTER, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 "SELECT timestampadd(SQL_TSI_QUARTER, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 query);
    }

    /** SELECT timestampadd(SQL_TSI_YEAR, 10, '2003-05-01 10:20:30') as x FROM my.group1 */
    @Test
    public void testTimestampaddFunctionYear() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("my.group1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("timestampadd", new BaseExpression[] {getFactory().newConstant("SQL_TSI_YEAR"), getFactory().newConstant(new Integer(10)),
            getFactory().newConstant("2003-05-01 10:20:30")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT timestampadd(SQL_TSI_YEAR, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 "SELECT timestampadd(SQL_TSI_YEAR, 10, '2003-05-01 10:20:30') AS x FROM my.group1",
                 query);
    }

    /** SELECT timestampdiff(SQL_TSI_FRAC_SECOND, '2003-05-01 10:20:10', '2003-05-01 10:20:30') as x FROM my.group1 */
    @Test
    public void testTimestampdiffFunctionFracSecond() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("my.group1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("timestampdiff", new BaseExpression[] {getFactory().newConstant("SQL_TSI_FRAC_SECOND"),
            getFactory().newConstant("2003-05-01 10:20:10"), getFactory().newConstant("2003-05-01 10:20:30")});
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().wrapExpression(f));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT timestampdiff(SQL_TSI_FRAC_SECOND, '2003-05-01 10:20:10', '2003-05-01 10:20:30') AS x FROM my.group1",
                 "SELECT timestampdiff(SQL_TSI_FRAC_SECOND, '2003-05-01 10:20:10', '2003-05-01 10:20:30') AS x FROM my.group1",
                 query);
    }

    /** SELECT 5 + 2 + 3 FROM g */
    @Test
    public void testArithmeticOperatorPrecedence1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("+", new BaseExpression[] {getFactory().newConstant(new Integer(5)), getFactory().newConstant(new Integer(2))});
        FunctionImpl f2 = getFactory().newFunction("+", new BaseExpression[] {f, getFactory().newConstant(new Integer(3))});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f2));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 5 + 2 + 3 FROM g", "SELECT ((5 + 2) + 3) FROM g", query);
    }

    /** SELECT 5 + 2 - 3 FROM g */
    @Test
    public void testArithmeticOperatorPrecedence2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("+", new BaseExpression[] {getFactory().newConstant(new Integer(5)), getFactory().newConstant(new Integer(2))});
        FunctionImpl f2 = getFactory().newFunction("-", new BaseExpression[] {f, getFactory().newConstant(new Integer(3))});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f2));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 5 + 2 - 3 FROM g", "SELECT ((5 + 2) - 3) FROM g", query);
    }

    /** SELECT 5 + 2 * 3 FROM g */
    @Test
    public void testArithmeticOperatorPrecedence3() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("*", new BaseExpression[] {getFactory().newConstant(new Integer(2)), getFactory().newConstant(new Integer(3))});
        FunctionImpl f2 = getFactory().newFunction("+", new BaseExpression[] {getFactory().newConstant(new Integer(5)), f});

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f2));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 5 + 2 * 3 FROM g", "SELECT (5 + (2 * 3)) FROM g", query);
    }

    /** SELECT 5 * 2 + 3 FROM g */
    @Test
    public void testArithmeticOperatorPrecedence4() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("*", new BaseExpression[] {getFactory().newConstant(new Integer(5)), getFactory().newConstant(new Integer(2))});
        FunctionImpl f2 = getFactory().newFunction("+", new BaseExpression[] {f, getFactory().newConstant(new Integer(3))});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f2));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 5 * 2 + 3 FROM g", "SELECT ((5 * 2) + 3) FROM g", query);
    }

    /** SELECT 5 * 2 * 3 FROM g */
    @Test
    public void testArithmeticOperatorPrecedence5() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("*", new BaseExpression[] {getFactory().newConstant(new Integer(5)), getFactory().newConstant(new Integer(2))});
        FunctionImpl f2 = getFactory().newFunction("*", new BaseExpression[] {f, getFactory().newConstant(new Integer(3))});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f2));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 5 * 2 * 3 FROM g", "SELECT ((5 * 2) * 3) FROM g", query);
    }

    /** SELECT 1 + 2 * 3 + 4 * 5 FROM g */
    @Test
    public void testArithmeticOperatorPrecedenceMixed1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("*", new BaseExpression[] {getFactory().newConstant(new Integer(2)), getFactory().newConstant(new Integer(3))});
        FunctionImpl f2 = getFactory().newFunction("*", new BaseExpression[] {getFactory().newConstant(new Integer(4)), getFactory().newConstant(new Integer(5))});
        FunctionImpl f3 = getFactory().newFunction("+", new BaseExpression[] {getFactory().newConstant(new Integer(1)), f});
        FunctionImpl f4 = getFactory().newFunction("+", new BaseExpression[] {f3, f2});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f4));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 1 + 2 * 3 + 4 * 5 FROM g", "SELECT ((1 + (2 * 3)) + (4 * 5)) FROM g", query);
    }

    /** SELECT 1 * 2 + 3 * 4 + 5 FROM g */
    @Test
    public void testArithmeticOperatorPrecedenceMixed2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("*", new BaseExpression[] {getFactory().newConstant(new Integer(1)), getFactory().newConstant(new Integer(2))});
        FunctionImpl f2 = getFactory().newFunction("*", new BaseExpression[] {getFactory().newConstant(new Integer(3)), getFactory().newConstant(new Integer(4))});
        FunctionImpl f3 = getFactory().newFunction("+", new BaseExpression[] {f, f2});
        FunctionImpl f4 = getFactory().newFunction("+", new BaseExpression[] {f3, getFactory().newConstant(new Integer(5))});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f4));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 1 * 2 + 3 * 4 + 5 FROM g", "SELECT (((1 * 2) + (3 * 4)) + 5) FROM g", query);
    }

    /** SELECT 5 - 4 - 3 - 2 FROM g --> SELECT ((5 - 4) - 3) - 2 FROM g */
    @Test
    public void testLeftAssociativeExpressions1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("-", new BaseExpression[] {getFactory().newConstant(new Integer(5)), getFactory().newConstant(new Integer(4))});
        FunctionImpl f2 = getFactory().newFunction("-", new BaseExpression[] {f, getFactory().newConstant(new Integer(3))});
        FunctionImpl f3 = getFactory().newFunction("-", new BaseExpression[] {f2, getFactory().newConstant(new Integer(2))});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f3));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 5 - 4 - 3 - 2 FROM g", "SELECT (((5 - 4) - 3) - 2) FROM g", query);
    }

    /** SELECT 5 / 4 / 3 / 2 FROM g --> SELECT ((5 / 4) / 3) / 2 FROM g */
    @Test
    public void testLeftAssociativeExpressions2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("/", new BaseExpression[] {getFactory().newConstant(new Integer(5)), getFactory().newConstant(new Integer(4))});
        FunctionImpl f2 = getFactory().newFunction("/", new BaseExpression[] {f, getFactory().newConstant(new Integer(3))});
        FunctionImpl f3 = getFactory().newFunction("/", new BaseExpression[] {f2, getFactory().newConstant(new Integer(2))});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f3));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 5 / 4 / 3 / 2 FROM g", "SELECT (((5 / 4) / 3) / 2) FROM g", query);
    }

    /** SELECT 'a' || 'b' || 'c' FROM g */
    @Test
    public void testConcatOperator1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("||", new BaseExpression[] {getFactory().newConstant("a"), getFactory().newConstant("b")});
        FunctionImpl f2 = getFactory().newFunction("||", new BaseExpression[] {f, getFactory().newConstant("c")});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f2));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 'a' || 'b' || 'c' FROM g", "SELECT (('a' || 'b') || 'c') FROM g", query);
    }

    /** SELECT 2 + 3 || 5 + 1 * 2 FROM g */
    @Test
    public void testMixedOperators1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        FunctionImpl f = getFactory().newFunction("*", new BaseExpression[] {getFactory().newConstant(new Integer(1)), getFactory().newConstant(new Integer(2))});
        FunctionImpl f2 = getFactory().newFunction("+", new BaseExpression[] {getFactory().newConstant(new Integer(5)), f});
        FunctionImpl f3 = getFactory().newFunction("+", new BaseExpression[] {getFactory().newConstant(new Integer(2)), getFactory().newConstant(new Integer(3))});
        FunctionImpl f4 = getFactory().newFunction("||", new BaseExpression[] {f3, f2});
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(f4));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT 2 + 3 || 5 + 1 * 2 FROM g", "SELECT ((2 + 3) || (5 + (1 * 2))) FROM g", query);
    }

    // ======================= Group By ==============================================

    /** SELECT a FROM m.g GROUP BY b, c */
    @Test
    public void testGroupBy() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        GroupByImpl groupBy = getFactory().newGroupBy(getFactory().newElementSymbol("b"), getFactory().newElementSymbol("c"));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setGroupBy(groupBy);
        helpTest("SELECT a FROM m.g GROUP BY b, c", "SELECT a FROM m.g GROUP BY b, c", query);
    }

    /** SELECT a FROM m.g GROUP BY b, c HAVING b=5*/
    @Test
    public void testGroupByHaving() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        GroupByImpl groupBy = getFactory().newGroupBy(getFactory().newElementSymbol("b"), getFactory().newElementSymbol("c"));

        CompareCriteriaImpl having = getFactory().newCompareCriteria(getFactory().newElementSymbol("b"), Operator.EQ, getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setGroupBy(groupBy);
        query.setHaving(having);
        helpTest("SELECT a FROM m.g GROUP BY b, c HAVING b=5", "SELECT a FROM m.g GROUP BY b, c HAVING b = 5", query);
    }

    /** SELECT COUNT(a) AS c FROM m.g */
    @Test
    public void testAggregateFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newAliasSymbol("c", getFactory().newAggregateSymbol("COUNT", false, getFactory().newElementSymbol("a"))));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT COUNT(a) AS c FROM m.g", "SELECT COUNT(a) AS c FROM m.g", query);
    }

    /** SELECT (COUNT(a)) AS c FROM m.g - this kind of query is generated by ODBC sometimes */
    @Test
    public void testAggregateFunctionWithParens() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newAliasSymbol("c", getFactory().newAggregateSymbol("COUNT", false, getFactory().newElementSymbol("a"))));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT (COUNT(a)) AS c FROM m.g", "SELECT COUNT(a) AS c FROM m.g", query);
    }

    /** SELECT a FROM m.g GROUP BY a HAVING COUNT(b) > 0*/
    @Test
    public void testHavingFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        GroupByImpl groupBy = getFactory().newGroupBy(getFactory().newElementSymbol("a"));

        CriteriaImpl having = getFactory().newCompareCriteria(getFactory().newAggregateSymbol("COUNT", false, getFactory().newElementSymbol("b")),
                                             Operator.GT,
                                             getFactory().newConstant(new Integer(0)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setGroupBy(groupBy);
        query.setHaving(having);

        helpTest("SELECT a FROM m.g GROUP BY a HAVING COUNT(b) > 0", "SELECT a FROM m.g GROUP BY a HAVING COUNT(b) > 0", query);
    }

    /** SELECT a FROM m.g GROUP BY a, b HAVING COUNT(b) > 0 AND b+5 > 0 */
    @Test
    public void testCompoundHaving() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        GroupByImpl groupBy = getFactory().newGroupBy(getFactory().newElementSymbol("a"), getFactory().newElementSymbol("b"));

        CompoundCriteriaImpl having = getFactory().newCompoundCriteria(CompoundCriteriaImpl.AND,
                                                      getFactory().newCompareCriteria(getFactory().newAggregateSymbol("COUNT", false, getFactory().newElementSymbol("b")),
                                                                         Operator.GT,
                                                                         getFactory().newConstant(new Integer(0))),
                                                      getFactory().newCompareCriteria(getFactory().newFunction("+", new BaseExpression[] {
                                                                             getFactory().newElementSymbol("b"), getFactory().newConstant(new Integer(5))}),
                                                                         Operator.GT,
                                                                         getFactory().newConstant(new Integer(0))));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setGroupBy(groupBy);
        query.setHaving(having);

        helpTest("SELECT a FROM m.g GROUP BY a, b HAVING COUNT(b) > 0 AND b+5 > 0",
                 "SELECT a FROM m.g GROUP BY a, b HAVING (COUNT(b) > 0) AND ((b + 5) > 0)",
                 query);
    }

    @Test
    public void testFunctionOfAggregates() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        BaseAggregateSymbol agg1 = getFactory().newAggregateSymbol("COUNT", false, getFactory().newElementSymbol("a"));
        BaseAggregateSymbol agg2 = getFactory().newAggregateSymbol("SUM", false, getFactory().newElementSymbol("a"));
        FunctionImpl f = getFactory().newFunction("*", new BaseExpression[] {agg1, agg2});
        AliasSymbolImpl alias = getFactory().newAliasSymbol("c", getFactory().wrapExpression(f));
        select.addSymbol(alias);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT COUNT(a) * SUM(a) AS c FROM m.g", "SELECT (COUNT(a) * SUM(a)) AS c FROM m.g", query);

    }

    /** SELECT 5-null, a.g1.c1 FROM a.g1 */
    @Test
    public void testArithmeticNullFunction() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newFunction("-", new BaseExpression[] {getFactory().newConstant(new Integer(5)), getFactory().newConstant(null)})));
        select.addSymbol(getFactory().newElementSymbol("a.g1.c1"));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT 5-null, a.g1.c1 FROM a.g1", "SELECT (5 - null), a.g1.c1 FROM a.g1", query);
    }

    /** SELECT 'abc' FROM a.g1 */
    @Test
    public void testStringLiteral() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant("abc")));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT 'abc' FROM a.g1", "SELECT 'abc' FROM a.g1", query);
    }

    /** SELECT 'O''Leary' FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant("O'Leary")));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT 'O''Leary' FROM a.g1", "SELECT 'O''Leary' FROM a.g1", query);
    }

    /** SELECT '''abc''' FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant("'abc'")));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT '''abc''' FROM a.g1", "SELECT '''abc''' FROM a.g1", query);
    }

    /** SELECT 'a''b''c' FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick3() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant("a'b'c")));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT 'a''b''c' FROM a.g1", "SELECT 'a''b''c' FROM a.g1", query);
    }

    /** SELECT " "" " FROM a.g1 */
    @Test
    public void testStringLiteralEscapedTick4() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol(" \" "));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT \" \"\" \" FROM a.g1", "SELECT \" \"\" \" FROM a.g1", query);
    }

    /** SELECT 123456789012 FROM a.g1 */
    @Test
    public void testLongLiteral() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(new Long(123456789012L))));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT 123456789012 FROM a.g1", "SELECT 123456789012 FROM a.g1", query);
    }

    /** SELECT 1000000000000000000000000 FROM a.g1 */
    @Test
    public void testBigIntegerLiteral() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(new BigInteger("1000000000000000000000000"))));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT 1000000000000000000000000 FROM a.g1", "SELECT 1000000000000000000000000 FROM a.g1", query);
    }

    /** SELECT {d'2002-10-02'} FROM m.g1 */
    @Test
    public void testDateLiteral1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(java.sql.Date.valueOf("2002-10-02"))));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT {d'2002-10-02'} FROM m.g1", "SELECT {d'2002-10-02'} FROM m.g1", query);
    }

    /** SELECT {d'2002-9-1'} FROM m.g1 */
    @Test
    public void testDateLiteral2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(java.sql.Date.valueOf("2002-09-01"))));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT {d'2002-09-01'} FROM m.g1", "SELECT {d'2002-09-01'} FROM m.g1", query);
    }

    /** SELECT {t '11:10:00' } FROM m.g1 */
    @Test
    public void testTimeLiteral1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(java.sql.Time.valueOf("11:10:00"))));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT {t '11:10:00' } FROM m.g1", "SELECT {t'11:10:00'} FROM m.g1", query);
    }

    /** SELECT {t '5:10:00'} FROM m.g1 */
    @Test
    public void testTimeLiteral2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(java.sql.Time.valueOf("5:10:00"))));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT {t '05:10:00'} FROM m.g1", "SELECT {t'05:10:00'} FROM m.g1", query);
    }

    /** SELECT {ts'2002-10-02 19:00:02.50'} FROM m.g1 */
    @Test
    public void testTimestampLiteral() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(java.sql.Timestamp.valueOf("2002-10-02 19:00:02.50"))));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT {ts'2002-10-02 19:00:02.50'} FROM m.g1", "SELECT {ts'2002-10-02 19:00:02.5'} FROM m.g1", query);
    }

    /** SELECT {b'true'} FROM m.g1 */
    @Test
    public void testBooleanLiteralTrue() {
        Boolean expected = Boolean.TRUE;
        Class<?> expectedType = DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass();
        String sql = "SELECT {b'true'}";
        String expectedSql = "SELECT TRUE";

        helpTestLiteral(expected, expectedType, sql, expectedSql);
    }

    /** SELECT TRUE FROM m.g1 */
    @Test
    public void testBooleanLiteralTrue2() {
        Boolean expected = Boolean.TRUE;
        Class<?> expectedType = DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass();
        String sql = "SELECT TRUE";
        String expectedSql = "SELECT TRUE";

        helpTestLiteral(expected, expectedType, sql, expectedSql);
    }

    /** SELECT {b'false'} FROM m.g1 */
    @Test
    public void testBooleanLiteralFalse() {
        Boolean expected = Boolean.FALSE;
        Class<?> expectedType = DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass();
        String sql = "SELECT {b'false'}";
        String expectedSql = "SELECT FALSE";

        helpTestLiteral(expected, expectedType, sql, expectedSql);
    }

    /** SELECT FALSE FROM m.g1 */
    @Test
    public void testBooleanLiteralFalse2() {
        Boolean expected = Boolean.FALSE;
        Class<?> expectedType = DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass();
        String sql = "SELECT {b'false'}";
        String expectedSql = "SELECT FALSE";

        helpTestLiteral(expected, expectedType, sql, expectedSql);
    }

    @Test
    public void testBooleanLiteralUnknown() {
        Boolean expected = null;
        Class<?> expectedType = DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass();
        String sql = "SELECT {b'unknown'}";
        String expectedSql = "SELECT UNKNOWN";

        helpTestLiteral(expected, expectedType, sql, expectedSql);
    }

    @Test
    public void testBooleanLiteralUnknown2() {
        Boolean expected = null;
        Class<?> expectedType = DefaultDataTypeManager.DefaultDataTypes.BOOLEAN.getTypeClass();
        String sql = "SELECT UNKNOWN";
        String expectedSql = "SELECT UNKNOWN";

        helpTestLiteral(expected, expectedType, sql, expectedSql);
    }

    /** SELECT DISTINCT a FROM g */
    @Test
    public void testSelectDistinct() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));
        select.setDistinct(true);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT DISTINCT a FROM g", "SELECT DISTINCT a FROM g", query);
    }

    /** SELECT ALL a FROM g */
    @Test
    public void testSelectAll() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));
        select.setDistinct(false);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT ALL a FROM g", "SELECT a FROM g", query);
    }

    //=========================Aliasing==============================================

    /** SELECT a AS myA, b FROM g */
    @Test
    public void testAliasInSelect() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        AliasSymbolImpl as = getFactory().newAliasSymbol("myA", getFactory().newElementSymbol("a"));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);
        select.addSymbol(getFactory().newElementSymbol("b"));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT a AS myA, b FROM g", "SELECT a AS myA, b FROM g", query);
    }

    /** SELECT a myA, b FROM g, h */
    @Test
    public void testAliasInSelect2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        GroupSymbolImpl h = getFactory().newGroupSymbol("h");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);
        from.addGroup(h);

        AliasSymbolImpl as = getFactory().newAliasSymbol("myA", getFactory().newElementSymbol("a"));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);
        select.addSymbol(getFactory().newElementSymbol("b"));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT a myA, b FROM g, h", "SELECT a AS myA, b FROM g, h", query);
    }

    /** SELECT myG.a FROM g AS myG */
    @Test
    public void testAliasInFrom() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("myG", "g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("myG.a"));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT myG.a FROM g AS myG", "SELECT myG.a FROM g AS myG", query);
    }

    /** SELECT myG.*, myH.b FROM g AS myG, h AS myH */
    @Test
    public void testAliasesInFrom() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("myG", "g");
        GroupSymbolImpl h = getFactory().newGroupSymbol("myH", "h");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);
        from.addGroup(h);

        SelectImpl select = getFactory().newSelect();
        MultipleElementSymbolImpl myG = getFactory().newMultipleElementSymbol("myG");
        select.addSymbol(myG);
        select.addSymbol(getFactory().newElementSymbol("myH.b"));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT myG.*, myH.b FROM g AS myG, h AS myH", "SELECT myG.*, myH.b FROM g AS myG, h AS myH", query);
    }

    /** SELECT myG.a, myH.b FROM g myG, h myH */
    @Test
    public void testHiddenAliasesInFrom() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("myG", "g");
        GroupSymbolImpl h = getFactory().newGroupSymbol("myH", "h");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);
        from.addGroup(h);

        SelectImpl select = getFactory().newSelect();
        MultipleElementSymbolImpl myG = getFactory().newMultipleElementSymbol("myG");
        select.addSymbol(myG);
        select.addSymbol(getFactory().newElementSymbol("myH.b"));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT myG.*, myH.b FROM g myG, h myH", "SELECT myG.*, myH.b FROM g AS myG, h AS myH", query);
    }

    // ======================= Misc ==============================================

    /** Select a From db.g Where a IS NULL */
    @Test
    public void testIsNullCriteria1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        CriteriaImpl crit = getFactory().newIsNullCriteria(a);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("Select a From db.g Where a IS NULL", "SELECT a FROM db.g WHERE a IS NULL", query);
    }

    /** Select a From db.g Where a IS NOT NULL */
    @Test
    public void testIsNullCriteria2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        IsNullCriteriaImpl crit = getFactory().newIsNullCriteria(a);
        crit.setNegated(true);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("Select a From db.g Where a IS NOT NULL", "SELECT a FROM db.g WHERE a IS NOT NULL", query);
    }

    /** Select a From db.g Where Not a IS NULL */
    @Test
    public void testNotIsNullCriteria() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        CriteriaImpl crit = getFactory().newNotCriteria(getFactory().newIsNullCriteria(a));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("Select a From db.g Where Not a IS NULL", "SELECT a FROM db.g WHERE NOT (a IS NULL)", query);
    }

    /** SELECT a from db.g where a <> "value" */
    @Test
    public void testStringNotEqualDoubleTicks() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression ex = getFactory().newElementSymbol("value");
        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.NE, ex);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where a <> \"value\"", "SELECT a FROM db.g WHERE a <> \"value\"", query);
    }

    /** SELECT a from db.g where a != "value" */
    @Test
    public void testNotEquals2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression constant = getFactory().newConstant("value");
        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.NE, constant);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where a != 'value'", "SELECT a FROM db.g WHERE a <> 'value'", query);
    }

    /** SELECT a from db."g" where a = 5 */
    @Test
    public void testPartlyQuotedGroup() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.EQ, getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.\"g\" where a = 5", "SELECT a FROM db.g WHERE a = 5", query);
    }

    /** SELECT a from "db"."g" where a = 5 */
    @Test
    public void testFullyQuotedGroup() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.EQ, getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from \"db\".\"g\" where a = 5", "SELECT a FROM db.g WHERE a = 5", query);
    }

    /** SELECT "db".g.a from db.g */
    @Test
    public void testPartlyQuotedElement1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("db.g.a");
        select.addSymbol(a);

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT \"db\".g.a from db.g", "SELECT db.g.a FROM db.g", query);
    }

    /** SELECT "db"."g".a from db.g */
    @Test
    public void testPartlyQuotedElement2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("db.g.a");
        select.addSymbol(a);

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT \"db\".\"g\".a from db.g", "SELECT db.g.a FROM db.g", query);
    }

    /** SELECT "db"."g"."a" from db.g */
    @Test
    public void testPartlyQuotedElement3() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("db.g.a");
        select.addSymbol(a);

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT \"db\".\"g\".\"a\" from db.g", "SELECT db.g.a FROM db.g", query);
    }

    /** SELECT ""g"".""a" from db.g */
    @Test
    public void testStringLiteralLikeQuotedElement() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("g\".\"a"));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT \"g\"\".\"\"a\" from g", "SELECT \"g\"\"\".\"\"\"a\" FROM g", query);
    }

    /** SELECT ""g"".""a" from db.g */
    @Test
    public void testStringLiteralLikeQuotedElement1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant("g\".\"a")));

        QueryImpl query = getFactory().newQuery(select, from);
        ParseInfo info = new ParseInfo();
        info.setAnsiQuotedIdentifiers(false);
        helpTest("SELECT \"g\"\".\"\"a\" from g", "SELECT 'g\".\"a' FROM g", query, info);
    }

    /** SELECT g.x AS "select" FROM g */
    @Test
    public void testQuotedAlias() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        AliasSymbolImpl a = getFactory().newAliasSymbol("select", getFactory().newElementSymbol("g.x"));
        select.addSymbol(a);

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT g.x AS \"select\" FROM g", "SELECT g.x AS \"select\" FROM g", query);
    }

    /** SELECT g.x AS year FROM g */
    @Test
    public void testQuotedAlias2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        AliasSymbolImpl a = getFactory().newAliasSymbol("year", getFactory().newElementSymbol("g.x"));
        select.addSymbol(a);

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT g.x AS \"year\" FROM g", "SELECT g.x AS \"year\" FROM g", query);
    }

    @Test
    public void testQuotedAlias3() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        AliasSymbolImpl a = getFactory().newAliasSymbol("some year", getFactory().newElementSymbol("g.x"));
        select.addSymbol(a);

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT g.x AS \"some year\" FROM g", "SELECT g.x AS \"some year\" FROM g", query);
    }

    /** SELECT g."select" FROM g */
    @Test
    public void testReservedWordElement1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("g.select");
        select.addSymbol(a);

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT g.\"select\" FROM g", "SELECT g.\"select\" FROM g", query);
    }

    /** SELECT newModel5.ResultSetDocument.MappingClasses.from.from.Query1InputSet.x FROM newModel5.ResultSetDocument.MappingClasses.from.from.Query1InputSet */
    @Test
    public void testReservedWordElement2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("newModel5.ResultSetDocument.MappingClasses.from.from.Query1InputSet");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("newModel5.ResultSetDocument.MappingClasses.from.from.Query1InputSet.x");
        select.addSymbol(a);

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT newModel5.ResultSetDocument.MappingClasses.from.from.Query1InputSet.x FROM newModel5.ResultSetDocument.MappingClasses.from.from.Query1InputSet",
                 "SELECT newModel5.ResultSetDocument.MappingClasses.\"from\".\"from\".Query1InputSet.x FROM newModel5.ResultSetDocument.MappingClasses.\"from\".\"from\".Query1InputSet",
                 query);
    }

    /** SELECT * FROM newModel5.ResultSetDocument.MappingClasses.from.from.Query1InputSet  */
    @Test
    public void testReservedWordGroup1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("newModel5.ResultSetDocument.MappingClasses.from.from.Query1InputSet");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM newModel5.ResultSetDocument.MappingClasses.from.from.Query1InputSet",
                 "SELECT * FROM newModel5.ResultSetDocument.MappingClasses.\"from\".\"from\".Query1InputSet",
                 query);
    }

    /** SELECT * FROM newModel5."ResultSetDocument.MappingClasses.from.from.Query1InputSet"  */
    @Test
    public void testReservedWordGroup2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("newModel5.ResultSetDocument.MappingClasses.from.from.Query1InputSet");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT * FROM newModel5.\"ResultSetDocument.MappingClasses.from.from.Query1InputSet\"",
                 "SELECT * FROM newModel5.ResultSetDocument.MappingClasses.\"from\".\"from\".Query1InputSet",
                 query);
    }

    /** SELECT * FROM model.doc WHERE ab.cd.@ef = 'abc' */
    @Test
    public void testXMLCriteriaWithAttribute() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("model.doc");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());

        QueryImpl query = getFactory().newQuery(select, from);

        ElementSymbolImpl elem = getFactory().newElementSymbol("ab.cd.@ef");
        query.setCriteria(getFactory().newCompareCriteria(elem, Operator.EQ, getFactory().newConstant("abc")));

        helpTest("SELECT * FROM model.doc WHERE ab.cd.@ef = 'abc'", "SELECT * FROM model.doc WHERE ab.cd.@ef = 'abc'", query);
    }

    /** SELECT a from db.g where a <> 'value' */
    @Test
    public void testStringNotEqual() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression constant = getFactory().newConstant("value");
        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.NE, constant);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where a <> 'value'", "SELECT a FROM db.g WHERE a <> 'value'", query);
    }

    /** SELECT a from db.g where a BETWEEN 1000 AND 2000 */
    @Test
    public void testBetween1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression constant1 = getFactory().newConstant(new Integer(1000));
        BaseExpression constant2 = getFactory().newConstant(new Integer(2000));
        CriteriaImpl crit = getFactory().newBetweenCriteria(a, constant1, constant2);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where a BETWEEN 1000 AND 2000", "SELECT a FROM db.g WHERE a BETWEEN 1000 AND 2000", query);
    }

    /** SELECT a from db.g where a NOT BETWEEN 1000 AND 2000 */
    @Test
    public void testBetween2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression constant1 = getFactory().newConstant(new Integer(1000));
        BaseExpression constant2 = getFactory().newConstant(new Integer(2000));
        BetweenCriteriaImpl crit = getFactory().newBetweenCriteria(a, constant1, constant2);
        crit.setNegated(true);
        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where a NOT BETWEEN 1000 AND 2000",
                 "SELECT a FROM db.g WHERE a NOT BETWEEN 1000 AND 2000",
                 query);
    }

    /** SELECT a from db.g where a < 1000 */
    @Test
    public void testCompareLT() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression constant = getFactory().newConstant(new Integer(1000));
        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.LT, constant);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where a < 1000", "SELECT a FROM db.g WHERE a < 1000", query);
    }

    /** SELECT a from db.g where a > 1000 */
    @Test
    public void testCompareGT() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression constant = getFactory().newConstant(new Integer(1000));
        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.GT, constant);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where a > 1000", "SELECT a FROM db.g WHERE a > 1000", query);
    }

    /** SELECT a from db.g where a <= 1000 */
    @Test
    public void testCompareLE() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression constant = getFactory().newConstant(new Integer(1000));
        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.LE, constant);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where a <= 1000", "SELECT a FROM db.g WHERE a <= 1000", query);
    }

    /** SELECT a from db.g where a >= 1000 */
    @Test
    public void testCompareGE() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression constant = getFactory().newConstant(new Integer(1000));
        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.GE, constant);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where a >= 1000", "SELECT a FROM db.g WHERE a >= 1000", query);
    }

    /** SELECT a from db.g where b = x and a = 1000 */
    @Test
    public void testCompoundCompare1() {
        helpTestCompoundCompare("SELECT a from db.g where b = x and a = 1000");
    }

    /** SELECT a from db.g where (b = x and a = 1000) */
    @Test
    public void testCompoundCompare2() {
        helpTestCompoundCompare("SELECT a from db.g where (b = x and a = 1000)");
    }

    /** SELECT a from db.g where ((b = x) and (a = 1000)) */
    @Test
    public void testCompoundCompare3() {
        helpTestCompoundCompare("SELECT a from db.g where ((b = x) and (a = 1000))");
    }

    /** SELECT a from db.g where (((b = x) and (a = 1000))) */
    @Test
    public void testCompoundCompare4() {
        helpTestCompoundCompare("SELECT a from db.g where (((b = x) and (a = 1000)))");
    }

    /** SELECT a FROM db.g WHERE (b = x) AND (a = 1000) */
    protected void helpTestCompoundCompare(String testSQL) {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        CriteriaImpl crit1 = getFactory().newCompareCriteria(getFactory().newElementSymbol("b"), Operator.EQ, getFactory().newElementSymbol("x"));
        BaseExpression constant = getFactory().newConstant(new Integer(1000));
        CriteriaImpl crit2 = getFactory().newCompareCriteria(a, Operator.EQ, constant);
        CriteriaImpl crit = getFactory().newCompoundCriteria(CompoundCriteriaImpl.AND, crit1, crit2);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest(testSQL, "SELECT a FROM db.g WHERE (b = x) AND (a = 1000)", query);
    }

    /** SELECT a FROM db.g WHERE b IN (1000,5000)*/
    @Test
    public void testSetCriteria0() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        BaseExpression constant1 = getFactory().newConstant(new Integer(1000));
        BaseExpression constant2 = getFactory().newConstant(new Integer(5000));
        List<BaseExpression> constants = new ArrayList<BaseExpression>(2);
        constants.add(constant1);
        constants.add(constant2);
        CriteriaImpl crit = getFactory().newSetCriteria(getFactory().newElementSymbol("b"), constants);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a FROM db.g WHERE b IN (1000,5000)", "SELECT a FROM db.g WHERE b IN (1000, 5000)", query);
    }

    /** SELECT a FROM db.g WHERE b NOT IN (1000,5000)*/
    @Test
    public void testSetCriteria1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        BaseExpression constant1 = getFactory().newConstant(new Integer(1000));
        BaseExpression constant2 = getFactory().newConstant(new Integer(5000));
        List<BaseExpression> constants = new ArrayList<BaseExpression>(2);
        constants.add(constant1);
        constants.add(constant2);
        SetCriteriaImpl crit = getFactory().newSetCriteria(getFactory().newElementSymbol("b"), constants);
        crit.setNegated(true);
        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a FROM db.g WHERE b NOT IN (1000,5000)", "SELECT a FROM db.g WHERE b NOT IN (1000, 5000)", query);
    }

    // ================================== order by ==================================

    /** SELECT a FROM db.g WHERE b = aString order by c*/
    @Test
    public void testOrderBy() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        CriteriaImpl crit = getFactory().newCompareCriteria(getFactory().newElementSymbol("b"), Operator.EQ, getFactory().newElementSymbol("aString"));

        ArrayList elements = new ArrayList();
        elements.add(getFactory().newElementSymbol("c"));
        OrderByImpl orderBy = getFactory().newOrderBy(elements);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        query.setOrderBy(orderBy);
        helpTest("SELECT a FROM db.g WHERE b = aString ORDER BY c", "SELECT a FROM db.g WHERE b = aString ORDER BY c", query);
    }

    /** SELECT a FROM db.g WHERE b = aString order by c desc*/
    @Test
    public void testOrderByDesc() {
        List<BaseExpression> elements = new ArrayList<BaseExpression>();
        elements.add(getFactory().newElementSymbol("c"));
        List<Boolean> orderTypes = new ArrayList<Boolean>();
        orderTypes.add(Boolean.FALSE);
        OrderByImpl orderBy = getFactory().newOrderBy(elements, orderTypes);

        QueryImpl query = getOrderByQuery(orderBy);
        helpTest("SELECT a FROM db.g WHERE b = aString ORDER BY c desc",
                 "SELECT a FROM db.g WHERE b = aString ORDER BY c DESC",
                 query);
    }

    protected QueryImpl getOrderByQuery(OrderByImpl orderBy) {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        CriteriaImpl crit = getFactory().newCompareCriteria(getFactory().newElementSymbol("b"), Operator.EQ, getFactory().newElementSymbol("aString"));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        query.setOrderBy(orderBy);
        return query;
    }

    /** SELECT a FROM db.g WHERE b = aString order by c,d*/
    @Test
    public void testOrderBys() {
        ArrayList elements = new ArrayList();
        elements.add(getFactory().newElementSymbol("c"));
        elements.add(getFactory().newElementSymbol("d"));
        OrderByImpl orderBy = getFactory().newOrderBy(elements);

        QueryImpl query = getOrderByQuery(orderBy);
        helpTest("SELECT a FROM db.g WHERE b = aString ORDER BY c,d", "SELECT a FROM db.g WHERE b = aString ORDER BY c, d", query);
    }

    /** SELECT a FROM db.g WHERE b = aString order by c desc,d desc*/
    @Test
    public void testOrderBysDesc() {
        List<ElementSymbolImpl> elements = new ArrayList<ElementSymbolImpl>();
        elements.add(getFactory().newElementSymbol("c"));
        elements.add(getFactory().newElementSymbol("d"));
        List<Boolean> orderTypes = new ArrayList<Boolean>();
        orderTypes.add(Boolean.FALSE);
        orderTypes.add(Boolean.FALSE);
        OrderByImpl orderBy = getFactory().newOrderBy(elements, orderTypes);

        QueryImpl query = getOrderByQuery(orderBy);
        helpTest("SELECT a FROM db.g WHERE b = aString ORDER BY c desc,d desc",
                 "SELECT a FROM db.g WHERE b = aString ORDER BY c DESC, d DESC",
                 query);
    }

    /** SELECT a FROM db.g WHERE b = aString order by c desc,d*/
    @Test
    public void testMixedOrderBys() {
        ArrayList<ElementSymbolImpl> elements = new ArrayList<ElementSymbolImpl>();
        elements.add(getFactory().newElementSymbol("c"));
        elements.add(getFactory().newElementSymbol("d"));
        ArrayList<Boolean> orderTypes = new ArrayList<Boolean>();
        orderTypes.add(Boolean.FALSE);
        orderTypes.add(Boolean.TRUE);
        OrderByImpl orderBy = getFactory().newOrderBy(elements, orderTypes);

        QueryImpl query = getOrderByQuery(orderBy);
        helpTest("SELECT a FROM db.g WHERE b = aString ORDER BY c desc,d",
                 "SELECT a FROM db.g WHERE b = aString ORDER BY c DESC, d",
                 query);
    }

    @Test
    public void testOrderByNullOrdering() {
        OrderByImpl orderBy = getFactory().newOrderBy();
        OrderByItemImpl item = getFactory().newOrderByItem(getFactory().newElementSymbol("c"), true);
        item.setNullOrdering(SortSpecification.NullOrdering.FIRST);
        orderBy.getOrderByItems().add(item);
        item = getFactory().newOrderByItem(getFactory().newElementSymbol("d"), false);
        item.setNullOrdering(SortSpecification.NullOrdering.LAST);
        orderBy.getOrderByItems().add(item);

        QueryImpl query = getOrderByQuery(orderBy);
        helpTest("SELECT a FROM db.g WHERE b = aString ORDER BY c NULLS FIRST,d desc nulls last",
                 "SELECT a FROM db.g WHERE b = aString ORDER BY c NULLS FIRST, d DESC NULLS LAST",
                 query);
    }

    // ================================== match ====================================

    /** SELECT a FROM db.g WHERE b LIKE 'aString'*/
    @Test
    public void testLike0() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression string1 = getFactory().newConstant("aString");
        CriteriaImpl crit = getFactory().newMatchCriteria(getFactory().newElementSymbol("b"), string1);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a FROM db.g WHERE b LIKE 'aString'", "SELECT a FROM db.g WHERE b LIKE 'aString'", query);
    }

    /** SELECT a FROM db.g WHERE b NOT LIKE 'aString'*/
    @Test
    public void testLike1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression string1 = getFactory().newConstant("aString");
        MatchCriteriaImpl crit = getFactory().newMatchCriteria(getFactory().newElementSymbol("b"), string1);
        crit.setNegated(true);
        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a FROM db.g WHERE b NOT LIKE 'aString'", "SELECT a FROM db.g WHERE b NOT LIKE 'aString'", query);
    }

    /** SELECT a from db.g where b like '#String' escape '#'*/
    @Test
    public void testLikeWithEscape() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        BaseExpression string1 = getFactory().newConstant("#String");
        CriteriaImpl crit = getFactory().newMatchCriteria(getFactory().newElementSymbol("b"), string1, '#');

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where b like '#String' escape '#'",
                 "SELECT a FROM db.g WHERE b LIKE '#String' ESCAPE '#'",
                 query);
    }

    /** SELECT "date"."time" from db.g */
    @Test
    public void testReservedWordsInElement() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("date.time");
        select.addSymbol(a);

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT \"date\".\"time\" from db.g", "SELECT \"date\".\"time\" FROM db.g", query);

    }

    /** SELECT a */
    @Test
    public void testNoFromClause() {
        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        BaseExpression b = getFactory().wrapExpression(getFactory().newConstant(new Integer(5), Integer.class));
        select.addSymbol(a);
        select.addSymbol(b);
        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);
        helpTest("SELECT a, 5", "SELECT a, 5", query);
    }

    // ==================== misc queries that should fail ===========================

    /** SELECT a or b from g */
    @Test
    public void testOrInSelect() {
        QueryImpl query = getFactory().newQuery();
        CompoundCriteriaImpl compoundCriteria = getFactory().newCompoundCriteria(CompoundCriteriaImpl.OR,
                                                                getFactory().newExpressionCriteria(getFactory().newElementSymbol("a")),
                                                                getFactory().newExpressionCriteria(getFactory().newElementSymbol("b")));
        query.setSelect(getFactory().newSelect(Arrays.asList(getFactory().wrapExpression(compoundCriteria))));
        helpTest("select a or b", "SELECT (a) OR (b)", query);
    }

    /** SELECT a FROM g WHERE a LIKE x*/
    @Test
    public void testLikeWOConstant() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        ElementSymbolImpl x = getFactory().newElementSymbol("x");
        CriteriaImpl crit = getFactory().newMatchCriteria(a, x);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a FROM g WHERE a LIKE x", "SELECT a FROM g WHERE a LIKE x", query);
    }

    /** Test reusability of parser */
    @Test
    public void testReusabilityOfParserObject() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT a FROM m.g", "SELECT a FROM m.g", query);

        helpTest("SELECT a FROM m.g", "SELECT a FROM m.g", query);
    }

    /** SELECT a from db.g where b LIKE ? */
    @Test
    public void testParameter1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        ReferenceImpl ref1 = getFactory().newReference(0);
        CriteriaImpl crit = getFactory().newMatchCriteria(getFactory().newElementSymbol("b"), ref1);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a from db.g where b LIKE ?", "SELECT a FROM db.g WHERE b LIKE ?", query);
    }

    /** SELECT a from db.g where b LIKE ? */
    @Test
    public void testParameter2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ReferenceImpl ref0 = getFactory().newReference(0);
        BaseExpression expr = getFactory().wrapExpression(ref0);
        select.addSymbol(expr);

        ReferenceImpl ref1 = getFactory().newReference(1);
        CriteriaImpl crit = getFactory().newMatchCriteria(getFactory().newElementSymbol("b"), ref1);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT ? from db.g where b LIKE ?", "SELECT ? FROM db.g WHERE b LIKE ?", query);
    }

    /** SELECT a, b FROM (SELECT c FROM m.g) AS y */
    @Test
    public void testSubquery1() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl symbol = getFactory().newElementSymbol("c");
        select.addSymbol(symbol);

        QueryImpl query = getFactory().newQuery(select, from);

        SubqueryFromClauseImpl sfc = getFactory().newSubqueryFromClause("y", query);
        FromImpl from2 = getFactory().newFrom();
        from2.addClause(sfc);

        SelectImpl select2 = getFactory().newSelect();
        select2.addSymbol(getFactory().newElementSymbol("a"));
        select2.addSymbol(getFactory().newElementSymbol("b"));

        QueryImpl query2 = getFactory().newQuery();
        query2.setSelect(select2);
        query2.setFrom(from2);

        helpTest("SELECT a, b FROM (SELECT c FROM m.g) AS y", "SELECT a, b FROM (SELECT c FROM m.g) AS y", query2);
    }

    /** SELECT a, b FROM ((SELECT c FROM m.g)) AS y */
    @Test
    public void testSubquery1a() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl symbol = getFactory().newElementSymbol("c");
        select.addSymbol(symbol);

        QueryImpl query = getFactory().newQuery(select, from);

        SubqueryFromClauseImpl sfc = getFactory().newSubqueryFromClause("y", query);
        FromImpl from2 = getFactory().newFrom();
        from2.addClause(sfc);

        SelectImpl select2 = getFactory().newSelect();
        select2.addSymbol(getFactory().newElementSymbol("a"));
        select2.addSymbol(getFactory().newElementSymbol("b"));

        QueryImpl query2 = getFactory().newQuery();
        query2.setSelect(select2);
        query2.setFrom(from2);

        helpTest("SELECT a, b FROM ((SELECT c FROM m.g)) AS y", "SELECT a, b FROM (SELECT c FROM m.g) AS y", query2);
    }

    /** SELECT a, b FROM m.g1 JOIN (SELECT c FROM m.g2) AS y ON m.g1.a = y.c */
    @Test
    public void testSubquery2() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g2");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl symbol = getFactory().newElementSymbol("c");
        select.addSymbol(symbol);

        QueryImpl query = getFactory().newQuery(select, from);

        UnaryFromClauseImpl ufc = getFactory().newUnaryFromClause("m.g1");
        SubqueryFromClauseImpl sfc = getFactory().newSubqueryFromClause("y", query);
        CompareCriteriaImpl join = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g1.a"), Operator.EQ, getFactory().newElementSymbol("y.c"));
        List crits = new ArrayList();
        crits.add(join);
        JoinPredicateImpl jp = getFactory().newJoinPredicate(ufc, sfc, JoinTypeTypes.JOIN_INNER, crits);
        FromImpl from2 = getFactory().newFrom();
        from2.addClause(jp);

        SelectImpl select2 = getFactory().newSelect();
        select2.addSymbol(getFactory().newElementSymbol("a"));
        select2.addSymbol(getFactory().newElementSymbol("b"));

        QueryImpl query2 = getFactory().newQuery();
        query2.setSelect(select2);
        query2.setFrom(from2);

        helpTest("SELECT a, b FROM m.g1 JOIN (SELECT c FROM m.g2) AS y ON m.g1.a = y.c",
                 "SELECT a, b FROM m.g1 INNER JOIN (SELECT c FROM m.g2) AS y ON m.g1.a = y.c",
                 query2);
    }

    /** INSERT INTO m.g (a) VALUES (?) */
    @Test
    public void testInsertWithReference() {
        InsertImpl insert = getFactory().newInsert();
        insert.setGroup(getFactory().newGroupSymbol("m.g"));
        List vars = new ArrayList();
        vars.add(getFactory().newElementSymbol("a"));
        insert.setVariables(vars);
        List values = new ArrayList();
        values.add(getFactory().newReference(0));
        insert.setValues(values);
        helpTest("INSERT INTO m.g (a) VALUES (?)", "INSERT INTO m.g (a) VALUES (?)", insert);
    }

    @Test
    public void testStoredQueryWithNoParameter() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setProcedureName("proc1");
        helpTest("exec proc1()", "EXEC proc1()", storedQuery);
        helpTest("execute proc1()", "EXEC proc1()", storedQuery);
    }

    @Test
    public void testStoredQueryWithNoParameter2() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setProcedureName("proc1");

        FromImpl from = getFactory().newFrom();
        SubqueryFromClauseImpl sfc = getFactory().newSubqueryFromClause("X", storedQuery);
        from.addClause(sfc);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("X.A"));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT X.A FROM (exec proc1()) AS X", "SELECT X.A FROM (EXEC proc1()) AS X", query);
    }

    @Test
    public void testStoredQuery() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setProcedureName("proc1");
        SPParameterImpl parameter = getFactory().newSPParameter(1, getFactory().newConstant("param1"));
        parameter.setParameterType(ParameterInfo.IN);
        storedQuery.addParameter(parameter);
        helpTest("Exec proc1('param1')", "EXEC proc1('param1')", storedQuery);
        helpTest("execute proc1('param1')", "EXEC proc1('param1')", storedQuery);
    }

    @Test
    public void testStoredQuery2() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setProcedureName("proc1");
        SPParameterImpl parameter = getFactory().newSPParameter(1, getFactory().newConstant("param1"));
        storedQuery.addParameter(parameter);
        FromImpl from = getFactory().newFrom();
        SubqueryFromClauseImpl sfc = getFactory().newSubqueryFromClause("X", storedQuery);
        from.addClause(sfc);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("X.A"));

        QueryImpl query = getFactory().newQuery(select, from);
        helpTest("SELECT X.A FROM (exec proc1('param1')) AS X", "SELECT X.A FROM (EXEC proc1('param1')) AS X", query);
    }

    @Test
    public void testStoredQuery2SanityCheck() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setProcedureName("proc1");
        SPParameterImpl parameter = getFactory().newSPParameter(1, getFactory().newConstant("param1"));
        storedQuery.addParameter(parameter);
        FromImpl from = getFactory().newFrom();
        SubqueryFromClauseImpl sfc = getFactory().newSubqueryFromClause("x", storedQuery);
        from.addClause(sfc);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("x.a"));

        helpTest("exec proc1('param1')", "EXEC proc1('param1')", storedQuery);
    }

    @Test
    public void testIfStatement() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        String shortType = new String("short");
        StatementImpl ifStmt = getFactory().newDeclareStatement(a, shortType);

        ElementSymbolImpl b = getFactory().newElementSymbol("b");
        StatementImpl elseStmt = getFactory().newDeclareStatement(b, shortType);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(ifStmt);

        BlockImpl elseBlock = getFactory().newBlock();
        elseBlock.addStatement(elseStmt);

        ElementSymbolImpl c = getFactory().newElementSymbol("c");
        CriteriaImpl crit = getFactory().newCompareCriteria(c, Operator.EQ, getFactory().newConstant(new Integer(5)));

        IfStatementImpl stmt = getFactory().newIfStatement(crit, ifBlock);
        stmt.setElseBlock(elseBlock);

        helpStmtTest("IF(c = 5) BEGIN DECLARE short a; END ELSE BEGIN DECLARE short b; END", "IF(c = 5)" + "\n" + "BEGIN" + "\n"
                                                                                             + "DECLARE short a;" + "\n" + "END"
                                                                                             + "\n" + "ELSE" + "\n" + "BEGIN"
                                                                                             + "\n" + "DECLARE short b;" + "\n"
                                                                                             + "END", stmt);
    }

    @Test
    public void testAssignStatement() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ, getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        BaseExpression expr = getFactory().newConstant("aString");

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(a, query);
        AssignmentStatementImpl exprStmt = getFactory().newAssignmentStatement(a, expr);

        helpStmtTest("a = SELECT a1 FROM g WHERE a2 = 5;", "a = (SELECT a1 FROM g WHERE a2 = 5);", queryStmt);

        helpStmtTest("a = 'aString';", "a = 'aString';", exprStmt);
    }

    @Test
    public void testDeclareStatement() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        String type = new String("short");
        DeclareStatementImpl stmt = getFactory().newDeclareStatement(a, type);

        helpStmtTest("DECLARE short a;", "DECLARE short a;", stmt);
    }

    @Test
    public void testDeclareStatementWithAssignment() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        String type = new String("short");
        DeclareStatementImpl stmt = getFactory().newDeclareStatement(a, type, getFactory().newConstant(null));

        helpStmtTest("DECLARE short a = null;", "DECLARE short a = null;", stmt);
    }

    @Test
    public void testDeclareStatementWithAssignment1() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        String type = new String("string");
        DeclareStatementImpl stmt = getFactory().newDeclareStatement(a, type, getFactory().newScalarSubquery(sampleQuery()));

        helpStmtTest("DECLARE string a = SELECT a1 FROM g WHERE a2 = 5;",
                     "DECLARE string a = (SELECT a1 FROM g WHERE a2 = 5);",
                     stmt);
    }

    @Test
    public void testStatement() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        String type = new String("short");
        DeclareStatementImpl declStmt = getFactory().newDeclareStatement(a, type);
        StatementImpl stmt = declStmt;

        helpStmtTest("DECLARE short a;", "DECLARE short a;", stmt);
    }

    @Test
    public void testCommandStatement() throws Exception {
        QueryImpl query = sampleQuery();

        CommandImpl sqlCmd = query;
        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(sqlCmd);

        helpStmtTest("SELECT a1 FROM g WHERE a2 = 5;", "SELECT a1 FROM g WHERE a2 = 5;", cmdStmt);
    }

    protected QueryImpl sampleQuery() {
        List<ElementSymbolImpl> symbols = new ArrayList<ElementSymbolImpl>();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ, getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);
        return query;
    }

    @Test
    public void testDynamicCommandStatement() throws Exception {
        List symbols = new ArrayList();

        ElementSymbolImpl a1 = getFactory().newElementSymbol("a1");
        a1.setType(DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass());
        symbols.add(a1);

        DynamicCommandImpl sqlCmd = getFactory().newDynamicCommand();
        BaseExpression sql = getFactory().newConstant("SELECT a1 FROM g WHERE a2 = 5");

        sqlCmd.setSql(sql);
        sqlCmd.setAsColumns(symbols);
        sqlCmd.setAsClauseSet(true);

        sqlCmd.setIntoGroup(getFactory().newGroupSymbol("#g"));

        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(sqlCmd);

        helpStmtTest("exec string 'SELECT a1 FROM g WHERE a2 = 5' as a1 string into #g;",
                     "EXECUTE IMMEDIATE 'SELECT a1 FROM g WHERE a2 = 5' AS a1 string INTO #g;",
                     cmdStmt);
    }

    //sql is a variable, also uses the as, into, and update clauses
    @Test
    public void testDynamicCommandStatement1() throws Exception {
        List<ElementSymbolImpl> symbols = new ArrayList<ElementSymbolImpl>();

        ElementSymbolImpl a1 = getFactory().newElementSymbol("a1");
        a1.setType(DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass());
        symbols.add(a1);

        ElementSymbolImpl a2 = getFactory().newElementSymbol("a2");
        a2.setType(DefaultDataTypeManager.DefaultDataTypes.INTEGER.getTypeClass());
        symbols.add(a2);

        DynamicCommandImpl sqlCmd = getFactory().newDynamicCommand();
        BaseExpression sql = getFactory().newElementSymbol("z");

        sqlCmd.setSql(sql);
        sqlCmd.setAsColumns(symbols);
        sqlCmd.setAsClauseSet(true);

        sqlCmd.setIntoGroup(getFactory().newGroupSymbol("#g"));

        sqlCmd.setUpdatingModelCount(1);

        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(sqlCmd);

        helpStmtTest("execute IMMEDIATE z as a1 string, a2 integer into #g update 1;",
                     "EXECUTE IMMEDIATE z AS a1 string, a2 integer INTO #g UPDATE 1;",
                     cmdStmt);
    }

    @Test
    public void testDynamicCommandStatementWithUsing() throws Exception {
        SetClauseListImpl using = getFactory().newSetClauseList();

        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        SetClauseImpl setClause = getFactory().newSetClause(a, getFactory().newElementSymbol("b"));
        using.addClause(setClause);

        DynamicCommandImpl sqlCmd = getFactory().newDynamicCommand();
        BaseExpression sql = getFactory().newElementSymbol("z");

        sqlCmd.setSql(sql);
        sqlCmd.setUsing(using);

        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(sqlCmd);

        helpStmtTest("execute immediate z using a=b;", "EXECUTE IMMEDIATE z USING a = b;", cmdStmt);
    }

    @Test
    public void testSubquerySetCriteria0() {
        //test wrap up command with subquerySetCriteria
        QueryImpl outer = exampleIn(false);

        helpTest("SELECT a FROM db.g WHERE b IN (SELECT a FROM db.g WHERE a2 = 5)",
                 "SELECT a FROM db.g WHERE b IN (SELECT a FROM db.g WHERE a2 = 5)",
                 outer);
    }

    protected QueryImpl exampleIn(boolean semiJoin) {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        BaseExpression expr = getFactory().newElementSymbol("b");

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ, getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);
        SubquerySetCriteriaImpl subCrit = getFactory().newSubquerySetCriteria(expr, query);
        subCrit.getSubqueryHint().setMergeJoin(semiJoin);
        QueryImpl outer = getFactory().newQuery();
        outer.setSelect(select);
        outer.setFrom(from);
        outer.setCriteria(subCrit);
        return outer;
    }

    @Test
    public void testSubquerySetCriteria1() {

        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        BaseExpression expr = getFactory().newElementSymbol("b");

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ, getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);
        SubquerySetCriteriaImpl subCrit = getFactory().newSubquerySetCriteria(expr, query);
        subCrit.setNegated(true);
        QueryImpl outer = getFactory().newQuery();
        outer.setSelect(select);
        outer.setFrom(from);
        outer.setCriteria(subCrit);

        helpTest("SELECT a FROM db.g WHERE b NOT IN (SELECT a FROM db.g WHERE a2 = 5)",
                 "SELECT a FROM db.g WHERE b NOT IN (SELECT a FROM db.g WHERE a2 = 5)",
                 outer);
    }

    @Test
    public void testSubquerySetCriteriaWithExec() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        BaseExpression expr = getFactory().newElementSymbol("b");

        StoredProcedureImpl exec = getFactory().newStoredProcedure();
        exec.setProcedureName("m.sq1");
        QueryImpl query = getFactory().newQuery(getFactory().newSelect(Arrays.asList(getFactory().newMultipleElementSymbol())),
                               getFactory().newFrom(Arrays.asList(getFactory().newSubqueryFromClause("x", exec))));
        SubquerySetCriteriaImpl subCrit = getFactory().newSubquerySetCriteria(expr, query);

        QueryImpl outer = getFactory().newQuery();
        outer.setSelect(select);
        outer.setFrom(from);
        outer.setCriteria(subCrit);

        helpTest("SELECT a FROM db.g WHERE b IN (EXEC m.sq1())",
                 "SELECT a FROM db.g WHERE b IN (SELECT * FROM (EXEC m.sq1()) AS x)",
                 outer);
    }

    @Test
    public void testSubquerySetCriteriaWithUnion() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("db.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        BaseExpression expr = getFactory().newElementSymbol("b");

        QueryImpl u1 = getFactory().newQuery();
        SelectImpl u1s = getFactory().newSelect();
        u1s.addSymbol(getFactory().newElementSymbol("x1"));
        u1.setSelect(u1s);
        FromImpl u1f = getFactory().newFrom();
        u1f = getFactory().newFrom();
        u1f.addClause(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("db.g2")));
        u1.setFrom(u1f);

        QueryImpl u2 = getFactory().newQuery();
        SelectImpl u2s = getFactory().newSelect();
        u2s.addSymbol(getFactory().newElementSymbol("x2"));
        u2.setSelect(u2s);
        FromImpl u2f = getFactory().newFrom();
        u2f = getFactory().newFrom();
        u2f.addClause(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("db.g3")));
        u2.setFrom(u2f);

        SetQueryImpl union = getFactory().newSetQuery(u1, SetQuery.Operation.UNION, u2, true);

        SubquerySetCriteriaImpl subCrit = getFactory().newSubquerySetCriteria(expr, union);

        QueryImpl outer = getFactory().newQuery();
        outer.setSelect(select);
        outer.setFrom(from);
        outer.setCriteria(subCrit);

        helpTest("SELECT a FROM db.g WHERE b IN (SELECT x1 FROM db.g2 UNION ALL SELECT x2 FROM db.g3)",
                 "SELECT a FROM db.g WHERE b IN (SELECT x1 FROM db.g2 UNION ALL SELECT x2 FROM db.g3)",
                 outer);
    }

    @Test
    public void testVariablesInExec() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setProcedureName("proc1");
        SPParameterImpl parameter = getFactory().newSPParameter(1, getFactory().newElementSymbol("param1"));
        parameter.setParameterType(ParameterInfo.IN);
        storedQuery.addParameter(parameter);
        helpTest("Exec proc1(param1)", "EXEC proc1(param1)", storedQuery);
        helpTest("execute proc1(param1)", "EXEC proc1(param1)", storedQuery);
    }

    @Test
    public void testExecSubquery() {
        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        query.setSelect(select);
        FromImpl from = getFactory().newFrom();
        from.addClause(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("newModel2.Table1")));
        StoredProcedureImpl subquery = getFactory().newStoredProcedure();
        subquery.setProcedureName("NewVirtual.StoredQuery");
        from.addClause(getFactory().newSubqueryFromClause("a", subquery));
        query.setFrom(from);

        helpTest("SELECT * FROM newModel2.Table1, (EXEC NewVirtual.StoredQuery()) AS a",
                 "SELECT * FROM newModel2.Table1, (EXEC NewVirtual.StoredQuery()) AS a",
                 query);
    }

    @Test
    public void testUnicode1() {
        try {
            byte[] data = {(byte)0xd0, (byte)0x9c, (byte)0xd0, (byte)0xbe, (byte)0xd1, (byte)0x81, (byte)0xd0, (byte)0xba,
                (byte)0xd0, (byte)0xb2, (byte)0xd0, (byte)0xb0};

            String string = new String(data, "UTF-8");
            String sql = "SELECT * FROM TestDocument.TestDocument WHERE Subject='" + string + "'";

            QueryImpl query = getFactory().newQuery();
            SelectImpl select = getFactory().newSelect();
            select.addSymbol(getFactory().newMultipleElementSymbol());
            query.setSelect(select);
            FromImpl from = getFactory().newFrom();
            from.addGroup(getFactory().newGroupSymbol("TestDocument.TestDocument"));
            query.setFrom(from);
            CompareCriteriaImpl crit = getFactory().newCompareCriteria(getFactory().newElementSymbol("Subject"), Operator.EQ, getFactory().newConstant(string));
            query.setCriteria(crit);

            helpTest(sql, query.toString(), query);

        } catch (UnsupportedEncodingException e) {
            fail(e.getMessage());
        }
    }

    @Test
    public void testUnicode2() {
        String sql = "SELECT * FROM TestDocument.TestDocument WHERE Subject='\u0041\u005a'";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        query.setSelect(select);
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("TestDocument.TestDocument"));
        query.setFrom(from);
        CompareCriteriaImpl crit = getFactory().newCompareCriteria(getFactory().newElementSymbol("Subject"), Operator.EQ, getFactory().newConstant("AZ"));
        query.setCriteria(crit);

        helpTest(sql, query.toString(), query);
    }

    @Test
    public void testUnicode3() {
        String sql = "SELECT '\u05e0'";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        ConstantImpl c = getFactory().newConstant("\u05e0");
        select.addSymbol(getFactory().wrapExpression(c));
        query.setSelect(select);

        helpTest(sql, query.toString(), query);
    }

    @Test
    public void testUnicode4() {
        String sql = "SELECT \u05e0 FROM g";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl e = getFactory().newElementSymbol("\u05e0");
        select.addSymbol(e);
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));
        query.setSelect(select);
        query.setFrom(from);

        helpTest(sql, query.toString(), query);
    }

    @Test
    public void testEscapedFunction1() {
        String sql = "SELECT * FROM a.thing WHERE e1 = {fn concat('a', 'b')}";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        query.setSelect(select);
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("a.thing"));
        query.setFrom(from);
        FunctionImpl function = getFactory().newFunction("concat", new BaseExpression[] {getFactory().newConstant("a"), getFactory().newConstant("b")});
        CompareCriteriaImpl crit = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, function);
        query.setCriteria(crit);

        helpTest(sql, "SELECT * FROM a.thing WHERE e1 = concat('a', 'b')", query);
    }

    @Test
    public void testEscapedFunction2() {
        String sql = "SELECT * FROM a.thing WHERE e1 = {fn convert(5, string)}";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        query.setSelect(select);
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("a.thing"));
        query.setFrom(from);
        FunctionImpl function = getFactory().newFunction("convert", new BaseExpression[] {getFactory().newConstant(new Integer(5)), getFactory().newConstant("string")});
        CompareCriteriaImpl crit = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, function);
        query.setCriteria(crit);

        helpTest(sql, "SELECT * FROM a.thing WHERE e1 = convert(5, string)", query);
    }

    @Test
    public void testEscapedFunction3() {
        String sql = "SELECT * FROM a.thing WHERE e1 = {fn cast(5 as string)}";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        query.setSelect(select);
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("a.thing"));
        query.setFrom(from);
        FunctionImpl function = getFactory().newFunction("cast", new BaseExpression[] {getFactory().newConstant(new Integer(5)), getFactory().newConstant("string")});
        CompareCriteriaImpl crit = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, function);
        query.setCriteria(crit);

        helpTest(sql, "SELECT * FROM a.thing WHERE e1 = cast(5 AS string)", query);
    }

    @Test
    public void testEscapedFunction4() {
        String sql = "SELECT * FROM a.thing WHERE e1 = {fn concat({fn concat('a', 'b')}, 'c')}";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        query.setSelect(select);
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("a.thing"));
        query.setFrom(from);
        FunctionImpl func1 = getFactory().newFunction("concat", new BaseExpression[] {getFactory().newConstant("a"), getFactory().newConstant("b")});
        FunctionImpl func2 = getFactory().newFunction("concat", new BaseExpression[] {func1, getFactory().newConstant("c")});
        CompareCriteriaImpl crit = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, func2);
        query.setCriteria(crit);

        helpTest(sql, "SELECT * FROM a.thing WHERE e1 = concat(concat('a', 'b'), 'c')", query);
    }

    @Test
    public void testFunctionWithUnderscore() {
        String sql = "SELECT yowza_yowza() FROM a.thing";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        FunctionImpl func1 = getFactory().newFunction("yowza_yowza", new BaseExpression[] {});
        BaseExpression expr = getFactory().wrapExpression(func1);
        select.addSymbol(expr);
        query.setSelect(select);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("a.thing"));
        query.setFrom(from);

        helpTest(sql, "SELECT yowza_yowza() FROM a.thing", query);
    }

    @Test
    public void testManyInnerJoins1() {
        String sql = "SELECT * " + "FROM SQL1.dbo.Customers INNER JOIN SQL1.dbo.Orders "
                     + "ON SQL1.dbo.Customers.CustomerID = SQL1.dbo.Orders.CustomerID " + "INNER JOIN SQL1.dbo.order_details "
                     + "ON SQL1.dbo.Orders.OrderID = SQL1.dbo.order_details.OrderID";

        String sqlExpected = "SELECT * " + "FROM (SQL1.dbo.Customers INNER JOIN SQL1.dbo.Orders "
                             + "ON SQL1.dbo.Customers.CustomerID = SQL1.dbo.Orders.CustomerID) "
                             + "INNER JOIN SQL1.dbo.order_details "
                             + "ON SQL1.dbo.Orders.OrderID = SQL1.dbo.order_details.OrderID";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        query.setSelect(select);
        FromImpl from = getFactory().newFrom();

        GroupSymbolImpl g1 = getFactory().newGroupSymbol("SQL1.dbo.Customers");
        GroupSymbolImpl g2 = getFactory().newGroupSymbol("SQL1.dbo.Orders");
        GroupSymbolImpl g3 = getFactory().newGroupSymbol("SQL1.dbo.order_details");

        ElementSymbolImpl e1 = getFactory().newElementSymbol("SQL1.dbo.Customers.CustomerID");
        ElementSymbolImpl e2 = getFactory().newElementSymbol("SQL1.dbo.Orders.CustomerID");
        ElementSymbolImpl e3 = getFactory().newElementSymbol("SQL1.dbo.Orders.OrderID");
        ElementSymbolImpl e4 = getFactory().newElementSymbol("SQL1.dbo.order_details.OrderID");

        List jcrits1 = new ArrayList();
        jcrits1.add(getFactory().newCompareCriteria(e1, Operator.EQ, e2));
        List jcrits2 = new ArrayList();
        jcrits2.add(getFactory().newCompareCriteria(e3, Operator.EQ, e4));

        JoinPredicateImpl jp1 = getFactory().newJoinPredicate(getFactory().newUnaryFromClause(g1), getFactory().newUnaryFromClause(g2), JoinTypeTypes.JOIN_INNER, jcrits1);
        JoinPredicateImpl jp2 = getFactory().newJoinPredicate(jp1, getFactory().newUnaryFromClause(g3), JoinTypeTypes.JOIN_INNER, jcrits2);

        from.addClause(jp2);
        query.setFrom(from);

        helpTest(sql, sqlExpected, query);
    }

    @Test
    public void testManyInnerJoins2() {
        String sql = "SELECT * " + "FROM A INNER JOIN (B RIGHT OUTER JOIN C ON b1 = c1) " + "ON a1 = b1 " + "INNER JOIN D "
                     + "ON a1 = d1";

        String sqlExpected = "SELECT * " + "FROM (A INNER JOIN (B RIGHT OUTER JOIN C ON b1 = c1) " + "ON a1 = b1) "
                             + "INNER JOIN D " + "ON a1 = d1";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        query.setSelect(select);
        FromImpl from = getFactory().newFrom();

        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("A"));
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("B"));
        UnaryFromClauseImpl g3 = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("C"));
        UnaryFromClauseImpl g4 = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("D"));

        ElementSymbolImpl e1 = getFactory().newElementSymbol("a1");
        ElementSymbolImpl e2 = getFactory().newElementSymbol("b1");
        ElementSymbolImpl e3 = getFactory().newElementSymbol("c1");
        ElementSymbolImpl e4 = getFactory().newElementSymbol("d1");

        List jcrits1 = new ArrayList();
        jcrits1.add(getFactory().newCompareCriteria(e1, Operator.EQ, e2));
        List jcrits2 = new ArrayList();
        jcrits2.add(getFactory().newCompareCriteria(e2, Operator.EQ, e3));
        List jcrits3 = new ArrayList();
        jcrits3.add(getFactory().newCompareCriteria(e1, Operator.EQ, e4));

        JoinPredicateImpl jp1 = getFactory().newJoinPredicate(g2, g3, JoinTypeTypes.JOIN_RIGHT_OUTER, jcrits2);
        JoinPredicateImpl jp2 = getFactory().newJoinPredicate(g1, jp1, JoinTypeTypes.JOIN_INNER, jcrits1);
        JoinPredicateImpl jp3 = getFactory().newJoinPredicate(jp2, g4, JoinTypeTypes.JOIN_INNER, jcrits3);

        from.addClause(jp3);
        query.setFrom(from);

        helpTest(sql, sqlExpected, query);
    }

    @Test
    public void testManyInnerJoins3() {
        String sql = "SELECT * " + "FROM A INNER JOIN " + "(B RIGHT OUTER JOIN C ON b1 = c1 " + "CROSS JOIN D) " + "ON a1 = d1";

        String sqlExpected = "SELECT * " + "FROM A INNER JOIN " + "((B RIGHT OUTER JOIN C ON b1 = c1) " + "CROSS JOIN D) "
                             + "ON a1 = d1";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        query.setSelect(select);
        FromImpl from = getFactory().newFrom();

        UnaryFromClauseImpl g1 = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("A"));
        UnaryFromClauseImpl g2 = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("B"));
        UnaryFromClauseImpl g3 = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("C"));
        UnaryFromClauseImpl g4 = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("D"));

        ElementSymbolImpl e1 = getFactory().newElementSymbol("a1");
        ElementSymbolImpl e2 = getFactory().newElementSymbol("b1");
        ElementSymbolImpl e3 = getFactory().newElementSymbol("c1");
        ElementSymbolImpl e4 = getFactory().newElementSymbol("d1");

        List jcrits1 = new ArrayList();
        jcrits1.add(getFactory().newCompareCriteria(e2, Operator.EQ, e3));
        List jcrits2 = new ArrayList();
        jcrits2.add(getFactory().newCompareCriteria(e1, Operator.EQ, e4));

        JoinPredicateImpl jp1 = getFactory().newJoinPredicate(g2, g3, JoinTypeTypes.JOIN_RIGHT_OUTER, jcrits1);
        JoinPredicateImpl jp2 = getFactory().newJoinPredicate(jp1, g4, JoinTypeTypes.JOIN_CROSS);
        JoinPredicateImpl jp3 = getFactory().newJoinPredicate(g1, jp2, JoinTypeTypes.JOIN_INNER, jcrits2);

        from.addClause(jp3);
        query.setFrom(from);

        helpTest(sql, sqlExpected, query);
    }

    @Test
    public void testLoopStatement() throws Exception {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl c1 = getFactory().newElementSymbol("c1");
        select.addSymbol(c1);
        select.addSymbol(getFactory().newElementSymbol("c2"));

        QueryImpl query = getFactory().newQuery(select, from);

        ElementSymbolImpl x = getFactory().newElementSymbol("x");
        String intType = new String("integer");
        StatementImpl dStmt = getFactory().newDeclareStatement(x, intType);
        c1 = getFactory().newElementSymbol("mycursor.c1");
        StatementImpl assignmentStmt = getFactory().newAssignmentStatement(x, c1);
        BlockImpl block = getFactory().newBlock();
        block.addStatement(dStmt);
        block.addStatement(assignmentStmt);

        String cursor = "mycursor";

        LoopStatementImpl loopStmt = getFactory().newLoopStatement(block, query, cursor);

        helpStmtTest("LOOP ON (SELECT c1, c2 FROM m.g) AS mycursor BEGIN DECLARE integer x; x=mycursor.c1; END",
                     "LOOP ON (SELECT c1, c2 FROM m.g) AS mycursor" + "\n" + "BEGIN" + "\n" + "DECLARE integer x;" + "\n"
                     + "x = mycursor.c1;" + "\n" + "END",
                     loopStmt);
    }

    @Test
    public void testLoopStatementWithOrderBy() throws Exception {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl c1 = getFactory().newElementSymbol("c1");
        select.addSymbol(c1);
        select.addSymbol(getFactory().newElementSymbol("c2"));

        OrderByImpl orderBy = getFactory().newOrderBy();
        orderBy.addVariable(c1);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setOrderBy(orderBy);

        ElementSymbolImpl x = getFactory().newElementSymbol("x");
        String intType = new String("integer");
        StatementImpl dStmt = getFactory().newDeclareStatement(x, intType);
        c1 = getFactory().newElementSymbol("mycursor.c1");
        StatementImpl assignmentStmt = getFactory().newAssignmentStatement(x, c1);
        BlockImpl block = getFactory().newBlock();
        block.addStatement(dStmt);
        block.addStatement(assignmentStmt);

        String cursor = "mycursor";

        LoopStatementImpl loopStmt = getFactory().newLoopStatement(block, query, cursor);

        helpStmtTest("LOOP ON (SELECT c1, c2 FROM m.g ORDER BY c1) AS mycursor BEGIN DECLARE integer x; x=mycursor.c1; END",
                     "LOOP ON (SELECT c1, c2 FROM m.g ORDER BY c1) AS mycursor" + "\n" + "BEGIN" + "\n" + "DECLARE integer x;"
                     + "\n" + "x = mycursor.c1;" + "\n" + "END",
                     loopStmt);
    }

    @Test
    public void testWhileStatement() throws Exception {
        ElementSymbolImpl x = getFactory().newElementSymbol("x");
        FunctionImpl f = getFactory().newFunction("+", new BaseExpression[] {x, getFactory().newConstant(new Integer(1))});
        StatementImpl assignmentStmt = getFactory().newAssignmentStatement(x, f);
        BlockImpl block = getFactory().newBlock();
        block.addStatement(assignmentStmt);
        CriteriaImpl crit = getFactory().newCompareCriteria(x, Operator.LT, getFactory().newConstant(new Integer(100)));
        WhileStatementImpl whileStmt = getFactory().newWhileStatement(crit, block);
        helpStmtTest("WHILE (x < 100) BEGIN x=x+1; END",
                     "WHILE(x < 100)" + "\n" + "BEGIN" + "\n" + "x = (x + 1);" + "\n" + "END",
                     whileStmt);
    }

    @Test
    public void testWhileStatement1() throws Exception {
        ElementSymbolImpl x = getFactory().newElementSymbol("x");
        FunctionImpl f = getFactory().newFunction("+", new BaseExpression[] {x, getFactory().newConstant(new Integer(1))});
        StatementImpl assignmentStmt = getFactory().newAssignmentStatement(x, f);
        BlockImpl block = getFactory().newBlock();
        block.setAtomic(true);
        block.setLabel("1y");
        block.addStatement(assignmentStmt);
        BranchingStatementImpl bs = getFactory().newBranchingStatement(BranchingMode.CONTINUE);
        bs.setLabel("1y");
        block.addStatement(bs);
        CriteriaImpl crit = getFactory().newCompareCriteria(x, Operator.LT, getFactory().newConstant(new Integer(100)));
        WhileStatementImpl whileStmt = getFactory().newWhileStatement(crit, block);
        helpStmtTest("WHILE (x < 100) \"1y\": BEGIN ATOMIC x=x+1; CONTINUE \"1y\"; END", "WHILE(x < 100)" + "\n"
                                                                                         + "\"1y\" : BEGIN ATOMIC" + "\n"
                                                                                         + "x = (x + 1);\nCONTINUE \"1y\";"
                                                                                         + "\n" + "END", whileStmt);
    }

    @Test
    public void testBreakStatement() throws Exception {
        StatementImpl breakStmt = getFactory().newBranchingStatement();
        helpStmtTest("break;", "BREAK;", breakStmt);
    }

    @Test
    public void testContinueStatement() throws Exception {
        BranchingStatementImpl contStmt = getFactory().newBranchingStatement(BranchingMode.CONTINUE);
        helpStmtTest("continue;", "CONTINUE;", contStmt);
    }

    @Test
    public void testContinueStatement1() throws Exception {
        BranchingStatementImpl contStmt = getFactory().newBranchingStatement(BranchingMode.CONTINUE);
        contStmt.setLabel("x");
        helpStmtTest("continue x;", "CONTINUE x;", contStmt);
    }

    @Test
    public void testScalarSubqueryExpressionInSelect() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));
        s2.addSymbol(getFactory().wrapExpression(getFactory().newScalarSubquery(q1)));
        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        helpTest("SELECT e1, (SELECT e1 FROM m.g1) FROM m.g2", "SELECT e1, (SELECT e1 FROM m.g1) FROM m.g2", q2);
    }

    @Test
    public void testScalarSubqueryExpressionInSelect2() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().wrapExpression(getFactory().newScalarSubquery(q1)));
        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        helpTest("SELECT (SELECT e1 FROM m.g1) FROM m.g2", "SELECT (SELECT e1 FROM m.g1) FROM m.g2", q2);
    }

    @Test
    public void testScalarSubqueryExpressionInSelect3() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().wrapExpression(getFactory().newScalarSubquery(q1)));
        s2.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        helpTest("SELECT (SELECT e1 FROM m.g1), e1 FROM m.g2", "SELECT (SELECT e1 FROM m.g1), e1 FROM m.g2", q2);
    }

    @Test
    public void testScalarSubqueryExpressionWithAlias() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));
        s2.addSymbol(getFactory().newAliasSymbol("X", getFactory().wrapExpression(getFactory().newScalarSubquery(q1))));
        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        helpTest("SELECT e1, (SELECT e1 FROM m.g1) as X FROM m.g2", "SELECT e1, (SELECT e1 FROM m.g1) AS X FROM m.g2", q2);
    }

    @Test
    public void testScalarSubqueryExpressionInComplexExpression() throws Exception {
        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));

        s2.addSymbol(getFactory().newAliasSymbol("X", getFactory().wrapExpression(parser.parseExpression("(SELECT e1 FROM m.g1) + 2"))));

        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        helpTest("SELECT e1, ((SELECT e1 FROM m.g1) + 2) as X FROM m.g2",
                 "SELECT e1, ((SELECT e1 FROM m.g1) + 2) AS X FROM m.g2",
                 q2);
    }

    @Test
    public void testScalarSubqueryExpressionInComplexExpression2() throws Exception {
        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));

        s2.addSymbol(getFactory().newAliasSymbol("X", getFactory().wrapExpression(parser.parseExpression("3 + (SELECT e1 FROM m.g1)"))));

        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        helpTest("SELECT e1, (3 + (SELECT e1 FROM m.g1)) as X FROM m.g2",
                 "SELECT e1, (3 + (SELECT e1 FROM m.g1)) AS X FROM m.g2",
                 q2);
    }

    @Test
    public void testScalarSubqueryExpressionInComplexExpression3() throws Exception {
        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));

        s2.addSymbol(getFactory().newAliasSymbol("X", getFactory().wrapExpression(parser.parseExpression("(SELECT e1 FROM m.g1) + (SELECT e3 FROM m.g3)"))));

        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        helpTest("SELECT e1, ((SELECT e1 FROM m.g1) + (SELECT e3 FROM m.g3)) as X FROM m.g2",
                 "SELECT e1, ((SELECT e1 FROM m.g1) + (SELECT e3 FROM m.g3)) AS X FROM m.g2",
                 q2);
    }

    @Test
    public void testScalarSubqueryExpressionInFunction() throws Exception {
        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));

        s2.addSymbol(getFactory().newAliasSymbol("X", getFactory().wrapExpression(parser.parseExpression("length((SELECT e1 FROM m.g1))"))));

        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        QueryImpl q2 = getFactory().newQuery(s2, f2);

        helpTest("SELECT e1, length((SELECT e1 FROM m.g1)) as X FROM m.g2",
                 "SELECT e1, length((SELECT e1 FROM m.g1)) AS X FROM m.g2",
                 q2);
    }

    @Test
    public void testExistsPredicateCriteria() {

        QueryImpl q2 = exampleExists(false);

        helpTest("SELECT e1 FROM m.g2 WHERE Exists (SELECT e1 FROM m.g1)",
                 "SELECT e1 FROM m.g2 WHERE EXISTS (SELECT e1 FROM m.g1)",
                 q2);
    }

    protected QueryImpl exampleExists(boolean semiJoin) {
        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        ExistsCriteriaImpl existsCrit = getFactory().newExistsCriteria(q1);
        existsCrit.getSubqueryHint().setMergeJoin(semiJoin);
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);
        q2.setCriteria(existsCrit);
        return q2;
    }

    @Test
    public void testAnyQuantifierSubqueryComparePredicate() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        CriteriaImpl left = getFactory().newSubqueryCompareCriteria(getFactory().newElementSymbol("e3"), q1, Operator.GE, PredicateQuantifier.ANY);
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);
        q2.setCriteria(left);

        helpTest("SELECT e1 FROM m.g2 WHERE e3 >= ANY (SELECT e1 FROM m.g1)",
                 "SELECT e1 FROM m.g2 WHERE e3 >= ANY (SELECT e1 FROM m.g1)",
                 q2);

    }

    @Test
    public void testSomeQuantifierSubqueryComparePredicate() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        CriteriaImpl left = getFactory().newSubqueryCompareCriteria(getFactory().newElementSymbol("e3"), q1, Operator.GT, PredicateQuantifier.SOME);
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);
        q2.setCriteria(left);

        helpTest("SELECT e1 FROM m.g2 WHERE e3 > some (SELECT e1 FROM m.g1)",
                 "SELECT e1 FROM m.g2 WHERE e3 > SOME (SELECT e1 FROM m.g1)",
                 q2);

    }

    @Test
    public void testAllQuantifierSubqueryComparePredicate() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        CriteriaImpl left = getFactory().newSubqueryCompareCriteria(getFactory().newElementSymbol("e3"), q1, Operator.EQ, PredicateQuantifier.ALL);
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);
        q2.setCriteria(left);

        helpTest("SELECT e1 FROM m.g2 WHERE e3 = all (SELECT e1 FROM m.g1)",
                 "SELECT e1 FROM m.g2 WHERE e3 = ALL (SELECT e1 FROM m.g1)",
                 q2);

    }

    @Test
    public void testScalarSubqueryComparePredicate() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        CriteriaImpl left = getFactory().newCompareCriteria(getFactory().newElementSymbol("e3"), Operator.LT, getFactory().newScalarSubquery(q1));
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);
        q2.setCriteria(left);

        helpTest("SELECT e1 FROM m.g2 WHERE e3 < (SELECT e1 FROM m.g1)",
                 "SELECT e1 FROM m.g2 WHERE e3 < (SELECT e1 FROM m.g1)",
                 q2);

    }

    @Test
    public void testSelectInto() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl c1 = getFactory().newElementSymbol("c1");
        select.addSymbol(c1);
        select.addSymbol(getFactory().newElementSymbol("c2"));

        IntoImpl into = getFactory().newInto(getFactory().newGroupSymbol("#temp"));
        QueryImpl q = getFactory().newQuery();
        q.setSelect(select);
        q.setFrom(from);
        q.setInto(into);
        helpTest("SELECT c1, c2 INTO #temp FROM m.g", "SELECT c1, c2 INTO #temp FROM m.g", q);
    }

    @Test
    public void testAndOrPrecedence_1575() {
        SelectImpl s = getFactory().newSelect();
        s.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl f = getFactory().newFrom();
        f.addGroup(getFactory().newGroupSymbol("m.g1"));
        CompareCriteriaImpl c1 = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, getFactory().newConstant(new Integer(0)));
        CompareCriteriaImpl c2 = getFactory().newCompareCriteria(getFactory().newElementSymbol("e2"), Operator.EQ, getFactory().newConstant(new Integer(1)));
        CompareCriteriaImpl c3 = getFactory().newCompareCriteria(getFactory().newElementSymbol("e3"), Operator.EQ, getFactory().newConstant(new Integer(3)));
        CompoundCriteriaImpl cc1 = getFactory().newCompoundCriteria(CompoundCriteriaImpl.AND, c2, c3);
        CompoundCriteriaImpl cc2 = getFactory().newCompoundCriteria(CompoundCriteriaImpl.OR, c1, cc1);
        QueryImpl q = getFactory().newQuery();
        q.setSelect(s);
        q.setFrom(f);
        q.setCriteria(cc2);

        helpTest("SELECT * FROM m.g1 WHERE e1=0 OR e2=1 AND e3=3",
                 "SELECT * FROM m.g1 WHERE (e1 = 0) OR ((e2 = 1) AND (e3 = 3))",
                 q);
    }

    @Test
    public void testAndOrPrecedence2_1575() {
        SelectImpl s = getFactory().newSelect();
        s.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl f = getFactory().newFrom();
        f.addGroup(getFactory().newGroupSymbol("m.g1"));
        CompareCriteriaImpl c1 = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, getFactory().newConstant(new Integer(0)));
        CompareCriteriaImpl c2 = getFactory().newCompareCriteria(getFactory().newElementSymbol("e2"), Operator.EQ, getFactory().newConstant(new Integer(1)));
        CompareCriteriaImpl c3 = getFactory().newCompareCriteria(getFactory().newElementSymbol("e3"), Operator.EQ, getFactory().newConstant(new Integer(3)));
        CompoundCriteriaImpl cc1 = getFactory().newCompoundCriteria(CompoundCriteriaImpl.AND, c1, c2);
        CompoundCriteriaImpl cc2 = getFactory().newCompoundCriteria(CompoundCriteriaImpl.OR, cc1, c3);
        QueryImpl q = getFactory().newQuery();
        q.setSelect(s);
        q.setFrom(f);
        q.setCriteria(cc2);

        helpTest("SELECT * FROM m.g1 WHERE e1=0 AND e2=1 OR e3=3",
                 "SELECT * FROM m.g1 WHERE ((e1 = 0) AND (e2 = 1)) OR (e3 = 3)",
                 q);
    }

    protected void helpTestCompoundNonJoinCriteria(String sqlPred, CriteriaImpl predCrit) {
        SelectImpl s = getFactory().newSelect();
        s.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl f = getFactory().newFrom();

        CompareCriteriaImpl c1 = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, getFactory().newConstant(new Integer(0)));
        CompoundCriteriaImpl cc1 = getFactory().newCompoundCriteria(CompoundCriteriaImpl.AND, c1, predCrit);
        JoinPredicateImpl jp = getFactory().newJoinPredicate(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g1")),
                                            getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g2")),
                                            JoinTypeTypes.JOIN_INNER,
                                            Collections.singletonList(cc1));
        f.addClause(jp);

        QueryImpl q = getFactory().newQuery();
        q.setSelect(s);
        q.setFrom(f);

        helpTest("SELECT * FROM m.g1 JOIN m.g2 ON e1=0 AND " + sqlPred, "SELECT * FROM m.g1 INNER JOIN m.g2 ON e1 = 0 AND "
                                                                        + sqlPred, q);

    }

    @Test
    public void testCompoundNonJoinCriteriaInFromWithComparisonCriteria() {
        CompareCriteriaImpl c2 = getFactory().newCompareCriteria(getFactory().newElementSymbol("e2"), Operator.EQ, getFactory().newConstant(new Integer(1)));
        helpTestCompoundNonJoinCriteria("e2 = 1", c2);
    }

    @Test
    public void testCompoundNonJoinCriteriaInFromWithIsNull() {
        helpTestCompoundNonJoinCriteria("e2 IS NULL", getFactory().newIsNullCriteria(getFactory().newElementSymbol("e2")));
    }

    @Test
    public void testCompoundNonJoinCriteriaInFromUWithIN() {
        List<BaseExpression> values = new ArrayList<BaseExpression>();
        values.add(getFactory().newConstant(new Integer(0)));
        values.add(getFactory().newConstant(new Integer(1)));
        SetCriteriaImpl crit = getFactory().newSetCriteria(getFactory().newElementSymbol("e2"), values);
        helpTestCompoundNonJoinCriteria("e2 IN (0, 1)", crit);
    }

    @Test
    public void testCompoundNonJoinCriteriaInFromUWithLIKE() {
        MatchCriteriaImpl crit = getFactory().newMatchCriteria(getFactory().newElementSymbol("e2"), getFactory().newConstant("%"));
        helpTestCompoundNonJoinCriteria("e2 LIKE '%'", crit);
    }

    @Test
    public void testCompoundNonJoinCriteria_defect15167_1() throws Exception {
        parser.parseCommand("SELECT A.alert_id, A.primary_entity_name, A.primary_entity_level_code, A.alert_description, A.create_date, A.alert_risk_score, S.scenario_name, A.alert_status_code, A.process_id, A.actual_values_text, S.SCENARIO_CATEGORY_DESC, A.primary_entity_number, A.scenario_id, A.primary_entity_key FROM (FSK_ALERT AS A LEFT OUTER JOIN FSK_SCENARIO AS S ON A.scenario_id = S.scenario_id) INNER JOIN FSC_ACCOUNT_DIM AS C ON A.primary_entity_key = C.ACCOUNT_KEY  AND ((S.current_ind = 'Y') OR (S.current_ind IS NULL)) WHERE (A.primary_entity_level_code = 'ACC') AND (C.ACCOUNT_KEY = 23923) AND (A.logical_delete_ind = 'N')");
    }

    @Test
    public void testCompoundNonJoinCriteria_defect15167_2() throws Exception {
        parser.parseCommand("SELECT A.alert_id, A.primary_entity_name, A.primary_entity_level_code, A.alert_description, A.create_date, A.alert_risk_score, S.scenario_name, A.alert_status_code, A.process_id, A.actual_values_text, S.SCENARIO_CATEGORY_DESC, A.primary_entity_number, A.scenario_id, A.primary_entity_key FROM (FSK_ALERT AS A LEFT OUTER JOIN FSK_SCENARIO AS S ON A.scenario_id = S.scenario_id) INNER JOIN FSC_ACCOUNT_DIM AS C ON A.primary_entity_key = C.ACCOUNT_KEY  AND (S.current_ind = 'Y' OR S.current_ind IS NULL) WHERE (A.primary_entity_level_code = 'ACC') AND (C.ACCOUNT_KEY = 23923) AND (A.logical_delete_ind = 'N')");
    }

    @Test
    public void testCompoundNonJoinCriteria_defect15167_3() throws Exception {
        parser.parseCommand("SELECT A.alert_id, A.primary_entity_name, A.primary_entity_level_code, A.alert_description, A.create_date, A.alert_risk_score, S.scenario_name, A.alert_status_code, A.process_id, A.actual_values_text, S.SCENARIO_CATEGORY_DESC, A.primary_entity_number, A.scenario_id, A.primary_entity_key FROM (FSK_ALERT AS A LEFT OUTER JOIN FSK_SCENARIO AS S ON A.scenario_id = S.scenario_id) INNER JOIN FSC_ACCOUNT_DIM AS C ON (A.primary_entity_key = C.ACCOUNT_KEY AND (S.current_ind = 'Y' OR S.current_ind IS NULL)) WHERE (A.primary_entity_level_code = 'ACC') AND (C.ACCOUNT_KEY = 23923) AND (A.logical_delete_ind = 'N')");
    }

    @Test
    public void testCompoundNonJoinCriteria_defect15167_4() throws Exception {
        parser.parseCommand("SELECT A.alert_id, A.primary_entity_name, A.primary_entity_level_code, A.alert_description, A.create_date, A.alert_risk_score, S.scenario_name, A.alert_status_code, A.process_id, A.actual_values_text, S.SCENARIO_CATEGORY_DESC, A.primary_entity_number, A.scenario_id, A.primary_entity_key FROM (FSK_ALERT AS A LEFT OUTER JOIN FSK_SCENARIO AS S ON A.scenario_id = S.scenario_id) INNER JOIN FSC_ACCOUNT_DIM AS C ON (A.primary_entity_key = C.ACCOUNT_KEY AND S.current_ind = 'Y' OR S.current_ind IS NULL) WHERE (A.primary_entity_level_code = 'ACC') AND (C.ACCOUNT_KEY = 23923) AND (A.logical_delete_ind = 'N')");
    }

    @Test
    public void testFunctionInGroupBy() throws Exception {
        parser.parseCommand("SELECT SUM(s), elem+1 FROM m.g GROUP BY elem+1");
    }

    @Test
    public void testCaseInGroupBy() throws Exception {
        parser.parseCommand("SELECT SUM(elem+1), CASE elem WHEN 0 THEN 1 ELSE 2 END AS c FROM m.g GROUP BY CASE elem WHEN 0 THEN 1 ELSE 2 END");
    }

    @Test
    public void testNationCharString() throws Exception {
        QueryImpl query = (QueryImpl)parser.parseCommand("SELECT N'blah' FROM m.g");
        SelectImpl select = query.getSelect();
        ExpressionSymbolImpl s = (ExpressionSymbolImpl)select.getSymbol(0);
        ConstantImpl c = (ConstantImpl)s.getExpression();
        assertEquals(c, getFactory().newConstant("blah"));
    }

    @Test
    public void testNationCharString2() throws Exception {
        QueryImpl query = (QueryImpl)parser.parseCommand("SELECT DISTINCT TABLE_QUALIFIER, NULL AS TABLE_OWNER, NULL AS TABLE_NAME, NULL AS TABLE_TYPE, NULL AS REMARKS FROM ATIODBCSYSTEM.OA_TABLES  WHERE TABLE_QUALIFIER LIKE N'%'  ESCAPE '\\'  ORDER BY TABLE_QUALIFIER  ");
        MatchCriteriaImpl matchCrit = (MatchCriteriaImpl)query.getCriteria();
        ConstantImpl c = (ConstantImpl)matchCrit.getRightExpression();
        assertEquals(c, getFactory().newConstant("%"));
    }

    @Test
    public void testScalarSubquery() throws Exception {
        parser.parseCommand("SELECT (SELECT 1) FROM x");
    }

    @Test
    public void testElementInDoubleQuotes() throws Exception {
        GroupSymbolImpl g = getFactory().newGroupSymbol("x");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        ElementSymbolImpl e = getFactory().newElementSymbol("foo");
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(e);

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT \"foo\" FROM x", "SELECT foo FROM x", query);
    }

    @Test
    public void testElementInDoubleQuotes_Insert() throws Exception {
        GroupSymbolImpl g = getFactory().newGroupSymbol("x");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        ElementSymbolImpl e = getFactory().newElementSymbol("foo");

        InsertImpl query = getFactory().newInsert();
        query.setGroup(g);
        query.addVariable(e);
        query.addValue(getFactory().newConstant("bar", String.class));

        helpTest("insert into x (\"foo\") values ('bar')", "INSERT INTO x (foo) VALUES ('bar')", query);
    }

    @Test
    public void testElementInDoubleQuotes_Update() throws Exception {
        GroupSymbolImpl g = getFactory().newGroupSymbol("x");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        ElementSymbolImpl e = getFactory().newElementSymbol("foo");
        UpdateImpl query = getFactory().newUpdate();
        query.setGroup(g);
        query.addChange(e, getFactory().newConstant("bar", String.class));

        helpTest("update x set \"foo\"='bar'", "UPDATE x SET foo = 'bar'", query);
    }

    @Test
    public void testElementInDoubleQuotes_delete() throws Exception {
        GroupSymbolImpl g = getFactory().newGroupSymbol("x");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        ElementSymbolImpl e = getFactory().newElementSymbol("foo");
        CompareCriteriaImpl c = getFactory().newCompareCriteria(e, Operator.EQ, getFactory().newConstant("bar", String.class));
        DeleteImpl query = getFactory().newDelete(g, c);

        helpTest("delete from x where \"foo\"='bar'", "DELETE FROM x WHERE foo = 'bar'", query);
    }

    @Test
    public void testAliasInDoubleQuotes() throws Exception {
        GroupSymbolImpl g = getFactory().newGroupSymbol("x");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        AliasSymbolImpl as = getFactory().newAliasSymbol("fooAlias", getFactory().newElementSymbol("fooKey"));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT fooKey AS \"fooAlias\" FROM x", "SELECT fooKey AS fooAlias FROM x", query);
    }

    @Test
    public void testAliasInDoubleQuotesWithQuotedGroup() throws Exception {

        GroupSymbolImpl g = getFactory().newGroupSymbol("x.y.z");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        AliasSymbolImpl as = getFactory().newAliasSymbol("fooAlias", getFactory().newElementSymbol("fooKey"));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        ElementSymbolImpl a = getFactory().newElementSymbol("x.y.z.id");
        ConstantImpl c = getFactory().newConstant(new Integer(10));
        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.EQ, c);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(crit);

        helpTest("SELECT fooKey AS \"fooAlias\" FROM \"x.y\".z where x.\"y.z\".id = 10",
                 "SELECT fooKey AS fooAlias FROM x.y.z WHERE x.y.z.id = 10",
                 query);
    }

    @Test
    public void testSingleQuotedConstant() throws Exception {

        GroupSymbolImpl g = getFactory().newGroupSymbol("x.y.z");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        ConstantImpl as = getFactory().newConstant("fooString");
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(as));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest("SELECT 'fooString' FROM \"x.y.z\"", "SELECT 'fooString' FROM x.y.z", query);
    }

    /** QUERY Tool Format*/
    @Test
    public void testQueryWithQuotes_MSQuery() throws Exception {
        parser.parseCommand("SELECT \"PART_COLOR\", \"PART_ID\", \"PART_NAME\", \"PART_WEIGHT\" FROM \"VirtualParts.base\".\"Parts\"");
    }

    /** MS Access Format**/
    @Test
    public void testQueryWithQuotes_MSAccess() throws Exception {
        parser.parseCommand("SELECT \"PART_COLOR\" ,\"PART_ID\" ,\"PART_NAME\" ,\"PART_WEIGHT\"  FROM \"parts_oracle.DEV_RRAMESH\".\"PARTS\"");
    }

    /** BO Business View Manager**/
    @Test
    public void testQueryWithQuotes_BODesigner() throws Exception {
        parser.parseCommand("SELECT DISTINCT \"PARTS\".\"PART_NAME\" FROM   \"parts_oracle.DEV_RRAMESH\".\"PARTS\" \"PARTS\"");
    }

    /** Crystal Reports **/
    @Test
    public void testQueryWithQuotes_CrystalReports() throws Exception {
        parser.parseCommand("SELECT \"Oracle_PARTS\".\"PART_COLOR\", \"Oracle_PARTS\".\"PART_ID\", \"Oracle_PARTS\".\"PART_NAME\", \"Oracle_PARTS\".\"PART_WEIGHT\", \"SQL_PARTS\".\"PART_COLOR\", \"SQL_PARTS\".\"PART_ID\", \"SQL_PARTS\".\"PART_NAME\", \"SQL_PARTS\".\"PART_WEIGHT\" FROM   \"parts_oracle.DEV_RRAMESH\".\"PARTS\" \"Oracle_PARTS\", \"parts_sqlserver.dv_rreddy.dv_rreddy\".\"PARTS\" \"SQL_PARTS\" WHERE  (\"Oracle_PARTS\".\"PART_ID\"=\"SQL_PARTS\".\"PART_ID\")");
    }

    @Test
    public void testOrderByWithNumbers_InQuotes() throws Exception {
        GroupSymbolImpl g = getFactory().newGroupSymbol("z");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("x"));
        select.addSymbol(getFactory().newElementSymbol("y"));

        OrderByImpl orderby = getFactory().newOrderBy();
        orderby.addVariable(getFactory().newElementSymbol("1"), true);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setOrderBy(orderby);

        helpTest("SELECT x, y from z order by \"1\"", "SELECT x, y FROM z ORDER BY \"1\"", query);
    }

    @Test
    public void testOrderByWithNumbers_AsInt() throws Exception {
        GroupSymbolImpl g = getFactory().newGroupSymbol("z");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("x"));
        select.addSymbol(getFactory().newElementSymbol("y"));

        OrderByImpl orderby = getFactory().newOrderBy();
        orderby.addVariable(getFactory().wrapExpression(getFactory().newConstant(1)), true);

        QueryImpl query = getFactory().newQuery(select, from);
        query.setOrderBy(orderby);

        helpTest("SELECT x, y FROM z order by 1", "SELECT x, y FROM z ORDER BY 1", query);
    }

    @Test
    public void testEmptyAndNullInputsGiveSameErrorMessage() throws Exception {
        String emptyMessage = null;
        try {
            parser.parseCommand("");
            fail("Expected exception for parsing empty string");
        } catch (Exception e) {
            emptyMessage = e.getMessage();
        }

        String nullMessage = null;
        try {
            parser.parseCommand(null);
            fail("Expected exception for parsing null string");
        } catch (Exception e) {
            nullMessage = e.getMessage();
        }

        assertTrue("Expected same message for empty and null cases", emptyMessage.equals(nullMessage));
    }

    @Test
    public void testCase3281NamedVariable() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setDisplayNamedParameters(true);
        storedQuery.setProcedureName("proc1");
        SPParameterImpl parameter = getFactory().newSPParameter(1, getFactory().newConstant("paramValue1"));
        parameter.setName("param1");
        parameter.setParameterType(ParameterInfo.IN);
        storedQuery.addParameter(parameter);
        helpTest("Exec proc1(param1 = 'paramValue1')", "EXEC proc1(param1 => 'paramValue1')", storedQuery);
        helpTest("execute proc1(param1 = 'paramValue1')", "EXEC proc1(param1 => 'paramValue1')", storedQuery);
    }

    @Test
    public void testCase3281NamedVariables() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setDisplayNamedParameters(true);
        storedQuery.setProcedureName("proc1");
        SPParameterImpl param1 = getFactory().newSPParameter(1, getFactory().newConstant("paramValue1"));
        param1.setName("param1");
        param1.setParameterType(ParameterInfo.IN);
        storedQuery.addParameter(param1);
        SPParameterImpl param2 = getFactory().newSPParameter(2, getFactory().newConstant("paramValue2"));
        param2.setName("param2");
        param2.setParameterType(ParameterInfo.IN);
        storedQuery.addParameter(param2);
        helpTest("Exec proc1(param1 = 'paramValue1', param2 = 'paramValue2')",
                 "EXEC proc1(param1 => 'paramValue1', param2 => 'paramValue2')",
                 storedQuery);
        helpTest("execute proc1(param1 = 'paramValue1', param2 = 'paramValue2')",
                 "EXEC proc1(param1 => 'paramValue1', param2 => 'paramValue2')",
                 storedQuery);
    }

    @Test
    public void testCase3281QuotedNamedVariableFails2() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setProcedureName("proc1");
        SPParameterImpl param1 = getFactory().newSPParameter(1, getFactory().newCompareCriteria(getFactory().newConstant("a"), Operator.EQ, getFactory().newConstant("b")));
        param1.setParameterType(ParameterInfo.IN);
        storedQuery.addParameter(param1);
        helpTest("Exec proc1('a' = 'b')", "EXEC proc1(('a' = 'b'))", storedQuery);
    }

    /** Test what happens if the name of a parameter is a reserved word.  It must be quoted (double-ticks). */
    @Test
    public void testCase3281NamedVariablesReservedWords() {
        StoredProcedureImpl storedQuery = getFactory().newStoredProcedure();
        storedQuery.setDisplayNamedParameters(true);
        storedQuery.setProcedureName("proc1");
        SPParameterImpl param1 = getFactory().newSPParameter(1, getFactory().newConstant("paramValue1"));
        param1.setName("in"); //<---RESERVED WORD
        param1.setParameterType(ParameterInfo.IN);
        storedQuery.addParameter(param1);
        SPParameterImpl param2 = getFactory().newSPParameter(2, getFactory().newConstant("paramValue2"));
        param2.setName("in2");
        param2.setParameterType(ParameterInfo.IN);
        storedQuery.addParameter(param2);
        helpTest("Exec proc1(\"in\" = 'paramValue1', in2 = 'paramValue2')",
                 "EXEC proc1(\"in\" => 'paramValue1', in2 => 'paramValue2')",
                 storedQuery);
        helpTest("execute proc1(\"in\" = 'paramValue1', in2 = 'paramValue2')",
                 "EXEC proc1(\"in\" => 'paramValue1', in2 => 'paramValue2')",
                 storedQuery);
    }

    @Test
    public void testExceptionMessageWithLocation() {
        try {
            parser.parseCommand("SELECT FROM");
            fail("Should not be able to parse invalid sql");
        } catch (Exception e) {
            // Exception should be thrown
        }
    }

    @Test
    public void testEscapedOuterJoin() {
        String sql = "SELECT * FROM {oj A LEFT OUTER JOIN B ON (A.x=B.x)}";
        String expected = "SELECT * FROM A LEFT OUTER JOIN B ON A.x = B.x";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        query.setSelect(select);
        select.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl from = getFactory().newFrom();
        query.setFrom(from);
        CriteriaImpl compareCriteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("A.x"), Operator.EQ, getFactory().newElementSymbol("B.x"));
        FromClauseImpl f1 = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("A"));
        FromClauseImpl f2 = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("B"));
        JoinPredicateImpl jp = getFactory().newJoinPredicate(f1,
                                            f2,
                                            JoinTypeTypes.JOIN_LEFT_OUTER,
                                            Arrays.asList(new CriteriaImpl[] {compareCriteria}));
        from.addClause(jp);

        helpTest(sql, expected, query);
    }

    @Test
    public void testNameSpacedFunctionName() {
        String sql = "select a.x()";

        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        FunctionImpl func1 = getFactory().newFunction("a.x", new BaseExpression[] {});
        BaseExpression expr = getFactory().wrapExpression(func1);
        select.addSymbol(expr);
        query.setSelect(select);

        helpTest(sql, "SELECT a.x()", query);
    }

    @Test
    public void testUnionJoin() {
        String sql = "select * from pm1.g1 union join pm1.g2 where g1.e1 = 1";
        String expected = "SELECT * FROM pm1.g1 UNION JOIN pm1.g2 WHERE g1.e1 = 1";

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());

        FromImpl from = getFactory().newFrom();
        from.addClause(getFactory().newJoinPredicate(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("pm1.g1")),
                                        getFactory().newUnaryFromClause(getFactory().newGroupSymbol("pm1.g2")),
                                        JoinTypeTypes.JOIN_UNION));

        CriteriaImpl crit = getFactory().newCompareCriteria(getFactory().newElementSymbol("g1.e1"), Operator.EQ, getFactory().newConstant(new Integer(1)));

        QueryImpl command = getFactory().newQuery(select, from);
        command.setCriteria(crit);
        helpTest(sql, expected, command);
    }

    @Test
    public void testCommandWithSemicolon() throws Exception {
        helpTest("select * from pm1.g1;", "SELECT * FROM pm1.g1", parser.parseCommand("select * from pm1.g1"));
    }

    @Test
    public void testLOBTypes() throws Exception {
        FunctionImpl convert = getFactory().newFunction("convert", new BaseExpression[] {getFactory().newConstant(null), getFactory().newConstant("blob")});
        FunctionImpl convert1 = getFactory().newFunction("convert", new BaseExpression[] {getFactory().newConstant(null), getFactory().newConstant("clob")});
        FunctionImpl convert2 = getFactory().newFunction("convert", new BaseExpression[] {getFactory().newConstant(null), getFactory().newConstant("xml")});
        SelectImpl select = getFactory().newSelect(Arrays.asList(getFactory().wrapExpression(convert, "expr"), getFactory().wrapExpression(convert1, "expr1"), getFactory().wrapExpression(convert2, "expr2")));
        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);

        helpTest("select convert(null, blob), convert(null, clob), convert(null, xml)",
                 "SELECT convert(null, blob), convert(null, clob), convert(null, xml)",
                 query);
    }

    @Test
    public void testInsertWithoutColumns() {
        InsertImpl insert = getFactory().newInsert();
        insert.setGroup(getFactory().newGroupSymbol("m.g"));
        insert.addValue(getFactory().newConstant("a"));
        insert.addValue(getFactory().newConstant("b"));
        helpTest("INSERT INTO m.g VALUES ('a', 'b')", "INSERT INTO m.g VALUES ('a', 'b')", insert);
    }

    @Test
    public void testXmlElement() throws Exception {
        XMLElementImpl f = getFactory().newXMLElement("table", Arrays.asList((BaseExpression)getFactory().newConstant("x")));
        helpTestExpression("xmlelement(name \"table\", 'x')", "XMLELEMENT(NAME \"table\", 'x')", f);
    }

    @Test
    public void testXmlElement1() throws Exception {
        XMLElementImpl f = getFactory().newXMLElement("table", Arrays.asList((BaseExpression)getFactory().newConstant("x")));
        helpTestExpression("xmlelement(\"table\", 'x')", "XMLELEMENT(NAME \"table\", 'x')", f);
    }

    @Test
    public void testXmlElementWithAttributes() throws Exception {
        XMLElementImpl f = getFactory().newXMLElement("y", new ArrayList<BaseExpression>());
        f.setAttributes(getFactory().newXMLAttributes(Arrays.asList(getFactory().newDerivedColumn("val", getFactory().newConstant("a")))));
        helpTestExpression("xmlelement(y, xmlattributes('a' as val))", "XMLELEMENT(NAME y, XMLATTRIBUTES('a' AS val))", f);
    }

    @Test
    public void testXmlForest() throws Exception {
        XMLForestImpl f = getFactory().newXMLForest(Arrays.asList(getFactory().newDerivedColumn("table", getFactory().newElementSymbol("a"))));
        helpTestExpression("xmlforest(a as \"table\")", "XMLFOREST(a AS \"table\")", f);
    }

    @Test
    public void testXmlPi() throws Exception {
        FunctionImpl f = getFactory().newFunction("xmlpi", new BaseExpression[] {getFactory().newConstant("a"), getFactory().newElementSymbol("val")});
        helpTestExpression("xmlpi(NAME a, val)", "xmlpi(NAME a, val)", f);
    }

    @Test
    public void testXmlNamespaces() throws Exception {
        XMLForestImpl f = getFactory().newXMLForest(Arrays.asList(getFactory().newDerivedColumn("table", getFactory().newElementSymbol("a"))));
        f.setNamespaces(getFactory().newXMLNamespaces(Arrays.asList(new NamespaceItem(), new NamespaceItem("http://foo", "x"))));
        helpTestExpression("xmlforest(xmlnamespaces(no default, 'http://foo' as x), a as \"table\")",
                           "XMLFOREST(XMLNAMESPACES(NO DEFAULT, 'http://foo' AS x), a AS \"table\")",
                           f);
    }

    @Test
    public void testXmlAggWithOrderBy() throws Exception {
        String sql = "SELECT xmlAgg(1 order by e2)";
        BaseAggregateSymbol as = getFactory().newAggregateSymbol(Reserved.XMLAGG, false, getFactory().newConstant(1));
        as.setOrderBy(getFactory().newOrderBy(Arrays.asList(getFactory().newElementSymbol("e2"))));
        QueryImpl query = getFactory().newQuery();
        query.setSelect(getFactory().newSelect(Arrays.asList(as)));
        helpTest(sql, "SELECT XMLAGG(1 ORDER BY e2)", query);
    }

    @Test
    public void testTextAggWithOrderBy() throws Exception {
        List<DerivedColumnImpl> expressions = new ArrayList<DerivedColumnImpl>();
        expressions.add(getFactory().newDerivedColumn("col1", getFactory().newElementSymbol("e1")));
        expressions.add(getFactory().newDerivedColumn("col2", getFactory().newElementSymbol("e2")));

        TextLineImpl tf = getFactory().newTextLine();
        tf.setExpressions(expressions);
        tf.setDelimiter(new Character(','));
        tf.setIncludeHeader(true);

        BaseAggregateSymbol as = getFactory().newAggregateSymbol(NonReserved.TEXTAGG, false, tf);
        as.setOrderBy(getFactory().newOrderBy(Arrays.asList(getFactory().newElementSymbol("e2"))));

        QueryImpl query = getFactory().newQuery();
        query.setSelect(getFactory().newSelect(Arrays.asList(as)));

        String sql = "SELECT TextAgg(FOR e1 as col1, e2 as col2 delimiter ',' header order by e2)";
        helpTest(sql, "SELECT TEXTAGG(FOR e1 AS col1, e2 AS col2 DELIMITER ',' HEADER ORDER BY e2)", query);
    }

    @Test
    public void testArrayAggWithOrderBy() throws Exception {
        String sql = "SELECT array_agg(1 order by e2)";
        BaseAggregateSymbol as = getFactory().newAggregateSymbol(Reserved.ARRAY_AGG, false, getFactory().newConstant(1));
        as.setOrderBy(getFactory().newOrderBy(Arrays.asList(getFactory().newElementSymbol("e2"))));
        QueryImpl query = getFactory().newQuery();
        query.setSelect(getFactory().newSelect(Arrays.asList(as)));
        helpTest(sql, "SELECT ARRAY_AGG(1 ORDER BY e2)", query);
    }

    @Test
    public void testArrayAggWithIndexing() throws Exception {
        String sql = "SELECT (array_agg(1))[1]";
        BaseAggregateSymbol as = getFactory().newAggregateSymbol(Reserved.ARRAY_AGG, false, getFactory().newConstant(1));
        BaseExpression expr = getFactory().wrapExpression(getFactory().newFunction("array_get", new BaseExpression[] {as, getFactory().newConstant(1)}));
        QueryImpl query = getFactory().newQuery();
        query.setSelect(getFactory().newSelect(Arrays.asList(expr)));
        helpTest(sql, "SELECT array_get(ARRAY_AGG(1), 1)", query);
    }

    @Test
    public void testNestedTable() throws Exception {
        String sql = "SELECT * from TABLE(exec foo()) as x";
        QueryImpl query = getFactory().newQuery();
        query.setSelect(getFactory().newSelect(Arrays.asList(getFactory().newMultipleElementSymbol())));
        StoredProcedureImpl sp = getFactory().newStoredProcedure();
        sp.setProcedureName("foo");
        SubqueryFromClauseImpl sfc = getFactory().newSubqueryFromClause("x", sp);
        sfc.setTable(true);
        query.setFrom(getFactory().newFrom(Arrays.asList(sfc)));
        helpTest(sql, "SELECT * FROM TABLE(EXEC foo()) AS x", query);
    }

    @Test
    public void testTextTable() throws Exception {
        String sql = "SELECT * from texttable(file columns x string WIDTH 1, y date width 10 skip 10) as x";
        QueryImpl query = getFactory().newQuery();
        query.setSelect(getFactory().newSelect(Arrays.asList(getFactory().newMultipleElementSymbol())));
        TextTableImpl tt = getFactory().newTextTable();
        tt.setFile(getFactory().newElementSymbol("file"));
        List<TextColumnImpl> columns = new ArrayList<TextColumnImpl>();
        columns.add(getFactory().newTextColumn("x", "string", 1));
        columns.add(getFactory().newTextColumn("y", "date", 10));
        tt.setColumns(columns);
        tt.setSkip(10);
        tt.setName("x");
        query.setFrom(getFactory().newFrom(Arrays.asList(tt)));
        helpTest(sql, "SELECT * FROM TEXTTABLE(file COLUMNS x string WIDTH 1, y date WIDTH 10 SKIP 10) AS x", query);

        sql = "SELECT * from texttable(file columns x string, y date delimiter ',' escape '\"' header skip 10) as x";
        tt.setDelimiter(',');
        tt.setQuote('"');
        tt.setEscape(true);
        tt.setHeader(1);
        for (TextColumnImpl textColumn : columns) {
            textColumn.setWidth(null);
        }
        helpTest(sql,
                 "SELECT * FROM TEXTTABLE(file COLUMNS x string, y date DELIMITER ',' ESCAPE '\"' HEADER SKIP 10) AS x",
                 query);
    }

    @Test
    public void testXMLTable() throws Exception {
        String sql = "SELECT * from xmltable(xmlnamespaces(no default), '/' columns x for ordinality, y date default {d'2000-01-01'} path '@date') as x";
        QueryImpl query = getFactory().newQuery();
        query.setSelect(getFactory().newSelect(Arrays.asList(getFactory().newMultipleElementSymbol())));
        XMLTableImpl xt = getFactory().newXMLTable();
        xt.setName("x");
        xt.setNamespaces(getFactory().newXMLNamespaces(Arrays.asList(new NamespaceItem())));
        xt.setXquery("/");
        List<XMLColumnImpl> columns = new ArrayList<XMLColumnImpl>();
        columns.add(getFactory().newXMLColumn("x", true));
        XMLColumnImpl c2 = getFactory().newXMLColumn("y", false);
        c2.setType("date");
        c2.setPath("@date");
        c2.setDefaultExpression(getFactory().newConstant(Date.valueOf("2000-01-01")));
        columns.add(c2);
        xt.setColumns(columns);
        query.setFrom(getFactory().newFrom(Arrays.asList(xt)));
        helpTest(sql,
                 "SELECT * FROM XMLTABLE(XMLNAMESPACES(NO DEFAULT), '/' COLUMNS x FOR ORDINALITY, y date DEFAULT {d'2000-01-01'} PATH '@date') AS x",
                 query);
    }

    @Test
    public void testXmlSerialize() throws Exception {
        XMLSerializeImpl f = getFactory().newXMLSerialize();
        f.setDocument(true);
        f.setExpression(getFactory().newElementSymbol("x"));
        f.setTypeString("CLOB");
        helpTestExpression("xmlserialize(document x as CLOB)", "XMLSERIALIZE(DOCUMENT x AS CLOB)", f);
    }

    @Test
    public void testXmlQuery() throws Exception {
        XMLQueryImpl f = getFactory().newXMLQuery();
        f.setXquery("/x");
        f.setEmptyOnEmpty(false);
        DerivedColumnImpl derivedColumn = getFactory().newDerivedColumn(null, getFactory().newElementSymbol("foo"));
        derivedColumn.setPropagateName(false);
        f.setPassing(Arrays.asList(derivedColumn));
        helpTestExpression("xmlquery('/x' passing foo null on empty)", "XMLQUERY('/x' PASSING foo NULL ON EMPTY)", f);
    }

    @Test
    public void testXmlParse() throws Exception {
        XMLParseImpl f = getFactory().newXMLParse();
        f.setDocument(true);
        f.setExpression(getFactory().newElementSymbol("x"));
        f.setWellFormed(true);
        helpTestExpression("xmlparse(document x wellformed)", "XMLPARSE(DOCUMENT x WELLFORMED)", f);
    }

    @Test
    public void testXmlSerialize1() throws Exception {
        XMLSerializeImpl f = getFactory().newXMLSerialize();
        f.setExpression(getFactory().newElementSymbol("x"));
        f.setTypeString("CLOB");
        helpTestExpression("xmlserialize(x as CLOB)", "XMLSERIALIZE(x AS CLOB)", f);
    }

    @Test
    public void testExpressionCriteria() throws Exception {
        SearchedCaseExpressionImpl sce = getFactory().newSearchedCaseExpression(Arrays.asList(getFactory().newExpressionCriteria(getFactory().newElementSymbol("x"))),
                                                               Arrays.asList(getFactory().newElementSymbol("y")));
        helpTestExpression("case when x then y end", "CASE WHEN x THEN y END", sce);
    }

    @Test
    public void testExpressionCriteria1() throws Exception {
        SearchedCaseExpressionImpl sce = getFactory().newSearchedCaseExpression(Arrays.asList(getFactory().newNotCriteria(getFactory().newExpressionCriteria(getFactory().newElementSymbol("x")))),
                                                               Arrays.asList(getFactory().newElementSymbol("y")));
        helpTestExpression("case when not x then y end", "CASE WHEN NOT (x) THEN y END", sce);
    }

    @Test
    public void testWithClause() throws Exception {
        QueryImpl query = getOrderByQuery(null);
        query.setWith(Arrays.asList(getFactory().newWithQueryCommand(getFactory().newGroupSymbol("x"), getOrderByQuery(null))));
        helpTest("WITH x AS (SELECT a FROM db.g WHERE b = aString) SELECT a FROM db.g WHERE b = aString",
                 "WITH x AS (SELECT a FROM db.g WHERE b = aString) SELECT a FROM db.g WHERE b = aString",
                 query);
    }

    @Test
    public void testExplicitTable() throws Exception {
        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        query.setSelect(select);
        select.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl from = getFactory().newFrom(Arrays.asList(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("X"))));
        query.setFrom(from);
        helpTest("TABLE X", "SELECT * FROM X", query);
    }

    @Test
    public void testArrayTable() throws Exception {
        String sql = "SELECT * from arraytable(null columns x string, y date) as x";
        QueryImpl query = getFactory().newQuery();
        query.setSelect(getFactory().newSelect(Arrays.asList(getFactory().newMultipleElementSymbol())));
        ArrayTableImpl tt = getFactory().newArrayTable();
        tt.setArrayValue(getFactory().newConstant(null, DefaultDataTypeManager.DefaultDataTypes.NULL.getTypeClass()));
        List<ProjectedColumnImpl> columns = new ArrayList<ProjectedColumnImpl>();
        columns.add(getFactory().newProjectedColumn("x", "string"));
        columns.add(getFactory().newProjectedColumn("y", "date"));
        tt.setColumns(columns);
        tt.setName("x");
        query.setFrom(getFactory().newFrom(Arrays.asList(tt)));
        helpTest(sql, "SELECT * FROM ARRAYTABLE(null COLUMNS x string, y date) AS x", query);
    }

    @Test
    public void testPositionalReference() throws Exception {
        String sql = "select $1";
        QueryImpl query = getFactory().newQuery();
        query.setSelect(getFactory().newSelect(Arrays.asList(getFactory().wrapExpression(getFactory().newReference(0)))));
        helpTest(sql, "SELECT ?", query);
    }

    @Test
    public void testNonReserved() throws Exception {
        String sql = "select count";
        QueryImpl query = getFactory().newQuery();
        query.setSelect(getFactory().newSelect(Arrays.asList(getFactory().newElementSymbol("count"))));
        helpTest(sql, "SELECT count", query);
    }

    @Test
    public void testAggFilter() throws Exception {
        String sql = "select count(*) filter (where x = 1) from g";
        QueryImpl query = getFactory().newQuery();
        BaseAggregateSymbol aggregateSymbol = getFactory().newAggregateSymbol(AggregateSymbol.Type.COUNT.name(), false, null);
        aggregateSymbol.setCondition(getFactory().newCompareCriteria(getFactory().newElementSymbol("x"), Operator.EQ, getFactory().newConstant(1)));
        query.setSelect(getFactory().newSelect(Arrays.asList(aggregateSymbol)));
        query.setFrom(getFactory().newFrom(Arrays.asList(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("g")))));
        helpTest(sql, "SELECT COUNT(*) FILTER(WHERE x = 1) FROM g", query);
    }

    @Test
    public void testWindowFunction() throws Exception {
        String sql = "select row_number() over (partition by x order by y) from g";
        QueryImpl query = getFactory().newQuery();
        BaseWindowFunction wf = getFactory().newWindowFunction("win_row_number");
        wf.setFunction(getFactory().newAggregateSymbol("ROW_NUMBER", false, null));
        WindowSpecificationImpl ws = getFactory().newWindowSpecification();
        ws.setPartition(new ArrayList<BaseExpression>(Arrays.asList(getFactory().newElementSymbol("x"))));
        ws.setOrderBy(getFactory().newOrderBy(Arrays.asList(getFactory().newElementSymbol("y"))));
        wf.setWindowSpecification(ws);
        query.setSelect(getFactory().newSelect(Arrays.asList(wf)));
        query.setFrom(getFactory().newFrom(Arrays.asList(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("g")))));
        helpTest(sql, "SELECT ROW_NUMBER() OVER (PARTITION BY x ORDER BY y) FROM g", query);
    }

    @Test
    public void testSubString() {
        String sql = "select substring(RTRIM(MED.BATDAT), 4, 4) from FCC.MEDMAS as MED";

        UnaryFromClauseImpl ufc = getFactory().newUnaryFromClause("MED", "FCC.MEDMAS");
        List<FromClauseImpl> clauses = new ArrayList<FromClauseImpl>();
        clauses.add(ufc);
        FromImpl from = getFactory().newFrom(clauses);

        ElementSymbolImpl e = getFactory().newElementSymbol("MED.BATDAT");
        BaseExpression[] f2args = new BaseExpression[] { e };
        FunctionImpl f2 = getFactory().newFunction("RTRIM", f2args);
        ConstantImpl c1 = getFactory().newConstant(4);
        ConstantImpl c2 = getFactory().newConstant(4);
        BaseExpression[] f1args = new BaseExpression[] { f2, c1, c2 };
        FunctionImpl f1 = getFactory().newFunction("substring", f1args);

        ExpressionSymbolImpl es = getFactory().newNode(ASTNodes.EXPRESSION_SYMBOL);
        
        /*
         * Annoying that I have to be so explicit but any other way is far more
         * loquacious and not really worth it.
         */
        String name;
        if (parser.getTeiidParser().getVersion().isSevenInstance())
            name = "expr";
        else
            name = "expr1";

        es.setName(name);
        es.setExpression(f1);

        SelectImpl select = getFactory().newSelect(Arrays.asList(es));
        QueryImpl query = getFactory().newQuery(select, from);
        
        helpTest(sql, "SELECT substring(RTRIM(MED.BATDAT), 4, 4) FROM FCC.MEDMAS AS MED", query);
    }

    /** SELECT * FROM g1 inner join g2 */
    @Test
    public void testInvalidInnerJoin() {
        helpException("SELECT * FROM g1 inner join g2");
    }

    /** SELECT a FROM m.g GROUP BY a, b HAVING COUNT(b) AS x = 5*/
    @Test
    public void testFailNestedAggregateInHaving() {
        helpException("SELECT a FROM m.g GROUP BY a, b HAVING COUNT(b) AS x = 5");
    }

    /** SELECT a FROM m.g GROUP BY a, b AS x */
    @Test
    public void testFailAliasInHaving() {
        helpException("SELECT a FROM m.g GROUP BY a, b AS x");
    }

    @Test
    public void testExceptionLength() {
        String sql = "SELECT * FROM Customer where Customer.Name = (select lastname from CUSTOMER where acctid = 9";
        helpException(sql);
    }

    /** SELECT {d'bad'} FROM m.g1 */
    @Test
    public void testDateLiteralFail() {
        helpException("SELECT {d'bad'} FROM m.g1");
    }

    /** SELECT {t 'xyz'} FROM m.g1 */
    @Test
    public void testTimeLiteralFail() {
        helpException("SELECT {t 'xyz'} FROM m.g1");
    }

    /** SELECT a AS or FROM g */
    @Test
    public void testAliasInSelectUsingKeywordFails() {
        helpException("SELECT a AS or FROM g");
    }

    /** SELECT or.a FROM g AS or */
    @Test
    public void testAliasInFromUsingKeywordFails() {
        helpException("SELECT or.a FROM g AS or");
    }

    /** FROM g WHERE a = 'aString' */
    @Test
    public void testFailsNoSelectClause() {
        helpException("FROM g WHERE a = 'aString'");
    }

    /** SELECT a WHERE a = 'aString' */
    @Test
    public void testFailsNoFromClause() {
        helpException("SELECT a WHERE a = 'aString'");
    }

    /** SELECT xx.yy%.a from xx.yy */
    @Test
    public void testFailsWildcardInSelect() {
        helpException("SELECT xx.yy%.a from xx.yy");
    }

    /** SELECT a from g ORDER BY b DSC*/
    @Test
    public void testFailsDSCMisspelled() {
        helpException("SELECT a from g ORDER BY b DSC");
    }

    /** SELECT a, b FROM (SELECT c FROM m.g2) */
    @Test
    public void testSubqueryInvalid() {
        helpException("SELECT a, b FROM (SELECT c FROM m.g2)");
    }

    //as clause should use short names
    @Test
    public void testDynamicCommandStatement2() {
        helpException("create virtual procedure begin execute string z as variables.a1 string, a2 integer into #g; end");
    }

    //using clause should use short names
    @Test
    public void testDynamicCommandStatement3() {
        helpException("create virtual procedure begin execute string z as a1 string, a2 integer into #g using variables.x=variables.y; end");
    }

    //into clause requires as clause
    @Test
    public void testDynamicCommandStatement4() {
        helpException("create virtual procedure begin execute string z into #g using x=variables.y; end");
    }

    @Test
    public void testBadScalarSubqueryExpression() {
        helpException("SELECT e1, length(SELECT e1 FROM m.g1) as X FROM m.g2");
    }

    @Test
    public void testAliasInSingleQuotes() throws Exception {

        GroupSymbolImpl g = getFactory().newGroupSymbol("x.y.z");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        AliasSymbolImpl as = getFactory().newAliasSymbol("fooAlias", getFactory().newElementSymbol("fooKey"));
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(as);

        helpException("SELECT fooKey 'fooAlias' FROM x.\"y\".z");
    }

    @Test
    public void testOrderByWithNumbers_AsNegitiveInt() {
        helpException("SELECT x, y FROM z order by -1");
    }

    @Test
    public void testBadAlias() {
        String sql = "select a as a.x from foo";

        helpException(sql);
    }

    @Test
    public void testUnionJoin1() {
        String sql = "select * from pm1.g1 union all join pm1.g2 where g1.e1 = 1";

        helpException(sql);
    }

    @Test
    public void testTextTableColumns() throws Exception {
        helpException("SELECT * from texttable(foo x string)");
    }

    @Test
    public void testTrim1() {
        helpException("select trim('xy' from e1) from pm1.g1");
    }
}
