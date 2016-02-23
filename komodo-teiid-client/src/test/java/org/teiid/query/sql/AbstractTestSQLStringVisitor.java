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
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import org.komodo.spi.query.JoinTypeTypes;
import org.komodo.spi.query.sql.lang.OrderBy;
import org.komodo.spi.query.sql.lang.SPParameter;
import org.komodo.spi.query.sql.lang.SetQuery.Operation;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.resolver.TCQueryResolver;
import org.teiid.query.sql.lang.BaseLanguageObject;
import org.teiid.query.sql.lang.BetweenCriteriaImpl;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.CompoundCriteriaImpl;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.CriteriaOperator.Operator;
import org.teiid.query.sql.lang.DeleteImpl;
import org.teiid.query.sql.lang.DynamicCommandImpl;
import org.teiid.query.sql.lang.ExistsCriteriaImpl;
import org.teiid.query.sql.lang.FromImpl;
import org.teiid.query.sql.lang.GroupByImpl;
import org.teiid.query.sql.lang.InsertImpl;
import org.teiid.query.sql.lang.IsNullCriteriaImpl;
import org.teiid.query.sql.lang.JoinPredicateImpl;
import org.teiid.query.sql.lang.LimitImpl;
import org.teiid.query.sql.lang.MatchCriteriaImpl;
import org.teiid.query.sql.lang.NotCriteriaImpl;
import org.teiid.query.sql.lang.OptionImpl;
import org.teiid.query.sql.lang.OrderByImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.SetCriteriaImpl;
import org.teiid.query.sql.lang.SetQueryImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl;
import org.teiid.query.sql.lang.SubqueryCompareCriteriaImpl.PredicateQuantifier;
import org.teiid.query.sql.lang.SubqueryFromClauseImpl;
import org.teiid.query.sql.lang.SubquerySetCriteriaImpl;
import org.teiid.query.sql.lang.UnaryFromClauseImpl;
import org.teiid.query.sql.lang.UpdateImpl;
import org.teiid.query.sql.proc.AssignmentStatementImpl;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.DeclareStatementImpl;
import org.teiid.query.sql.symbol.AliasSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.CaseExpressionImpl;
import org.teiid.query.sql.symbol.ConstantImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;
import org.teiid.query.sql.symbol.ScalarSubqueryImpl;
import org.teiid.query.sql.symbol.SearchedCaseExpressionImpl;
import org.teiid.query.sql.visitor.SQLStringVisitorImpl;
import org.teiid.query.unittest.RealMetadataFactory;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public abstract class AbstractTestSQLStringVisitor extends AbstractTest<CommandImpl> {

    private RealMetadataFactory metadataFactory;

    /**
     * @param teiidVersion 
     */
    public AbstractTestSQLStringVisitor(TeiidVersion teiidVersion) {
        super(teiidVersion);
        metadataFactory = new RealMetadataFactory(teiidVersion);
    }

    // ################################## TEST HELPERS ################################ 

    protected void helpTest(BaseLanguageObject obj, String expectedStr) {
        String actualStr = SQLStringVisitorImpl.getSQLString(obj);
        assertEquals("Expected and actual strings don't match: ", expectedStr, actualStr);
    }

    protected BaseExpression helpTestExpression(String sql, String expected) throws Exception {
        BaseExpression expr = parser.parseExpression(sql);
        helpTest(expr, expected);
        return expr;
    }

    protected List getWhenExpressions(int expressions) {
        return getWhenExpressions(expressions, -1, false);
    }
    
    protected List getWhenExpressions(int expressions, int nullIndex, boolean includeNull) {
        ArrayList list = new ArrayList();
        for (int i = 0; i < expressions; i++) {
            if(includeNull && i == nullIndex) {
                list.add(getFactory().newConstant(null) );
            }else {
                list.add(getFactory().newConstant(String.valueOf((char)('a' + i))));
            }
        }
        return list;
    }
    
    protected List getThenExpressions(int expressions) {
        ArrayList list = new ArrayList();
        for (int i = 0; i < expressions; i++) {
            list.add(getFactory().newConstant(new Integer(i)));
        }
        return list;
    }

    protected CaseExpressionImpl example(int whens) {
        ElementSymbolImpl x = getFactory().newElementSymbol("x");
        CaseExpressionImpl caseExpr = getFactory().newCaseExpression(x, getWhenExpressions(whens), getThenExpressions(whens));
        caseExpr.setElseExpression(getFactory().newConstant(new Integer(9999)));
        return caseExpr;
    }
    
    protected CaseExpressionImpl caseExample(int whens, int nullIndex, boolean includeNull) {
        assertTrue("Null Index must be less than the number of When expressions", nullIndex < whens);
        ElementSymbolImpl x = getFactory().newElementSymbol("x");
        CaseExpressionImpl caseExpr = getFactory().newCaseExpression(x, getWhenExpressions(whens, nullIndex, includeNull), getThenExpressions(whens));
        caseExpr.setElseExpression(getFactory().newConstant(new Integer(9999)));
        return caseExpr;
    }

    protected List getWhenCriteria(int criteria) {
        ArrayList list = new ArrayList();
        ElementSymbolImpl x = getFactory().newElementSymbol("x");
        for (int i = 0; i < criteria; i++) {
            list.add(getFactory().newCompareCriteria(x, Operator.EQ, getFactory().newConstant(new Integer(i))));
        }
        return list;
    }

    protected List getAlphaWhenCriteria(int criteria) {
        ArrayList list = new ArrayList();
        ElementSymbolImpl x = getFactory().newElementSymbol("x");
        for (int i = 0; i < criteria; i++) {
            list.add(getFactory().newCompareCriteria(x, Operator.EQ, getFactory().newConstant(String.valueOf((char)('a' + i)))));
        }
        return list;
    }

    protected SearchedCaseExpressionImpl searchedCaseExample(int whens) {
        SearchedCaseExpressionImpl caseExpr = getFactory().newSearchedCaseExpression(getWhenCriteria(whens), getThenExpressions(whens));
        caseExpr.setElseExpression(getFactory().newConstant(new Integer(9999)));
        return caseExpr;
    }

    protected SearchedCaseExpressionImpl searchedCaseExample2(int whens) {
        SearchedCaseExpressionImpl caseExpr = getFactory().newSearchedCaseExpression(getAlphaWhenCriteria(whens), getThenExpressions(whens));
        caseExpr.setElseExpression(getFactory().newConstant(new Integer(9999)));
        return caseExpr;
    }

    // ################################## ACTUAL TESTS ################################

    @Test
    public void testNull() {
        String sql = SQLStringVisitorImpl.getSQLString(null);
        assertEquals("Incorrect string for null object", SQLStringVisitorImpl.UNDEFINED, sql);
    }

    @Test
    public void testBetweenCriteria1() {
        BetweenCriteriaImpl bc = getFactory().newBetweenCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                             getFactory().newConstant(new Integer(1000)),
                                                             getFactory().newConstant(new Integer(2000)));
        helpTest(bc, "m.g.c1 BETWEEN 1000 AND 2000");
    }

    @Test
    public void testBetweenCriteria2() {
        BetweenCriteriaImpl bc = getFactory().newBetweenCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                             getFactory().newConstant(new Integer(1000)),
                                                             getFactory().newConstant(new Integer(2000)));
        bc.setNegated(true);
        helpTest(bc, "m.g.c1 NOT BETWEEN 1000 AND 2000");
    }

    @Test
    public void testCompareCriteria1() {
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                             Operator.EQ,
                                                             getFactory().newConstant("abc"));

        helpTest(cc, "m.g.c1 = 'abc'");
    }

    @Test
    public void testCompareCriteria2() {
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                             Operator.NE,
                                                             getFactory().newConstant("abc"));

        helpTest(cc, "m.g.c1 <> 'abc'");
    }

    @Test
    public void testCompareCriteria3() {
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                             Operator.GT,
                                                             getFactory().newConstant("abc"));

        helpTest(cc, "m.g.c1 > 'abc'");
    }

    @Test
    public void testCompareCriteria4() {
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                             Operator.GE,
                                                             getFactory().newConstant("abc"));

        helpTest(cc, "m.g.c1 >= 'abc'");
    }

    @Test
    public void testCompareCriteria5() {
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                             Operator.LT,
                                                             getFactory().newConstant("abc"));

        helpTest(cc, "m.g.c1 < 'abc'");
    }

    @Test
    public void testCompareCriteria6() {
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                             Operator.LE,
                                                             getFactory().newConstant("abc"));

        helpTest(cc, "m.g.c1 <= 'abc'");
    }

    @Test
    public void testCompareCriteria7() {
        CompareCriteriaImpl cc = getFactory().newCompareCriteria((BaseExpression)null, Operator.EQ, (BaseExpression)null);

        helpTest(cc, "<undefined> = <undefined>");
    }

    @Test
    public void testCompoundCriteria1() {
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                             Operator.EQ,
                                                             getFactory().newConstant("abc"));
        List<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(cc);
        CompoundCriteriaImpl comp = getFactory().newNode(ASTNodes.COMPOUND_CRITERIA);
        comp.setOperator(CompoundCriteriaImpl.AND);
        comp.setCriteria(crits);

        helpTest(comp, "m.g.c1 = 'abc'");
    }

    @Test
    public void testCompoundCriteria2() {
        CompareCriteriaImpl cc1 = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                              Operator.EQ,
                                                              getFactory().newConstant("abc"));
        CompareCriteriaImpl cc2 = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c2"),
                                                              Operator.EQ,
                                                              getFactory().newConstant("abc"));
        List<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(cc1);
        crits.add(cc2);
        CompoundCriteriaImpl comp = getFactory().newNode(ASTNodes.COMPOUND_CRITERIA);
        comp.setOperator(CompoundCriteriaImpl.AND);
        comp.setCriteria(crits);

        helpTest(comp, "(m.g.c1 = 'abc') AND (m.g.c2 = 'abc')");
    }

    @Test
    public void testCompoundCriteria3() {
        CompareCriteriaImpl cc1 = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                              Operator.EQ,
                                                              getFactory().newConstant("abc"));
        CompareCriteriaImpl cc2 = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c2"),
                                                              Operator.EQ,
                                                              getFactory().newConstant("abc"));
        CompareCriteriaImpl cc3 = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c3"),
                                                              Operator.EQ,
                                                              getFactory().newConstant("abc"));
        List<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(cc1);
        crits.add(cc2);
        crits.add(cc3);
        CompoundCriteriaImpl comp = getFactory().newNode(ASTNodes.COMPOUND_CRITERIA);
        comp.setOperator(CompoundCriteriaImpl.OR);
        comp.setCriteria(crits);

        helpTest(comp, "(m.g.c1 = 'abc') OR (m.g.c2 = 'abc') OR (m.g.c3 = 'abc')");
    }

    @Test
    public void testCompoundCriteria4() {
        CompareCriteriaImpl cc1 = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                              Operator.EQ,
                                                              getFactory().newConstant("abc"));
        List<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(cc1);
        crits.add(null);
        CompoundCriteriaImpl comp = getFactory().newNode(ASTNodes.COMPOUND_CRITERIA);
        comp.setOperator(CompoundCriteriaImpl.OR);
        comp.setCriteria(crits);

        helpTest(comp, "(m.g.c1 = 'abc') OR (<undefined>)");
    }

    @Test
    public void testCompoundCriteria5() {
        CompareCriteriaImpl cc1 = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                              Operator.EQ,
                                                              getFactory().newConstant("abc"));
        List<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(null);
        crits.add(cc1);
        CompoundCriteriaImpl comp = getFactory().newNode(ASTNodes.COMPOUND_CRITERIA);
        comp.setOperator(CompoundCriteriaImpl.OR);
        comp.setCriteria(crits);

        helpTest(comp, "(<undefined>) OR (m.g.c1 = 'abc')");
    }

    @Test
    public void testCompoundCriteria6() {
        CompareCriteriaImpl cc1 = getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                              Operator.EQ,
                                                              getFactory().newConstant("abc"));
        List<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(cc1);
        crits.add(null);
        CompoundCriteriaImpl comp = getFactory().newNode(ASTNodes.COMPOUND_CRITERIA);
        comp.setOperator(CompoundCriteriaImpl.OR);
        comp.setCriteria(crits);

        helpTest(comp, "(m.g.c1 = 'abc') OR (<undefined>)");
    }

    @Test
    public void testDelete1() {
        DeleteImpl delete = getFactory().newNode(ASTNodes.DELETE);
        delete.setGroup(getFactory().newGroupSymbol("m.g"));

        helpTest(delete, "DELETE FROM m.g");
    }

    @Test
    public void testDelete2() {
        DeleteImpl delete = getFactory().newNode(ASTNodes.DELETE);
        delete.setGroup(getFactory().newGroupSymbol("m.g"));
        delete.setCriteria(getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g.c1"),
                                                           Operator.EQ,
                                                           getFactory().newConstant("abc")));

        helpTest(delete, "DELETE FROM m.g WHERE m.g.c1 = 'abc'");
    }

    @Test
    public void testFrom1() {
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("m.g1"));
        from.addGroup(getFactory().newGroupSymbol("m.g2"));

        helpTest(from, "FROM m.g1, m.g2");
    }

    @Test
    public void testFrom2() {
        FromImpl from = getFactory().newFrom();
        from.addClause(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g1")));
        from.addClause(getFactory().newJoinPredicate(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g2")),
                                                     getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g3")),
                                                     JoinTypeTypes.JOIN_CROSS));

        helpTest(from, "FROM m.g1, m.g2 CROSS JOIN m.g3");
    }

    @Test
    public void testGroupBy1() {
        GroupByImpl gb = getFactory().newGroupBy();
        gb.addSymbol(getFactory().newElementSymbol("m.g.e1"));

        helpTest(gb, "GROUP BY m.g.e1");
    }

    @Test
    public void testGroupBy2() {
        GroupByImpl gb = getFactory().newGroupBy();
        gb.addSymbol(getFactory().newElementSymbol("m.g.e1"));
        gb.addSymbol(getFactory().newElementSymbol("m.g.e2"));
        gb.addSymbol(getFactory().newElementSymbol("m.g.e3"));

        helpTest(gb, "GROUP BY m.g.e1, m.g.e2, m.g.e3");
    }

    @Test
    public void testInsert1() {
        InsertImpl insert = getFactory().newInsert();
        insert.setGroup(getFactory().newGroupSymbol("m.g1"));

        List<ElementSymbolImpl> vars = new ArrayList<ElementSymbolImpl>();
        vars.add(getFactory().newElementSymbol("e1"));
        vars.add(getFactory().newElementSymbol("e2"));
        insert.setVariables(vars);
        List<ConstantImpl> values = new ArrayList<ConstantImpl>();
        values.add(getFactory().newConstant(new Integer(5)));
        values.add(getFactory().newConstant("abc"));
        insert.setValues(values);

        helpTest(insert, "INSERT INTO m.g1 (e1, e2) VALUES (5, 'abc')");
    }

    @Test
    public void testIsNullCriteria1() {
        IsNullCriteriaImpl inc = getFactory().newNode(ASTNodes.IS_NULL_CRITERIA);
        inc.setExpression(getFactory().newConstant("abc"));

        helpTest(inc, "'abc' IS NULL");
    }

    @Test
    public void testIsNullCriteria2() {
        IsNullCriteriaImpl inc = getFactory().newNode(ASTNodes.IS_NULL_CRITERIA);
        inc.setExpression(getFactory().newElementSymbol("m.g.e1"));

        helpTest(inc, "m.g.e1 IS NULL");
    }

    @Test
    public void testIsNullCriteria3() {
        IsNullCriteriaImpl inc = getFactory().newNode(ASTNodes.IS_NULL_CRITERIA);
        helpTest(inc, "<undefined> IS NULL");
    }

    @Test
    public void testIsNullCriteria4() {
        IsNullCriteriaImpl inc = getFactory().newNode(ASTNodes.IS_NULL_CRITERIA);
        inc.setExpression(getFactory().newElementSymbol("m.g.e1"));
        inc.setNegated(true);
        helpTest(inc, "m.g.e1 IS NOT NULL");
    }

    @Test
    public void testJoinPredicate1() {
        JoinPredicateImpl jp = getFactory().newJoinPredicate(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g2")),
                                                         getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g3")),
                                                         JoinTypeTypes.JOIN_CROSS);

        helpTest(jp, "m.g2 CROSS JOIN m.g3");
    }

    @Test
    public void testOptionalJoinPredicate1() {
        JoinPredicateImpl jp = getFactory().newJoinPredicate(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g2")),
                                                         getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g3")),
                                                         JoinTypeTypes.JOIN_CROSS);
        jp.setOptional(true);
        helpTest(jp, "/*+ optional */ (m.g2 CROSS JOIN m.g3)");
    }

    @Test
    public void testJoinPredicate2() {
        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g2.e1"), Operator.EQ, getFactory().newElementSymbol("m.g3.e1")));
        JoinPredicateImpl jp = getFactory().newJoinPredicate(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g2")),
                                                         getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g3")),
                                                         JoinTypeTypes.JOIN_INNER,
                                                         crits);

        helpTest(jp, "m.g2 INNER JOIN m.g3 ON m.g2.e1 = m.g3.e1");
    }

    @Test
    public void testJoinPredicate3() {
        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g2.e1"), Operator.EQ, getFactory().newElementSymbol("m.g3.e1")));
        crits.add(getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g2.e2"), Operator.EQ, getFactory().newElementSymbol("m.g3.e2")));
        JoinPredicateImpl jp = getFactory().newJoinPredicate(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g2")),
                                                         getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g3")),
                                                         JoinTypeTypes.JOIN_INNER,
                                                         crits);

        helpTest(jp, "m.g2 INNER JOIN m.g3 ON m.g2.e1 = m.g3.e1 AND m.g2.e2 = m.g3.e2");
    }

    @Test
    public void testJoinPredicate4() {
        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g2.e1"), Operator.EQ, getFactory().newElementSymbol("m.g3.e1")));
        JoinPredicateImpl jp = getFactory().newJoinPredicate(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g2")),
                                                         getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g3")),
                                                         JoinTypeTypes.JOIN_INNER,
                                                         crits);

        JoinPredicateImpl jp2 = getFactory().newJoinPredicate(jp,
                                                          getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g1")),
                                                          JoinTypeTypes.JOIN_CROSS);

        helpTest(jp2, "(m.g2 INNER JOIN m.g3 ON m.g2.e1 = m.g3.e1) CROSS JOIN m.g1");
    }

    @Test
    public void testJoinPredicate5() {
        ArrayList<CriteriaImpl> crits = new ArrayList<CriteriaImpl>();
        crits.add(getFactory().newNotCriteria(getFactory().newCompareCriteria(getFactory().newElementSymbol("m.g2.e1"), Operator.EQ, getFactory().newElementSymbol("m.g3.e1"))));
        JoinPredicateImpl jp = getFactory().newJoinPredicate(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g2")),
                                                         getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g3")),
                                                         JoinTypeTypes.JOIN_INNER,
                                                         crits);

        helpTest(jp, "m.g2 INNER JOIN m.g3 ON NOT (m.g2.e1 = m.g3.e1)");
    }

    @Test
    public void testJoinType1() {
        helpTest(getFactory().newJoinType(JoinTypeTypes.JOIN_CROSS), "CROSS JOIN");
    }

    @Test
    public void testJoinType2() {
        helpTest(getFactory().newJoinType(JoinTypeTypes.JOIN_INNER), "INNER JOIN");
    }

    @Test
    public void testJoinType3() {
        helpTest(getFactory().newJoinType(JoinTypeTypes.JOIN_RIGHT_OUTER), "RIGHT OUTER JOIN");
    }

    @Test
    public void testJoinType4() {
        helpTest(getFactory().newJoinType(JoinTypeTypes.JOIN_LEFT_OUTER), "LEFT OUTER JOIN");
    }

    @Test
    public void testJoinType5() {
        helpTest(getFactory().newJoinType(JoinTypeTypes.JOIN_FULL_OUTER), "FULL OUTER JOIN");
    }

    @Test
    public void testMatchCriteria1() {
        MatchCriteriaImpl mc = getFactory().newNode(ASTNodes.MATCH_CRITERIA);
        mc.setLeftExpression(getFactory().newElementSymbol("m.g.e1"));
        mc.setRightExpression(getFactory().newConstant("abc"));

        helpTest(mc, "m.g.e1 LIKE 'abc'");
    }

    @Test
    public void testMatchCriteria2() {
        MatchCriteriaImpl mc = getFactory().newNode(ASTNodes.MATCH_CRITERIA);
        mc.setLeftExpression(getFactory().newElementSymbol("m.g.e1"));
        mc.setRightExpression(getFactory().newConstant("%"));
        mc.setEscapeChar('#');

        helpTest(mc, "m.g.e1 LIKE '%' ESCAPE '#'");
    }

    @Test
    public void testMatchCriteria3() {
        MatchCriteriaImpl mc = getFactory().newNode(ASTNodes.MATCH_CRITERIA);
        mc.setLeftExpression(getFactory().newElementSymbol("m.g.e1"));
        mc.setRightExpression(getFactory().newConstant("abc"));
        mc.setNegated(true);
        helpTest(mc, "m.g.e1 NOT LIKE 'abc'");
    }

    @Test
    public void testNotCriteria1() {
        NotCriteriaImpl not = getFactory().newNotCriteria(getFactory().newIsNullCriteria(getFactory().newElementSymbol("m.g.e1")));
        helpTest(not, "NOT (m.g.e1 IS NULL)");
    }

    @Test
    public void testNotCriteria2() {
        NotCriteriaImpl not = getFactory().newNode(ASTNodes.NOT_CRITERIA);
        helpTest(not, "NOT (<undefined>)");
    }

    @Test
    public void testOption1() {
        OptionImpl option = getFactory().newNode(ASTNodes.OPTION);
        helpTest(option, "OPTION");
    }

    @Test
    public void testOption5() {
        OptionImpl option = getFactory().newNode(ASTNodes.OPTION);
        option.addDependentGroup("abc");
        option.addDependentGroup("def");
        option.addDependentGroup("xyz");
        helpTest(option, "OPTION MAKEDEP abc, def, xyz");
    }

    @Test
    public void testOption6() {
        OptionImpl option = getFactory().newNode(ASTNodes.OPTION);
        option.addDependentGroup("abc");
        option.addDependentGroup("def");
        option.addDependentGroup("xyz");
        helpTest(option, "OPTION MAKEDEP abc, def, xyz");
    }

    @Test
    public void testOption8() {
        OptionImpl option = getFactory().newNode(ASTNodes.OPTION);
        option.addNoCacheGroup("abc");
        option.addNoCacheGroup("def");
        option.addNoCacheGroup("xyz");
        helpTest(option, "OPTION NOCACHE abc, def, xyz");
    }

    //  related to defect 14423
    @Test
    public void testOption9() {
        OptionImpl option = getFactory().newNode(ASTNodes.OPTION);
        option.setNoCache(true);
        helpTest(option, "OPTION NOCACHE");
    }

    @Test
    public void testOrderBy1() {
        OrderByImpl ob = getFactory().newOrderBy();
        ob.addVariable(getFactory().newElementSymbol("e1"));

        helpTest(ob, "ORDER BY e1");
    }

    @Test
    public void testOrderBy2() {
        OrderByImpl ob = getFactory().newOrderBy();
        ob.addVariable(getFactory().newElementSymbol("e1"));
        ob.addVariable(getFactory().newAliasSymbol("x", getFactory().newElementSymbol("e2")));

        helpTest(ob, "ORDER BY e1, x");
    }

    @Test
    public void testOrderBy3() {
        OrderByImpl ob = getFactory().newOrderBy();
        ob.addVariable(getFactory().newElementSymbol("e1"), OrderBy.DESC);
        ob.addVariable(getFactory().newElementSymbol("x"), OrderBy.DESC);

        helpTest(ob, "ORDER BY e1 DESC, x DESC");
    }

    @Test
    public void testQuery1() {
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("m.g"));
        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);
        query.setFrom(from);

        helpTest(query, "SELECT * FROM m.g");
    }

    @Test
    public void testQuery2() {
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("m.g"));
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, getFactory().newConstant(new Integer(5)));
        GroupByImpl groupBy = getFactory().newGroupBy();
        groupBy.addSymbol(getFactory().newElementSymbol("e1"));
        CompareCriteriaImpl having = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.GT, getFactory().newConstant(new Integer(0)));
        OrderByImpl orderBy = getFactory().newOrderBy();
        orderBy.addVariable(getFactory().newElementSymbol("e1"));

        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);
        query.setFrom(from);
        query.setCriteria(cc);
        query.setGroupBy(groupBy);
        query.setHaving(having);
        query.setOrderBy(orderBy);

        helpTest(query, "SELECT * FROM m.g WHERE e1 = 5 GROUP BY e1 HAVING e1 > 0 ORDER BY e1");
    }

    @Test
    public void testQuery3() {
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("m.g"));
        GroupByImpl groupBy = getFactory().newGroupBy();
        groupBy.addSymbol(getFactory().newElementSymbol("e1"));
        CompareCriteriaImpl having = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.GT, getFactory().newConstant(new Integer(0)));
        OrderByImpl orderBy = getFactory().newOrderBy();
        orderBy.addVariable(getFactory().newElementSymbol("e1"));

        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);
        query.setFrom(from);
        query.setGroupBy(groupBy);
        query.setHaving(having);
        query.setOrderBy(orderBy);

        helpTest(query, "SELECT * FROM m.g GROUP BY e1 HAVING e1 > 0 ORDER BY e1");
    }

    @Test
    public void testQuery4() {
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("m.g"));
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, getFactory().newConstant(new Integer(5)));
        CompareCriteriaImpl having = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.GT, getFactory().newConstant(new Integer(0)));
        OrderByImpl orderBy = getFactory().newOrderBy();
        orderBy.addVariable(getFactory().newElementSymbol("e1"));

        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);
        query.setFrom(from);
        query.setCriteria(cc);
        query.setHaving(having);
        query.setOrderBy(orderBy);

        helpTest(query, "SELECT * FROM m.g WHERE e1 = 5 HAVING e1 > 0 ORDER BY e1");
    }

    @Test
    public void testQuery5() {
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("m.g"));
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, getFactory().newConstant(new Integer(5)));
        GroupByImpl groupBy = getFactory().newGroupBy();
        groupBy.addSymbol(getFactory().newElementSymbol("e1"));
        OrderByImpl orderBy = getFactory().newOrderBy();
        orderBy.addVariable(getFactory().newElementSymbol("e1"));

        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);
        query.setFrom(from);
        query.setCriteria(cc);
        query.setGroupBy(groupBy);
        query.setOrderBy(orderBy);

        helpTest(query, "SELECT * FROM m.g WHERE e1 = 5 GROUP BY e1 ORDER BY e1");
    }

    @Test
    public void testQuery6() {
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("m.g"));
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, getFactory().newConstant(new Integer(5)));
        GroupByImpl groupBy = getFactory().newGroupBy();
        groupBy.addSymbol(getFactory().newElementSymbol("e1"));
        CompareCriteriaImpl having = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.GT, getFactory().newConstant(new Integer(0)));

        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);
        query.setFrom(from);
        query.setCriteria(cc);
        query.setGroupBy(groupBy);
        query.setHaving(having);

        helpTest(query, "SELECT * FROM m.g WHERE e1 = 5 GROUP BY e1 HAVING e1 > 0");
    }

    @Test
    public void testQuery7() {
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newMultipleElementSymbol());
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("m.g"));
        CompareCriteriaImpl cc = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.EQ, getFactory().newConstant(new Integer(5)));
        GroupByImpl groupBy = getFactory().newGroupBy();
        groupBy.addSymbol(getFactory().newElementSymbol("e1"));
        CompareCriteriaImpl having = getFactory().newCompareCriteria(getFactory().newElementSymbol("e1"), Operator.GT, getFactory().newConstant(new Integer(0)));
        OrderByImpl orderBy = getFactory().newOrderBy();
        orderBy.addVariable(getFactory().newElementSymbol("e1"));

        QueryImpl query = getFactory().newQuery();
        query.setSelect(select);
        query.setFrom(from);
        query.setCriteria(cc);
        query.setGroupBy(groupBy);
        query.setHaving(having);
        query.setOrderBy(orderBy);

        helpTest(query, "SELECT * FROM m.g WHERE e1 = 5 GROUP BY e1 HAVING e1 > 0 ORDER BY e1");
    }

    @Test
    public void testSelect1() {
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("e1"));

        helpTest(select, " e1");
    }

    @Test
    public void testSelect2() {
        SelectImpl select = getFactory().newSelect();
        select.setDistinct(true);
        select.addSymbol(getFactory().newElementSymbol("e1"));

        helpTest(select, " DISTINCT e1");
    }

    @Test
    public void testSelect3() {
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("e1"));
        select.addSymbol(getFactory().newElementSymbol("e2"));

        helpTest(select, " e1, e2");
    }

    @Test
    public void testSetCriteria1() {
        SetCriteriaImpl sc = getFactory().newNode(ASTNodes.SET_CRITERIA);
        sc.setExpression(getFactory().newElementSymbol("e1"));
        sc.setValues(new ArrayList<BaseExpression>());

        helpTest(sc, "e1 IN ()");
    }

    @Test
    public void testSetCriteria2() {
        SetCriteriaImpl sc = getFactory().newNode(ASTNodes.SET_CRITERIA);
        sc.setExpression(getFactory().newElementSymbol("e1"));
        ArrayList<BaseExpression> values = new ArrayList<BaseExpression>();
        values.add(getFactory().newElementSymbol("e2"));
        values.add(getFactory().newConstant("abc"));
        sc.setValues(values);

        helpTest(sc, "e1 IN (e2, 'abc')");
    }

    @Test
    public void testSetCriteria3() {
        SetCriteriaImpl sc = getFactory().newNode(ASTNodes.SET_CRITERIA);
        sc.setExpression(getFactory().newElementSymbol("e1"));
        ArrayList<BaseExpression> values = new ArrayList<BaseExpression>();
        values.add(null);
        values.add(getFactory().newConstant("b"));
        sc.setValues(values);

        helpTest(sc, "e1 IN (<undefined>, 'b')");
    }

    @Test
    public void testSetCriteria4() {
        SetCriteriaImpl sc = getFactory().newNode(ASTNodes.SET_CRITERIA);
        sc.setExpression(getFactory().newElementSymbol("e1"));
        ArrayList<BaseExpression> values = new ArrayList<BaseExpression>();
        values.add(getFactory().newElementSymbol("e2"));
        values.add(getFactory().newConstant("abc"));
        sc.setValues(values);
        sc.setNegated(true);
        helpTest(sc, "e1 NOT IN (e2, 'abc')");
    }

    @Test
    public void testSetQuery1() {
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
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        SetQueryImpl sq = getFactory().newSetQuery(q1, Operation.UNION, q2, false);

        helpTest(sq, "SELECT e1 FROM m.g1 UNION SELECT e1 FROM m.g2");
    }

    @Test
    public void testSetQuery2() {
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
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        SetQueryImpl sq = getFactory().newSetQuery(q1, Operation.UNION, q2, true);

        helpTest(sq, "SELECT e1 FROM m.g1 UNION ALL SELECT e1 FROM m.g2");
    }

    @Test
    public void testSetQuery3() {
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
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        OrderByImpl orderBy = getFactory().newOrderBy();
        orderBy.addVariable(getFactory().newElementSymbol("e1"));

        SetQueryImpl sq = getFactory().newSetQuery(q1, Operation.UNION, q2, false);
        sq.setOrderBy(orderBy);

        helpTest(sq, "SELECT e1 FROM m.g1 UNION SELECT e1 FROM m.g2 ORDER BY e1");
    }

    @Test
    public void testSetQuery4() {
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
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        SetQueryImpl sq = getFactory().newSetQuery(q1, Operation.UNION, q2, false);

        helpTest(sq, "SELECT e1 FROM m.g1 UNION SELECT e1 FROM m.g2");
    }

    @Test
    public void testSetQuery5() {
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
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);

        SelectImpl s3 = getFactory().newSelect();
        s3.addSymbol(getFactory().newElementSymbol("e3"));
        FromImpl f3 = getFactory().newFrom();
        f3.addGroup(getFactory().newGroupSymbol("m.g3"));
        QueryImpl q3 = getFactory().newQuery();
        q3.setSelect(s3);
        q3.setFrom(f3);

        SetQueryImpl sq = getFactory().newSetQuery(q1, Operation.UNION, q2, false);

        SetQueryImpl sq2 = getFactory().newSetQuery(q3, Operation.UNION, sq, true);

        helpTest(sq2, "SELECT e3 FROM m.g3 UNION ALL (SELECT e1 FROM m.g1 UNION SELECT e1 FROM m.g2)");
    }

    @Test
    public void testSubqueryFromClause1() {
        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SubqueryFromClauseImpl sfc = getFactory().newSubqueryFromClause("temp", q1);
        helpTest(sfc, "(SELECT e1 FROM m.g1) AS temp");
    }

    @Test
    public void testOptionalSubqueryFromClause1() {
        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SubqueryFromClauseImpl sfc = getFactory().newSubqueryFromClause("temp", q1);
        sfc.setOptional(true);
        helpTest(sfc, "/*+ optional */ (SELECT e1 FROM m.g1) AS temp");
    }

    @Test
    public void testSubquerySetCriteria1() {
        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        ElementSymbolImpl expr = getFactory().newElementSymbol("e2");

        SubquerySetCriteriaImpl ssc = getFactory().newSubquerySetCriteria(expr, q1);
        helpTest(ssc, "e2 IN (SELECT e1 FROM m.g1)");
    }

    @Test
    public void testSubquerySetCriteria2() {
        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        ElementSymbolImpl expr = getFactory().newElementSymbol("e2");

        SubquerySetCriteriaImpl ssc = getFactory().newSubquerySetCriteria(expr, q1);
        ssc.setNegated(true);
        helpTest(ssc, "e2 NOT IN (SELECT e1 FROM m.g1)");
    }

    @Test
    public void testUnaryFromClause() {
        helpTest(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g1")), "m.g1");
    }

    @Test
    public void testOptionalUnaryFromClause() {
        UnaryFromClauseImpl unaryFromClause = getFactory().newUnaryFromClause(getFactory().newGroupSymbol("m.g1"));//$NON-NLS-1$
        unaryFromClause.setOptional(true);
        helpTest(unaryFromClause, "/*+ optional */ m.g1"); 
    }

    @Test
    public void testUpdate1() {
        UpdateImpl update = getFactory().newUpdate();
        update.setGroup(getFactory().newGroupSymbol("m.g1"));
        update.addChange(getFactory().newElementSymbol("e1"), getFactory().newConstant("abc"));

        helpTest(update, "UPDATE m.g1 SET e1 = 'abc'");
    }

    @Test
    public void testUpdate2() {
        UpdateImpl update = getFactory().newUpdate();
        update.setGroup(getFactory().newGroupSymbol("m.g1"));
        update.addChange(getFactory().newElementSymbol("e1"), getFactory().newConstant("abc"));
        update.addChange(getFactory().newElementSymbol("e2"), getFactory().newConstant("xyz"));

        helpTest(update, "UPDATE m.g1 SET e1 = 'abc', e2 = 'xyz'");
    }

    @Test
    public void testUpdate3() {
        UpdateImpl update = getFactory().newUpdate();
        update.setGroup(getFactory().newGroupSymbol("m.g1"));
        update.addChange(getFactory().newElementSymbol("e1"), getFactory().newConstant("abc"));
        update.setCriteria(getFactory().newCompareCriteria(getFactory().newElementSymbol("e2"),
                                                           Operator.EQ,
                                                           getFactory().newConstant("abc")));

        helpTest(update, "UPDATE m.g1 SET e1 = 'abc' WHERE e2 = 'abc'");
    }

    @Test
    public void testAliasSymbol1() {
        AliasSymbolImpl as = getFactory().newAliasSymbol("x", getFactory().newElementSymbol("y"));
        helpTest(as, "y AS x");
    }

    // Test alias symbol with reserved word 
    @Test
    public void testAliasSymbol2() {
        AliasSymbolImpl as = getFactory().newAliasSymbol("select", getFactory().newElementSymbol("y"));
        helpTest(as, "y AS \"select\"");
    }

    @Test
    public void testAllSymbol() {
        helpTest(getFactory().newMultipleElementSymbol(), "*");
    }

    @Test
    public void testAllInGroupSymbol() {
        helpTest(getFactory().newMultipleElementSymbol("m.g"), "m.g.*");
    }

    @Test
    public void testConstantNull() {
        helpTest(getFactory().newConstant(null), "null");
    }

    @Test
    public void testConstantString() {
        helpTest(getFactory().newConstant("abc"), "'abc'");
    }

    @Test
    public void testConstantInteger() {
        helpTest(getFactory().newConstant(new Integer(5)), "5");
    }

    @Test
    public void testConstantBigDecimal() {
        helpTest(getFactory().newConstant(new BigDecimal("5.4")), "5.4");
    }

    @Test
    public void testConstantStringWithTick() {
        helpTest(getFactory().newConstant("O'Leary"), "'O''Leary'");
    }

    @Test
    public void testConstantStringWithTicks() {
        helpTest(getFactory().newConstant("'abc'"), "'''abc'''");
    }

    @Test
    public void testConstantStringWithMoreTicks() {
        helpTest(getFactory().newConstant("a'b'c"), "'a''b''c'");
    }

    @Test
    public void testConstantStringWithDoubleTick() {
        helpTest(getFactory().newConstant("group=\"x\""), "'group=\"x\"'");
    }

    @Test
    public void testConstantBooleanTrue() {
        helpTest(getFactory().newConstant(Boolean.TRUE), "TRUE");
    }

    @Test
    public void testConstantBooleanFalse() {
        helpTest(getFactory().newConstant(Boolean.FALSE), "FALSE");
    }

    @Test
    public void testConstantDate() {
        helpTest(getFactory().newConstant(java.sql.Date.valueOf("2002-10-02")), "{d'2002-10-02'}");
    }

    @Test
    public void testConstantTime() {
        helpTest(getFactory().newConstant(java.sql.Time.valueOf("5:00:00")), "{t'05:00:00'}");
    }

    @Test
    public void testConstantTimestamp() {
        helpTest(getFactory().newConstant(java.sql.Timestamp.valueOf("2002-10-02 17:10:35.0234")), "{ts'2002-10-02 17:10:35.0234'}");
    }

    @Test
    public void testElementSymbol1() {
        ElementSymbolImpl es = getFactory().newElementSymbol("elem");
        helpTest(es, "elem");
    }

    @Test
    public void testElementSymbol2() {
        ElementSymbolImpl es = getFactory().newElementSymbol("elem");
        es.setDisplayFullyQualified(false);
        es.setGroupSymbol(getFactory().newGroupSymbol("m.g"));
        helpTest(es, "elem");
    }

    @Test
    public void testElementSymbol3() {
        ElementSymbolImpl es = getFactory().newElementSymbol("m.g.elem");
        es.setDisplayFullyQualified(true);
        es.setGroupSymbol(getFactory().newGroupSymbol("m.g"));
        helpTest(es, "m.g.elem");
    }

    @Test
    public void testElementSymbol4() {
        ElementSymbolImpl es = getFactory().newElementSymbol("vdb.m.g.elem");
        es.setDisplayFullyQualified(true);
        helpTest(es, "vdb.m.g.elem");
    }

    @Test
    public void testElementSymbol5() {
        ElementSymbolImpl es = getFactory().newElementSymbol("m.g.select");
        es.setDisplayFullyQualified(false);
        es.setGroupSymbol(getFactory().newGroupSymbol("m.g"));
        helpTest(es, "\"select\"");
    }

    @Test
    public void testExpressionSymbol1() {
        BaseExpression expr = getFactory().wrapExpression(getFactory().newConstant("abc"), "abc");
        helpTest(expr, "'abc'");
    }

    @Test public void testFunction1() {
            FunctionImpl func = getFactory().newFunction("concat", new BaseExpression[] {
                getFactory().newConstant("a"), null    
            });
            helpTest(func, "concat('a', <undefined>)");
        }

    @Test public void testFunction2() {
            FunctionImpl func = getFactory().newFunction("now", new BaseExpression[] {});
            helpTest(func, "now()");
        }

    @Test public void testFunction3() {
            FunctionImpl func = getFactory().newFunction("concat", new BaseExpression[] {null, null});
            helpTest(func, "concat(<undefined>, <undefined>)");
        }

    @Test public void testFunction4() {
            FunctionImpl func1 = getFactory().newFunction("power", new BaseExpression[] {
                getFactory().newConstant(new Integer(5)), 
                getFactory().newConstant(new Integer(3)) });
            FunctionImpl func2 = getFactory().newFunction("power", new BaseExpression[] {
                func1, 
                getFactory().newConstant(new Integer(3)) });            
            FunctionImpl func3 = getFactory().newFunction("+", new BaseExpression[] {
                getFactory().newConstant(new Integer(1000)),
                func2 });
            helpTest(func3, "(1000 + power(power(5, 3), 3))");
        }

    @Test public void testFunction5() {
            FunctionImpl func1 = getFactory().newFunction("concat", new BaseExpression[] {
                getFactory().newElementSymbol("elem2"),
                null });
            FunctionImpl func2 = getFactory().newFunction("concat", new BaseExpression[] {
                getFactory().newElementSymbol("elem1"),
                func1 });            
            helpTest(func2, "concat(elem1, concat(elem2, <undefined>))");
        }

    @Test public void testConvertFunction1() {
            FunctionImpl func = getFactory().newFunction("convert", new BaseExpression[] {
                getFactory().newConstant("5"), 
                getFactory().newConstant("integer")    
            });
            helpTest(func, "convert('5', integer)");
        }

    @Test public void testConvertFunction2() {
            FunctionImpl func = getFactory().newFunction("convert", new BaseExpression[] {
                null, 
                getFactory().newConstant("integer")    
            });
            helpTest(func, "convert(<undefined>, integer)");
        }

    @Test public void testConvertFunction3() {
            FunctionImpl func = getFactory().newFunction("convert", new BaseExpression[] {
                getFactory().newConstant(null), 
                getFactory().newConstant("integer")    
            });
            helpTest(func, "convert(null, integer)");
        }

    @Test public void testConvertFunction4() {
            FunctionImpl func = getFactory().newFunction("convert", new BaseExpression[] {
                getFactory().newConstant("abc"), 
                null    
            });
            helpTest(func, "convert('abc', <undefined>)");
        }

    @Test
    public void testConvertFunction5() {
        FunctionImpl func = getFactory().newFunction("convert");
        helpTest(func, "convert()");
    }

    @Test
    public void testConvertFunction6() {
        FunctionImpl func = getFactory().newFunction("convert", new BaseExpression[0]);
        helpTest(func, "convert()");
    }

    @Test public void testConvertFunction7() {
            FunctionImpl func = getFactory().newFunction("convert", new BaseExpression[] {getFactory().newConstant("abc")});
            helpTest(func, "convert('abc', <undefined>)");
        }

    @Test public void testCastFunction1() {
            FunctionImpl func = getFactory().newFunction("cast", new BaseExpression[] {
                getFactory().newConstant("5"), 
                getFactory().newConstant("integer")    
            });
            helpTest(func, "cast('5' AS integer)");
        }

    @Test public void testCastFunction2() {
            FunctionImpl func = getFactory().newFunction("cast", new BaseExpression[] {
                null, 
                getFactory().newConstant("integer")    
            });
            helpTest(func, "cast(<undefined> AS integer)");
        }

    @Test public void testCastFunction3() {
            FunctionImpl func = getFactory().newFunction("cast", new BaseExpression[] {
                getFactory().newConstant(null), 
                getFactory().newConstant("integer")    
            });
            helpTest(func, "cast(null AS integer)");
        }

    @Test public void testCastFunction4() {
            FunctionImpl func = getFactory().newFunction("cast", new BaseExpression[] {
                getFactory().newConstant("abc"), 
                null    
            });
            helpTest(func, "cast('abc' AS <undefined>)");
        }

    @Test public void testArithemeticFunction1() { 
            FunctionImpl func = getFactory().newFunction("-", new BaseExpression[] { 
                getFactory().newConstant(new Integer(-2)),
                getFactory().newConstant(new Integer(-1))});
            helpTest(func, "(-2 - -1)");    
        }

    @Test
    public void testGroupSymbol1() {
        GroupSymbolImpl gs = getFactory().newGroupSymbol("g");
        helpTest(gs, "g");
    }

    @Test
    public void testGroupSymbol2() {
        GroupSymbolImpl gs = getFactory().newGroupSymbol("x", "g");
        helpTest(gs, "g AS x");
    }

    @Test
    public void testGroupSymbol3() {
        GroupSymbolImpl gs = getFactory().newGroupSymbol("vdb.g");
        helpTest(gs, "vdb.g");
    }

    @Test
    public void testGroupSymbol4() {
        GroupSymbolImpl gs = getFactory().newGroupSymbol("x", "vdb.g");
        helpTest(gs, "vdb.g AS x");
    }

    @Test
    public void testGroupSymbol5() {
        GroupSymbolImpl gs = getFactory().newGroupSymbol("from", "m.g");
        helpTest(gs, "m.g AS \"from\"");
    }

    @Test
    public void testGroupSymbol6() {
        GroupSymbolImpl gs = getFactory().newGroupSymbol("x", "on.select");
        helpTest(gs, "\"on\".\"select\" AS x");
    }

    @Test
    public void testExecNoParams() {
        StoredProcedureImpl proc = getFactory().newStoredProcedure();
        proc.setProcedureName("myproc");
        helpTest(proc, "EXEC myproc()");
    }

    @Test
    public void testExecInputParam() {
        StoredProcedureImpl proc = getFactory().newStoredProcedure();
        proc.setProcedureName("myproc");
        SPParameterImpl param = getFactory().newSPParameter(1, getFactory().newReference(0));
        proc.addParameter(param);
        helpTest(proc, "EXEC myproc(?)");
    }

    @Test
    public void testExecInputOutputParam() {
        StoredProcedureImpl proc = getFactory().newStoredProcedure();
        proc.setProcedureName("myproc");
        SPParameterImpl param1 = getFactory().newSPParameter(1, getFactory().newConstant(new Integer(5)));
        param1.setParameterType(SPParameter.ParameterInfo.IN);
        proc.addParameter(param1);

        SPParameterImpl param2 = getFactory().newSPParameter(2, SPParameter.ParameterInfo.OUT, "x");
        proc.addParameter(param2);

        helpTest(proc, "EXEC myproc(5)");
    }

    @Test
    public void testExecOutputInputParam() {
        StoredProcedureImpl proc = getFactory().newStoredProcedure();
        proc.setProcedureName("myproc");

        SPParameterImpl param2 = getFactory().newSPParameter(2, SPParameter.ParameterInfo.OUT, "x");
        proc.addParameter(param2);

        SPParameterImpl param1 = getFactory().newSPParameter(1, getFactory().newConstant(new Integer(5)));
        param1.setParameterType(SPParameter.ParameterInfo.IN);
        proc.addParameter(param1);

        helpTest(proc, "EXEC myproc(5)");
    }

    @Test
    public void testExecReturnParam() {
        StoredProcedureImpl proc = getFactory().newStoredProcedure();
        proc.setProcedureName("myproc");

        SPParameterImpl param = getFactory().newSPParameter(1, SPParameter.ParameterInfo.RETURN_VALUE, "ret");
        proc.addParameter(param);
        helpTest(proc, "EXEC myproc()");
    }

    @Test
    public void testExecNamedParam() {
        StoredProcedureImpl proc = getFactory().newStoredProcedure();
        proc.setDisplayNamedParameters(true);
        proc.setProcedureName("myproc");
        SPParameterImpl param = getFactory().newSPParameter(1, getFactory().newReference(0));
        param.setName("p1");//$NON-NLS-1$
        proc.addParameter(param);
        helpTest(proc, "EXEC myproc(p1 => ?)");
    }

    @Test
    public void testExecNamedParams() {
        StoredProcedureImpl proc = getFactory().newStoredProcedure();
        proc.setDisplayNamedParameters(true);
        proc.setProcedureName("myproc");
        SPParameterImpl param = getFactory().newSPParameter(1, getFactory().newReference(0));
        param.setName("p1");//$NON-NLS-1$
        proc.addParameter(param);
        SPParameterImpl param2 = getFactory().newSPParameter(2, getFactory().newReference(0));
        param2.setName("p2");//$NON-NLS-1$
        proc.addParameter(param2);
        helpTest(proc, "EXEC myproc(p1 => ?, p2 => ?)");
    }

    /**
     * Test when a parameter's name is a reserved word.
     * (Note: parameters should always have short names, not
     * multiple period-delimited name components.) 
     * 
     * @since 4.3
     */
    @Test
    public void testExecNamedParamsReservedWord() {
        StoredProcedureImpl proc = getFactory().newStoredProcedure();
        proc.setDisplayNamedParameters(true);
        proc.setProcedureName("myproc");
        SPParameterImpl param = getFactory().newSPParameter(1, getFactory().newReference(0));
        param.setName("in");//$NON-NLS-1$
        proc.addParameter(param);
        SPParameterImpl param2 = getFactory().newSPParameter(2, getFactory().newReference(0));
        param2.setName("in2");//$NON-NLS-1$
        proc.addParameter(param2);
        helpTest(proc, "EXEC myproc(\"in\" => ?, in2 => ?)");
    }

    // Test methods for Update Procedure Language Objects

    @Test
    public void testDeclareStatement() {
        DeclareStatementImpl dclStmt = getFactory().newDeclareStatement(getFactory().newElementSymbol("a"), "String");
        helpTest(dclStmt, "DECLARE String a;");
    }

    @Test
    public void testAssignmentStatement1() {
        AssignmentStatementImpl assigStmt = getFactory().newAssignmentStatement(getFactory().newElementSymbol("a"), getFactory().newConstant(new Integer(1)));
        helpTest(assigStmt, "a = 1;");
    }

    @Test
    public void testAssignmentStatement2() {
        QueryImpl q1 = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("x"));
        q1.setSelect(select);
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));
        q1.setFrom(from);

        AssignmentStatementImpl assigStmt = getFactory().newAssignmentStatement(getFactory().newElementSymbol("a"), q1);
        helpTest(assigStmt, "a = (SELECT x FROM g);");
    }

    @Test
    public void testCommandStatement1() {
        QueryImpl q1 = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("x"));
        q1.setSelect(select);
        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));
        q1.setFrom(from);

        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(q1);
        helpTest(cmdStmt, "SELECT x FROM g;");
    }

    @Test
    public void testCommandStatement2() {
        DeleteImpl d1 = getFactory().newNode(ASTNodes.DELETE);
        d1.setGroup(getFactory().newGroupSymbol("g"));
        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(d1);
        helpTest(cmdStmt, "DELETE FROM g;");
    }

    @Test
    public void testSubqueryCompareCriteria1() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        ElementSymbolImpl expr = getFactory().newElementSymbol("e2");

        SubqueryCompareCriteriaImpl scc = getFactory().newSubqueryCompareCriteria(expr,
                                                                              q1,
                                                                              Operator.EQ,
                                                                              PredicateQuantifier.ANY);

        helpTest(scc, "e2 = ANY (SELECT e1 FROM m.g1)");
    }

    @Test
    public void testSubqueryCompareCriteria2() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        ElementSymbolImpl expr = getFactory().newElementSymbol("e2");

        SubqueryCompareCriteriaImpl scc = getFactory().newSubqueryCompareCriteria(expr,
                                                                              q1,
                                                                              Operator.LE,
                                                                              PredicateQuantifier.SOME);

        helpTest(scc, "e2 <= SOME (SELECT e1 FROM m.g1)");
    }

    @Test
    public void testExistsCriteria1() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        ExistsCriteriaImpl ec = getFactory().newExistsCriteria(q1);

        helpTest(ec, "EXISTS (SELECT e1 FROM m.g1)");
    }

    @Test
    public void testDynamicCommand() {
        List<ElementSymbolImpl> symbols = new ArrayList<ElementSymbolImpl>();

        ElementSymbolImpl a1 = getFactory().newElementSymbol("a1");
        a1.setType(DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass());
        symbols.add(a1);

        DynamicCommandImpl obj = getFactory().newDynamicCommand();
        BaseExpression sql = getFactory().newConstant("SELECT a1 FROM g WHERE a2 = 5");

        obj.setSql(sql);
        obj.setAsColumns(symbols);
        obj.setAsClauseSet(true);
        obj.setIntoGroup(getFactory().newGroupSymbol("#g"));

        helpTest(obj, "EXECUTE IMMEDIATE 'SELECT a1 FROM g WHERE a2 = 5' AS a1 string INTO #g");
    }

    @Test
    public void testScalarSubquery() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        ScalarSubqueryImpl obj = getFactory().newScalarSubquery(q1);

        helpTest(obj, "(SELECT e1 FROM m.g1)");
    }

    @Test
    public void testNewSubqueryObjects() {

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newElementSymbol("e1"));
        FromImpl f1 = getFactory().newFrom();
        f1.addGroup(getFactory().newGroupSymbol("m.g1"));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);
        q1.setFrom(f1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newElementSymbol("e1"));
        s2.addSymbol(getFactory().wrapExpression(getFactory().newScalarSubquery(q1), "blargh"));
        FromImpl f2 = getFactory().newFrom();
        f2.addGroup(getFactory().newGroupSymbol("m.g2"));
        CriteriaImpl left = getFactory().newSubqueryCompareCriteria(getFactory().newElementSymbol("e3"), q1, Operator.GE, PredicateQuantifier.ANY);
        CriteriaImpl right = getFactory().newExistsCriteria(q1);
        CriteriaImpl outer = getFactory().newCompoundCriteria(CompoundCriteriaImpl.AND, left, right);
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);
        q2.setFrom(f2);
        q2.setCriteria(outer);

        helpTest(q2,
                 "SELECT e1, (SELECT e1 FROM m.g1) FROM m.g2 WHERE (e3 >= ANY (SELECT e1 FROM m.g1)) AND (EXISTS (SELECT e1 FROM m.g1))");
    }

    @Test
    public void testCaseExpression1() {
        helpTest(example(2), "CASE x WHEN 'a' THEN 0 WHEN 'b' THEN 1 ELSE 9999 END");
    }

    @Test
    public void testCaseExpression2() {
        CaseExpressionImpl example = example(2);
        example.setElseExpression(null);
        helpTest(example, "CASE x WHEN 'a' THEN 0 WHEN 'b' THEN 1 END");
    }

    @Test
    public void testCaseExpression3() {
        CaseExpressionImpl example = caseExample(3, 0, true);
        helpTest(example, "CASE x WHEN null THEN 0 WHEN 'b' THEN 1 WHEN 'c' THEN 2 ELSE 9999 END");
    }

    @Test
    public void testCaseExpression4() {
        CaseExpressionImpl example = caseExample(3, 2, true);
        example.setElseExpression(null);
        helpTest(example, "CASE x WHEN 'a' THEN 0 WHEN 'b' THEN 1 WHEN null THEN 2 END");
    }

    @Test
    public void testSearchedCaseExpression1() {
        helpTest(searchedCaseExample(2), "CASE WHEN x = 0 THEN 0 WHEN x = 1 THEN 1 ELSE 9999 END");

    }

    @Test
    public void testSearchedCaseExpression2() {
        SearchedCaseExpressionImpl example = searchedCaseExample(2);
        example.setElseExpression(null);
        helpTest(example, "CASE WHEN x = 0 THEN 0 WHEN x = 1 THEN 1 END");

    }

    /**  
     * For some reason this test was outputting
     * SELECT 'A' AS FOO UNION SELECT 'A' AS FOO
     */
    @Test
    public void testSetQueryUnionOfLiteralsCase3102() {

        String expected = "SELECT 'A' AS FOO UNION SELECT 'B' AS FOO";

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newAliasSymbol("FOO", getFactory().wrapExpression(getFactory().newConstant("A"), "xxx")));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newAliasSymbol("FOO", getFactory().wrapExpression(getFactory().newConstant("B"), "xxx")));
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);

        SetQueryImpl sq = getFactory().newSetQuery(q1, Operation.UNION, q2, false);

        helpTest(sq, expected);
    }

    /**  
     * For some reason this test was outputting
     * SELECT 'A' AS FOO UNION SELECT 'A' AS FOO
     * Same as above except that ExpressionSymbols' internal names (which aren't visible
     * in the query) are different
     */
    @Test
    public void testSetQueryUnionOfLiteralsCase3102a() {

        String expected = "SELECT 'A' AS FOO UNION SELECT 'B' AS FOO";

        SelectImpl s1 = getFactory().newSelect();
        s1.addSymbol(getFactory().newAliasSymbol("FOO", getFactory().wrapExpression(getFactory().newConstant("A"), "xxx")));
        QueryImpl q1 = getFactory().newQuery();
        q1.setSelect(s1);

        SelectImpl s2 = getFactory().newSelect();
        s2.addSymbol(getFactory().newAliasSymbol("FOO", getFactory().wrapExpression(getFactory().newConstant("B"), "yyy")));
        QueryImpl q2 = getFactory().newQuery();
        q2.setSelect(s2);

        SetQueryImpl sq = getFactory().newSetQuery(q1, Operation.UNION, q2, false);

        helpTest(sq, expected);
    }

    @Test
    public void testLimit() {
        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect(Arrays.asList(getFactory().newMultipleElementSymbol()));
        FromImpl from = getFactory().newFrom(Arrays.asList(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("a"))));
        query.setSelect(select);
        query.setFrom(from);
        
        LimitImpl limit = getFactory().newNode(ASTNodes.LIMIT);
        limit.setRowLimit(getFactory().newConstant(new Integer(100)));
        query.setLimit(limit);
        helpTest(query, "SELECT * FROM a LIMIT 100");
    }

    @Test
    public void testLimitWithOffset() {
        QueryImpl query = getFactory().newQuery();
        SelectImpl select = getFactory().newSelect(Arrays.asList(getFactory().newMultipleElementSymbol()));
        FromImpl from = getFactory().newFrom(Arrays.asList(getFactory().newUnaryFromClause(getFactory().newGroupSymbol("a"))));
        query.setSelect(select);
        query.setFrom(from);

        LimitImpl limit = getFactory().newNode(ASTNodes.LIMIT);
        limit.setOffset(getFactory().newConstant(new Integer(50)));
        limit.setRowLimit(getFactory().newConstant(new Integer(100)));
        query.setLimit(limit);
        helpTest(query, "SELECT * FROM a LIMIT 50, 100"); 
    }

    @Test
    public void testUnionOrderBy() throws Exception {
        CommandImpl command = parser.parseCommand("select pm1.g1.e1 from pm1.g1 union select e2 from pm1.g2 order by e1");
        TCQueryResolver queryResolver = new TCQueryResolver(parser);
        queryResolver.resolveCommand(command, metadataFactory.example1Cached());
        helpTest(command, "SELECT pm1.g1.e1 FROM pm1.g1 UNION SELECT e2 FROM pm1.g2 ORDER BY e1");
    }

    @Test
    public void testUnionBranchOrderBy() throws Exception {
        CommandImpl command = parser.parseCommand("select pm1.g1.e1 from pm1.g1 union (select e2 from pm1.g2 order by e1)");
        TCQueryResolver queryResolver = new TCQueryResolver(parser);
        queryResolver.resolveCommand(command, metadataFactory.example1Cached());
        helpTest(command, "SELECT pm1.g1.e1 FROM pm1.g1 UNION (SELECT e2 FROM pm1.g2 ORDER BY e1)");
    }

    @Test
    public void testAliasedOrderBy() throws Exception {
        CommandImpl command = parser.parseCommand("select pm1.g1.e1 as a from pm1.g1 order by a");
        TCQueryResolver queryResolver = new TCQueryResolver(parser);
        queryResolver.resolveCommand(command, metadataFactory.example1Cached());
        helpTest(command, "SELECT pm1.g1.e1 AS a FROM pm1.g1 ORDER BY a");
    }

    @Test
    public void testNumberOrderBy() throws Exception {
        CommandImpl command = parser.parseCommand("select pm1.g1.e1 as a from pm1.g1 order by 1");
        TCQueryResolver queryResolver = new TCQueryResolver(parser);
        queryResolver.resolveCommand(command, metadataFactory.example1Cached());
        helpTest(command, "SELECT pm1.g1.e1 AS a FROM pm1.g1 ORDER BY 1");
    }

    @Test
    public void testLikeRegex() throws Exception {
        helpTestExpression("x like_regex 'b'", "x LIKE_REGEX 'b'");
    }

    @Test
    public void testSimilar() throws Exception {
        helpTestExpression("x similar to 'b' escape 'c'", "x SIMILAR TO 'b' ESCAPE 'c'");
    }

    @Test
    public void testTextTable() throws Exception {
        String sql = "SELECT * from texttable(file columns x string WIDTH 1 NO TRIM NO ROW DELIMITER) as x";
        helpTest(parser.parseCommand(sql),
                 "SELECT * FROM TEXTTABLE(file COLUMNS x string WIDTH 1 NO TRIM NO ROW DELIMITER) AS x");
    }

}
