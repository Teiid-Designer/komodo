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
package org.teiid.query.sql.v7;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.parser.TeiidNodeFactory.ASTNodes;
import org.teiid.query.sql.AbstractTestCloning;
import org.teiid.query.sql.lang.CriteriaImpl;
import org.teiid.query.sql.lang.CriteriaOperator.Operator;
import org.teiid.query.sql.lang.CriteriaSelectorImpl;
import org.teiid.query.sql.lang.DropImpl;
import org.teiid.query.sql.lang.FromImpl;
import org.teiid.query.sql.lang.HasCriteriaImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.SelectImpl;
import org.teiid.query.sql.lang.TranslateCriteriaImpl;
import org.teiid.query.sql.proc.AssignmentStatementImpl;
import org.teiid.query.sql.proc.BlockImpl;
import org.teiid.query.sql.proc.BranchingStatementImpl.BranchingMode;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.proc.IfStatementImpl;
import org.teiid.query.sql.proc.LoopStatementImpl;
import org.teiid.query.sql.proc.RaiseErrorStatementImpl;
import org.teiid.query.sql.proc.StatementImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.FunctionImpl;
import org.teiid.query.sql.symbol.GroupSymbolImpl;

/**
 * Unit testing for the SQLStringVisitor for teiid version 7
 */
@SuppressWarnings( {"nls", "javadoc"} )
public class Test7Cloning extends AbstractTestCloning {

    private Test7Factory factory;

    /**
     *
     */
    public Test7Cloning() {
        super(Version.TEIID_7_7.get());
    }

    @Override
    protected Test7Factory getFactory() {
        if (factory == null)
            factory = new Test7Factory(parser);

        return factory;
    }

    /** SELECT 1.3e8 FROM a.g1 */
    @Test
    public void testFloatWithE() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(new Double(1.3e8))));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest(
                 "SELECT 1.3E8 FROM a.g1",
                 query);
    }

    /** SELECT -1.3e-6 FROM a.g1 */
    @Test
    public void testFloatWithMinusE() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(new Double(-1.3e-6))));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest(
                 "SELECT -1.3E-6 FROM a.g1",
                 query);
    }

    /** SELECT -1.3e+8 FROM a.g1 */
    @Test
    public void testFloatWithPlusE() {
        GroupSymbolImpl g = getFactory().newGroupSymbol("a.g1");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        select.addSymbol(getFactory().wrapExpression(getFactory().newConstant(new Double(-1.3e+8))));

        QueryImpl query = getFactory().newQuery(select, from);

        helpTest(
                 "SELECT -1.3E8 FROM a.g1",
                 query);
    }

    @Test
    public void testErrorStatement() throws Exception {
        RaiseErrorStatementImpl errStmt = getFactory().newNode(ASTNodes.RAISE_ERROR_STATEMENT);
        errStmt.setExpression(getFactory().newConstant("Test only"));

        helpTest("ERROR 'Test only';",
                     errStmt);
    }

    @Test
    public void testCriteriaSelector0() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.IS_NULL);
        critSelector.addElement(a);

        helpTest("IS NULL CRITERIA ON (a)", critSelector);
    }

    @Test
    public void testCriteriaSelector1() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.EQ);
        critSelector.addElement(a);

        helpTest("= CRITERIA ON (a)", critSelector);
    }

    @Test
    public void testCriteriaSelector2() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.NE);
        critSelector.addElement(a);

        helpTest("<> CRITERIA ON (a)", critSelector);
    }

    @Test
    public void testCriteriaSelector3() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.LT);
        critSelector.addElement(a);

        helpTest("< CRITERIA ON (a)", critSelector);
    }

    @Test
    public void testCriteriaSelector4() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.GT);
        critSelector.addElement(a);

        helpTest("> CRITERIA ON (a)", critSelector);
    }

    @Test
    public void testCriteriaSelector5() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.GE);
        critSelector.addElement(a);

        helpTest(">= CRITERIA ON (a)", critSelector);
    }

    @Test
    public void testCriteriaSelector6() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.LE);
        critSelector.addElement(a);

        helpTest("<= CRITERIA ON (a)", critSelector);
    }

    @Test
    public void testCriteriaSelector7() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.LIKE);
        critSelector.addElement(a);

        helpTest("LIKE CRITERIA ON (a)", critSelector);
    }

    @Test
    public void testCriteriaSelector8() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.IN);
        critSelector.addElement(a);

        helpTest("IN CRITERIA ON (a)", critSelector);
    }

    @Test
    public void testCriteriaSelector9() throws Exception {
        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        helpTest("CRITERIA", critSelector);
    }

    @Test
    public void testCriteriaSelector10() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.BETWEEN);
        critSelector.addElement(a);

        helpTest("BETWEEN CRITERIA ON (a)", critSelector);
    }

    /**HAS IS NULL CRITERIA ON (a)*/
    @Test
    public void testHasIsNullCriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.IS_NULL);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        helpTest("HAS IS NULL CRITERIA ON (a)",
                         hasSelector);
    }

    /**HAS LIKE CRITERIA ON (a)*/
    @Test
    public void testHasLikeCriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.LIKE);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        helpTest("HAS LIKE CRITERIA ON (a)",
                         hasSelector);
    }

    @Test
    public void testHasEQCriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List<ElementSymbolImpl> elements = new ArrayList<ElementSymbolImpl>();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.EQ);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        helpTest("HAS = CRITERIA ON (a)",
                         hasSelector);
    }

    @Test
    public void testHasNECriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.NE);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        helpTest("HAS <> CRITERIA ON (a)",
                         hasSelector);
    }

    /**HAS IN CRITERIA ON (a)*/
    @Test
    public void testHasInCriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List<ElementSymbolImpl> elements = new ArrayList<ElementSymbolImpl>();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.IN);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        helpTest("HAS IN CRITERIA ON (a)",
                         hasSelector);
    }

    /**HAS COMPARE_LT CRITERIA ON (a)*/
    @Test
    public void testHasLTCriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List<ElementSymbolImpl> elements = new ArrayList<ElementSymbolImpl>();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.LT);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        helpTest("HAS < CRITERIA ON (a)",
                         hasSelector);
    }

    /**HAS COMPARE_LE CRITERIA ON (a)*/
    @Test
    public void testHasLECriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List<ElementSymbolImpl> elements = new ArrayList<ElementSymbolImpl>();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.LE);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        helpTest("HAS <= CRITERIA ON (a)",
                         hasSelector);
    }

    /**HAS COMPARE_GT CRITERIA ON (a)*/
    @Test
    public void testHasGTCriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.GT);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        helpTest("HAS > CRITERIA ON (a)",
                         hasSelector);
    }

    /**HAS COMPARE_GE CRITERIA ON (a)*/
    @Test
    public void testHasGECriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.GE);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        helpTest("HAS >= CRITERIA ON (a)",
                         hasSelector);
    }

    /**HAS BETWEEN CRITERIA ON (a)*/
    @Test
    public void testHasBetweenCriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.BETWEEN);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        helpTest("HAS BETWEEN CRITERIA ON (a)",
                         hasSelector);
    }

    @Test
    public void testTranslateCriteria() throws Exception {
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.EQ, getFactory().newConstant(new Integer(5)));
        List critList = new ArrayList();
        critList.add(crit);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.IS_NULL);
        critSelector.setElements(elements);

        TranslateCriteriaImpl transCriteria = getFactory().newTranslateCriteria(critSelector, critList);

        helpTest(
                         "TRANSLATE IS NULL CRITERIA ON (a) WITH (a = 5)",
                         transCriteria);
    }

    /** original test */
    @Test
    public void testCreateUpdateProcedureCommand() {
        helpTestCreateUpdateProcedureCommandCase3025("CREATE PROCEDURE\nBEGIN\nDECLARE short var1;" +
                                                     "IF(HAS IS NULL CRITERIA ON (a))\nBEGIN\nvar1 = (SELECT a1 FROM g WHERE a2 = 5);\nEND\n"
                                                     +
                                                     "ELSE\nBEGIN\nDECLARE short var2;\nvar2 = (SELECT b1 FROM g WHERE a2 = 5);\nEND\n"
                                                     +
                                                     " END");

    }

    @Test
    public void testCreateUpdateProcedureCommandCase3025_1() {

        helpTestCreateUpdateProcedureCommandCase3025("CREATE PROCEDURE\nBEGIN\nDECLARE short var1;" +
                                                     "IF(HAS IS NULL CRITERIA ON (a))\nBEGIN\nvar1 = (SELECT a1 FROM g WHERE a2 = 5);\nEND\n"
                                                     +
                                                     "ELSE\nBEGIN\nDECLARE short var2;\nvar2 = (SELECT b1 FROM g WHERE a2 = 5);\nEND\n"
                                                     +
                                                     " END"); 

    }

    @Test
    public void testCreateUpdateProcedureCommandCase3025_2() {
        helpTestCreateUpdateProcedureCommandCase3025("CREATE PROCEDURE\nBEGIN\nDECLARE short var1;" +
                                                     "IF(HAS IS NULL CRITERIA ON (a))\nBEGIN\nvar1 = ((SELECT a1 FROM g WHERE a2 = 5) );\nEND\n"
                                                     +
                                                     "ELSE\nBEGIN\nDECLARE short var2;\nvar2 = (SELECT b1 FROM g WHERE a2 = 5);\nEND\n"
                                                     +
                                                     " END"); 
    }

    private void helpTestCreateUpdateProcedureCommandCase3025(String procedureString) {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List<ElementSymbolImpl> symbols = new ArrayList<ElementSymbolImpl>();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        List<ElementSymbolImpl> elseSymbols = new ArrayList<ElementSymbolImpl>();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);
        elseQuery.setCriteria(criteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        //has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.IS_NULL);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector);

        BlockImpl block = getFactory().newBlock();

        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" + 
                                  "IF(HAS IS NULL CRITERIA ON (a))"
                                  + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                                  "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                                  "var2 = (SELECT b1 FROM g WHERE a2 = 5);" + "\n" + "END" + "\n" + "END", cmd);

    }

    /** test an expression in parentheses in an assignment statement */
    @Test
    public void testCreateUpdateProcedureCommandCase3025_3() {

        String procedureString = "CREATE PROCEDURE\nBEGIN\nDECLARE short var1;" +
                                 "IF(HAS IS NULL CRITERIA ON (a))\nBEGIN\nvar1 = (concat('x', 'y') );\nEND\n" +
                                 "ELSE\nBEGIN\nDECLARE short var2;\nvar2 = (SELECT b1 FROM g WHERE a2 = 5);\nEND\n" +
                                 " END";

        helpTestCreateUpdateProcedureCommandCase3025_Expression(procedureString);
    }

    /** test an expression in parentheses in an assignment statement */
    @Test
    public void testCreateUpdateProcedureCommandCase3025_4() {

        String procedureString = "CREATE PROCEDURE\nBEGIN\nDECLARE short var1;" +
                                 "IF(HAS IS NULL CRITERIA ON (a))\nBEGIN\nvar1 = ((concat('x', 'y') ));\nEND\n" +
                                 "ELSE\nBEGIN\nDECLARE short var2;\nvar2 = (SELECT b1 FROM g WHERE a2 = 5);\nEND\n" +
                                 " END";

        helpTestCreateUpdateProcedureCommandCase3025_Expression(procedureString);
    }

    /** test an expression without parentheses in an assignment statement */
    @Test
    public void testCreateUpdateProcedureCommandCase3025_5() {

        String procedureString = "CREATE PROCEDURE\nBEGIN\nDECLARE short var1;" +
                                 "IF(HAS IS NULL CRITERIA ON (a))\nBEGIN\nvar1 = concat('x', 'y') ;\nEND\n" +
                                 "ELSE\nBEGIN\nDECLARE short var2;\nvar2 = (SELECT b1 FROM g WHERE a2 = 5);\nEND\n" +
                                 " END";

        helpTestCreateUpdateProcedureCommandCase3025_Expression(procedureString);
    }

    /** test an expression in parentheses in an assignment statement */
    private void helpTestCreateUpdateProcedureCommandCase3025_Expression(String procedureString) {
        String expectedString = "CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" + 
                                "IF(HAS IS NULL CRITERIA ON (a))" + "\n" + "BEGIN" + "\n" + "var1 = concat('x', 'y');" + "\n" +
                                "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                                "var2 = (SELECT b1 FROM g WHERE a2 = 5);" + "\n" + "END" + "\n" + "END";

        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        BaseExpression[] args = new BaseExpression[] {getFactory().newConstant("x"), getFactory().newConstant("y")};
        FunctionImpl function = getFactory().newFunction("concat", args);
        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, function);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        List<ElementSymbolImpl> elseSymbols = new ArrayList<ElementSymbolImpl>();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);
        elseQuery.setCriteria(criteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        //has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.IS_NULL);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector);

        BlockImpl block = getFactory().newBlock();

        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest(expectedString, cmd);
    }

    /**IF statement with has criteria */
    @Test
    public void testCreateUpdateProcedureCommand1() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);
        elseQuery.setCriteria(criteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        //has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                         "IF(HAS CRITERIA ON (a))" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                         "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                         "var2 = (SELECT b1 FROM g WHERE a2 = 5);" + "\n" + "END" + "\n" + "END", cmd);
    }

    @Test
    public void testCreateUpdateProcedureCommand0() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);
        elseQuery.setCriteria(criteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        //has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        //critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                         "IF(HAS CRITERIA)" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                         "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                         "var2 = (SELECT b1 FROM g WHERE a2 = 5);" + "\n" + "END" + "\n" + "END", cmd);
    }

    /**IF statement with has LIKE criteria */
    @Test
    public void testCreateUpdateProcedureCommand2() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);
        elseQuery.setCriteria(criteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        //has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.LIKE);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                         "IF(HAS LIKE CRITERIA ON (a))"
                         + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                         "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                         "var2 = (SELECT b1 FROM g WHERE a2 = 5);" + "\n" + "END" + "\n" + "END", cmd);
    }

    /**IF statement with has IN criteria */
    @Test
    public void testCreateUpdateProcedureCommand3() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);
        elseQuery.setCriteria(criteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        //has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.IN);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                         "IF(HAS IN CRITERIA ON (a))" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                         "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                         "var2 = (SELECT b1 FROM g WHERE a2 = 5);" + "\n" + "END" + "\n" + "END", cmd);
    }

    /**IF statement with has <> criteria */
    @Test
    public void testCreateUpdateProcedureCommand4() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);
        elseQuery.setCriteria(criteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        //has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        critSelector.setSelectorType(Operator.NE);
        critSelector.setElements(elements);

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                         "IF(HAS <> CRITERIA ON (a))" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                         "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                         "var2 = (SELECT b1 FROM g WHERE a2 = 5);" + "\n" + "END" + "\n" + "END", cmd);
    }

    /**Has criteria in WHERE clause*/
    @Test
    public void testCreateUpdateProcedureCommand5() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        //element for has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);

        CriteriaSelectorImpl critSelector2 = getFactory().newCriteriaSelector();
        //critSelector2.setSelectorType(Operator.NE);
        critSelector2.setElements(elements);

        HasCriteriaImpl hasSelector2 = getFactory().newHasCriteria(critSelector2);
        //has criteria for else block
        elseQuery.setCriteria(hasSelector2);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        CriteriaSelectorImpl critSelector1 = getFactory().newCriteriaSelector();
        critSelector1.setSelectorType(Operator.NE);
        critSelector1.setElements(elements);

        HasCriteriaImpl hasSelector1 = getFactory().newHasCriteria(critSelector1);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector1);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                         "IF(HAS <> CRITERIA ON (a))" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                         "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                         "var2 = (SELECT b1 FROM g WHERE HAS CRITERIA ON (a));" + "\n" + "END" + "\n" + "END", cmd);
    }

    /** Translate criteria (empty criteriaSelector in WHERE clause*/
    @Test
    public void testCreateUpdateProcedureCommand7() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        //element for has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);

        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.EQ, getFactory().newConstant(new Integer(5)));
        List critList = new ArrayList();
        critList.add(crit);

        CriteriaSelectorImpl critSelector2 = getFactory().newCriteriaSelector();
        //critSelector2.setSelectorType(Operator.IS_NULL);
        critSelector2.setElements(elements);

        TranslateCriteriaImpl transCriteria = getFactory().newTranslateCriteria(critSelector2, critList);
        elseQuery.setCriteria(transCriteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        CriteriaSelectorImpl critSelector1 = getFactory().newCriteriaSelector();
        critSelector1.setSelectorType(Operator.NE);
        critSelector1.setElements(elements);

        HasCriteriaImpl hasSelector1 = getFactory().newHasCriteria(critSelector1);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector1);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                 "IF(HAS <> CRITERIA ON (a))" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                 "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                 "var2 = (SELECT b1 FROM g WHERE TRANSLATE CRITERIA ON (a) WITH (a = 5));" + "\n" + "END" + "\n" + "END", cmd);
    }

    /** Translate criteria (is null criteriaSelector in WHERE clause*/
    @Test
    public void testCreateUpdateProcedureCommand9() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        //element for has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);

        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.EQ, getFactory().newConstant(new Integer(5)));
        List critList = new ArrayList();
        critList.add(crit);

        CriteriaSelectorImpl critSelector2 = getFactory().newCriteriaSelector();
        critSelector2.setSelectorType(Operator.IS_NULL);
        critSelector2.setElements(elements);

        TranslateCriteriaImpl transCriteria = getFactory().newTranslateCriteria(critSelector2, critList);
        elseQuery.setCriteria(transCriteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        CriteriaSelectorImpl critSelector1 = getFactory().newCriteriaSelector();
        critSelector1.setSelectorType(Operator.NE);
        critSelector1.setElements(elements);

        HasCriteriaImpl hasSelector1 = getFactory().newHasCriteria(critSelector1);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector1);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                         "IF(HAS <> CRITERIA ON (a))" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                         "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                         "var2 = (SELECT b1 FROM g WHERE TRANSLATE IS NULL CRITERIA ON (a) WITH (a = 5));"
                         + "\n" + "END" + "\n" + "END", cmd);
    }

    /** Translate criteria ( only with WHERE clause) */
    @Test
    public void testCreateUpdateProcedureCommand10() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        //element for has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);

        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.EQ, getFactory().newConstant(new Integer(5)));
        List critList = new ArrayList();
        critList.add(crit);

        TranslateCriteriaImpl transCriteria = getFactory().newTranslateCriteria();
        CriteriaSelectorImpl critSelector2 = getFactory().newCriteriaSelector();
        transCriteria.setTranslations(critList);
        transCriteria.setSelector(critSelector2);

        elseQuery.setCriteria(transCriteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        CriteriaSelectorImpl critSelector1 = getFactory().newCriteriaSelector();
        critSelector1.setSelectorType(Operator.NE);
        critSelector1.setElements(elements);

        HasCriteriaImpl hasSelector1 = getFactory().newHasCriteria(critSelector1);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector1);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                         "IF(HAS <> CRITERIA ON (a))" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                         "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                         "var2 = (SELECT b1 FROM g WHERE TRANSLATE CRITERIA WITH (a = 5));" + "\n" + "END" + "\n" + "END", cmd);
    }

    /** Translate criteria ( only with WHERE clause) */
    @Test
    public void testCreateUpdateProcedureCommand12() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        //element for has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);

        CriteriaImpl crit1 = getFactory().newCompareCriteria(a, Operator.EQ, getFactory().newConstant(new Integer(5)));
        ElementSymbolImpl m = getFactory().newElementSymbol("m");
        CriteriaImpl crit2 = getFactory().newCompareCriteria(m, Operator.EQ, getFactory().newConstant(new Integer(6)));
        List critList = new ArrayList();
        critList.add(crit1);
        critList.add(crit2);

        TranslateCriteriaImpl transCriteria = getFactory().newTranslateCriteria();
        CriteriaSelectorImpl critSelector2 = getFactory().newCriteriaSelector();
        transCriteria.setTranslations(critList);
        transCriteria.setSelector(critSelector2);

        elseQuery.setCriteria(transCriteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        CriteriaSelectorImpl critSelector1 = getFactory().newCriteriaSelector();
        critSelector1.setSelectorType(Operator.NE);
        critSelector1.setElements(elements);

        HasCriteriaImpl hasSelector1 = getFactory().newHasCriteria(critSelector1);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector1);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                 "IF(HAS <> CRITERIA ON (a))" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                 "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                 "var2 = (SELECT b1 FROM g WHERE TRANSLATE CRITERIA WITH (a = 5, m = 6));" + "\n" + "END" + "\n" + "END", cmd);

    }

    /** Translate criteria (with only Criteria in WHERE clause) */
    @Test
    public void testCreateUpdateProcedureCommand11() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        //element for has criteria
        ElementSymbolImpl a = getFactory().newElementSymbol("a");
        List elements = new ArrayList();
        elements.add(a);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);

        CriteriaImpl crit = getFactory().newCompareCriteria(a, Operator.EQ, getFactory().newConstant(new Integer(5)));
        List critList = new ArrayList();
        critList.add(crit);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();
        TranslateCriteriaImpl transCrit = getFactory().newTranslateCriteria();
        transCrit.setSelector(critSelector);

        elseQuery.setCriteria(transCrit);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        CriteriaSelectorImpl critSelector1 = getFactory().newCriteriaSelector();
        critSelector1.setSelectorType(Operator.NE);
        critSelector1.setElements(elements);

        HasCriteriaImpl hasSelector1 = getFactory().newHasCriteria(critSelector1);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector1);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                         "IF(HAS <> CRITERIA ON (a))" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                         "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                         "var2 = (SELECT b1 FROM g WHERE TRANSLATE CRITERIA);" + "\n" + "END" + "\n" + "END", cmd);
    }

    /**IF statement with has criteria no on */
    @Test
    public void testCreateUpdateProcedureCommand8() {
        //declare var1
        ElementSymbolImpl var1 = getFactory().newElementSymbol("var1");
        String shortType = new String("short");
        StatementImpl declStmt = getFactory().newDeclareStatement(var1, shortType);

        //ifblock
        List symbols = new ArrayList();
        symbols.add(getFactory().newElementSymbol("a1"));
        SelectImpl select = getFactory().newSelect(symbols);

        FromImpl from = getFactory().newFrom();
        from.addGroup(getFactory().newGroupSymbol("g"));

        CriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("a2"), Operator.EQ,
                                               getFactory().newConstant(new Integer(5)));

        QueryImpl query = getFactory().newQuery(select, from);
        query.setCriteria(criteria);

        AssignmentStatementImpl queryStmt = getFactory().newAssignmentStatement(var1, query);

        BlockImpl ifBlock = getFactory().newBlock();
        ifBlock.addStatement(queryStmt);

        //else block 
        ElementSymbolImpl var2 = getFactory().newElementSymbol("var2");
        StatementImpl elseDeclStmt = getFactory().newDeclareStatement(var2, shortType);

        List elseSymbols = new ArrayList();
        elseSymbols.add(getFactory().newElementSymbol("b1"));
        SelectImpl elseSelect = getFactory().newSelect(elseSymbols);

        QueryImpl elseQuery = getFactory().newQuery(elseSelect, from);
        elseQuery.setCriteria(criteria);

        AssignmentStatementImpl elseQueryStmt = getFactory().newAssignmentStatement(var2, elseQuery);

        BlockImpl elseBlock = getFactory().newBlock();
        List elseStmts = new ArrayList();
        elseStmts.add(elseDeclStmt);
        elseStmts.add(elseQueryStmt);

        elseBlock.setStatements(elseStmts);

        CriteriaSelectorImpl critSelector = getFactory().newCriteriaSelector();

        HasCriteriaImpl hasSelector = getFactory().newHasCriteria(critSelector);

        IfStatementImpl stmt = getFactory().newIfStatement(ifBlock, elseBlock, hasSelector);

        BlockImpl block = getFactory().newBlock();
        block.addStatement(declStmt);
        block.addStatement(stmt);

        CreateUpdateProcedureCommandImpl cmd = getFactory().newCreateUpdateProcedureCommand();
        cmd.setBlock(block);

        helpTest("CREATE PROCEDURE" + "\n" + "BEGIN" + "\n" + "DECLARE short var1;" + "\n" +
                         "IF(HAS CRITERIA)" + "\n" + "BEGIN" + "\n" + "var1 = (SELECT a1 FROM g WHERE a2 = 5);" + "\n" +
                         "END" + "\n" + "ELSE" + "\n" + "BEGIN" + "\n" + "DECLARE short var2;" + "\n" +
                         "var2 = (SELECT b1 FROM g WHERE a2 = 5);" + "\n" + "END" + "\n" + "END", cmd);
    }

    @Test
    public void testVirtualProcedure() {
        ElementSymbolImpl x = getFactory().newElementSymbol("x");
        String intType = new String("integer");
        StatementImpl dStmt = getFactory().newDeclareStatement(x, intType);

        GroupSymbolImpl g = getFactory().newGroupSymbol("m.g");
        FromImpl from = getFactory().newFrom();
        from.addGroup(g);

        SelectImpl select = getFactory().newSelect();
        ElementSymbolImpl c1 = getFactory().newElementSymbol("c1");
        select.addSymbol(c1);
        select.addSymbol(getFactory().newElementSymbol("c2"));

        QueryImpl query = getFactory().newQuery(select, from);

        x = getFactory().newElementSymbol("x");
        c1 = getFactory().newElementSymbol("mycursor.c1");
        StatementImpl assignmentStmt = getFactory().newAssignmentStatement(x, c1);
        BlockImpl block = getFactory().newBlock();
        block.addStatement(assignmentStmt);

        BlockImpl ifBlock = getFactory().newBlock();
        StatementImpl continueStmt = getFactory().newBranchingStatement(BranchingMode.CONTINUE);
        ifBlock.addStatement(continueStmt);
        CriteriaImpl crit = getFactory().newCompareCriteria(x, Operator.GT, getFactory().newConstant(new Integer(5)));
        IfStatementImpl ifStmt = getFactory().newIfStatement(crit, ifBlock);
        block.addStatement(ifStmt);

        String cursor = "mycursor";
        LoopStatementImpl loopStmt = getFactory().newLoopStatement(block, query, cursor);

        block = getFactory().newBlock();
        block.addStatement(dStmt);
        block.addStatement(loopStmt);
        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(query);
        block.addStatement(cmdStmt);

        CreateUpdateProcedureCommandImpl virtualProcedureCommand = getFactory().newCreateUpdateProcedureCommand();
        virtualProcedureCommand.setBlock(block);
        virtualProcedureCommand.setUpdateProcedure(false);

        helpTest(
                 "CREATE VIRTUAL PROCEDURE\nBEGIN\nDECLARE integer x;\n"
                 + "LOOP ON (SELECT c1, c2 FROM m.g) AS mycursor\nBEGIN\n"
                 + "x = mycursor.c1;\nIF(x > 5)\nBEGIN\nCONTINUE;\nEND\nEND\n" + "SELECT c1, c2 FROM m.g;\nEND",
                 virtualProcedureCommand);

    }

    @Test
    public void testDropTable() {
        DropImpl drop = getFactory().newNode(ASTNodes.DROP);
        drop.setTable(getFactory().newGroupSymbol("tempTable"));
        helpTest("DROP TABLE tempTable", drop);
    }
}
