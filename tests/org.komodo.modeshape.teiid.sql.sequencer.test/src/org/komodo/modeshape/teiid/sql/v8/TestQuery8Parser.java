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
package org.komodo.modeshape.teiid.sql.v8;

import java.util.Arrays;
import org.junit.Test;
import org.komodo.modeshape.teiid.parser.SQQueryParser;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.sql.AbstractTestQueryParser;
import org.komodo.modeshape.teiid.sql.lang.CompareCriteria;
import org.komodo.modeshape.teiid.sql.lang.Criteria;
import org.komodo.modeshape.teiid.sql.lang.CriteriaOperator;
import org.komodo.modeshape.teiid.sql.lang.CriteriaOperator.Operator;
import org.komodo.modeshape.teiid.sql.lang.From;
import org.komodo.modeshape.teiid.sql.lang.MatchCriteria;
import org.komodo.modeshape.teiid.sql.lang.Query;
import org.komodo.modeshape.teiid.sql.lang.Select;
import org.komodo.modeshape.teiid.sql.proc.AssignmentStatement;
import org.komodo.modeshape.teiid.sql.proc.Block;
import org.komodo.modeshape.teiid.sql.proc.BranchingStatement.BranchingMode;
import org.komodo.modeshape.teiid.sql.proc.CommandStatement;
import org.komodo.modeshape.teiid.sql.proc.CreateProcedureCommand;
import org.komodo.modeshape.teiid.sql.proc.ExceptionExpression;
import org.komodo.modeshape.teiid.sql.proc.IfStatement;
import org.komodo.modeshape.teiid.sql.proc.LoopStatement;
import org.komodo.modeshape.teiid.sql.proc.RaiseStatement;
import org.komodo.modeshape.teiid.sql.proc.Statement;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.modeshape.teiid.sql.symbol.Function;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.modeshape.teiid.sql.symbol.JSONObject;
import org.komodo.modeshape.teiid.sql.symbol.XMLSerialize;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;

/**
 * Unit testing for the Query Parser for teiid version 8
 */
@SuppressWarnings( {"nls", "javadoc"} )
public class TestQuery8Parser extends AbstractTestQueryParser {

    private Test8Factory factory;

    protected TestQuery8Parser(ITeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public TestQuery8Parser() {
        this(Version.TEIID_8_0.get());
    }

    @Override
    protected Test8Factory getFactory() {
        if (factory == null)
            factory = new Test8Factory(parser);

        return factory;
    }

    @Test
    public void testBasicSelect() {
        GroupSymbol g = getFactory().newGroupSymbol("m.g");
        From from = getFactory().newFrom();
        from.addGroup(g);

        Select select = getFactory().newSelect();
        select.addSymbol(getFactory().newElementSymbol("a"));

        Query query = getFactory().newQuery(select, from);
        helpTest("SELECT a FROM m.g", "SELECT a FROM m.g", query);
    }

    @Test
    public void testSignedExpression() {
        GroupSymbol g = getFactory().newGroupSymbol("g");
        From from = getFactory().newFrom();
        from.addGroup(g);

        Function f = getFactory().newFunction("*", new Expression[] {getFactory().newConstant(-1), getFactory().newElementSymbol("x")});
        Select select = getFactory().newSelect();
        select.addSymbol(f);
        select.addSymbol(getFactory().newElementSymbol("x"));
        select.addSymbol(getFactory().newConstant(5));

        Query query = getFactory().newQuery(select, from);
        helpTest("SELECT -x, +x, +5 FROM g",
                 "SELECT (-1 * x), x, 5 FROM g",
                 query);
    }

    @Test
    public void testFloatWithE() {
        GroupSymbol g = getFactory().newGroupSymbol("a.g1");
        From from = getFactory().newFrom();
        from.addGroup(g);

        Select select = getFactory().newSelect();
        select.addSymbol(getFactory().newConstant(new Double(1.3e8)));
        select.addSymbol(getFactory().newConstant(new Double(-1.3e+8)));
        select.addSymbol(getFactory().newConstant(new Double(+1.3e-8)));

        Query query = getFactory().newQuery(select, from);

        helpTest("SELECT 1.3e8, -1.3e+8, +1.3e-8 FROM a.g1",
                 "SELECT 1.3E8, -1.3E8, 1.3E-8 FROM a.g1",
                 query);
    }

    @Test
    public void testPgLike() {
        GroupSymbol g = getFactory().newGroupSymbol("db.g");
        From from = getFactory().newFrom();
        from.addGroup(g);

        Select select = getFactory().newSelect();
        ElementSymbol a = getFactory().newElementSymbol("a");
        select.addSymbol(a);

        Expression string1 = getFactory().newConstant("\\_aString");
        MatchCriteria crit = getFactory().newMatchCriteria(getFactory().newElementSymbol("b"), string1, '\\');

        Query query = getFactory().newQuery(select, from);
        query.setCriteria(crit);
        helpTest("SELECT a FROM db.g WHERE b LIKE E'\\\\_aString'",
                 "SELECT a FROM db.g WHERE b LIKE '\\_aString' ESCAPE '\\'",
                 query);
    }

    @Test
    public void testLikeWithEscapeException() {
        String expectedMsg = buildErrorMessage("'#1'", 1, 50, "LIKE/SIMILAR TO ESCAPE requires the value should have a length of 1 character only");
        helpException("SELECT a from db.g where b like '#String' escape '#1'", expectedMsg);
    }

    @Test
    public void testErrorStatement() throws Exception {
        ExceptionExpression ee = getFactory().newExceptionExpression();
        ee.setMessage(getFactory().newConstant("Test only"));
        RaiseStatement errStmt = getFactory().newNode(ASTNodes.RAISE_STATEMENT);
        errStmt.setExpression(ee);

        helpStmtTest("ERROR 'Test only';", "RAISE SQLEXCEPTION 'Test only';",
                     errStmt);
    }

    @Test
    public void testRaiseErrorStatement() throws Exception {
        ExceptionExpression ee = getFactory().newExceptionExpression();
        ee.setMessage(getFactory().newConstant("Test only"));
        ee.setSqlState(getFactory().newConstant("100"));
        ee.setParentExpression(getFactory().newElementSymbol("e"));
        RaiseStatement errStmt = getFactory().newNode(ASTNodes.RAISE_STATEMENT);
        errStmt.setExpression(ee);
        errStmt.setWarning(true);

        helpStmtTest("RAISE SQLWARNING SQLEXCEPTION 'Test only' SQLSTATE '100' chain e;", "RAISE SQLWARNING SQLEXCEPTION 'Test only' SQLSTATE '100' CHAIN e;",
                     errStmt);
    }

    @Test
    public void testXmlSerialize2() throws Exception {
        XMLSerialize f = getFactory().newXMLSerialize();
        f.setExpression(getFactory().newElementSymbol("x"));
        f.setTypeString("BLOB");
        f.setDeclaration(Boolean.TRUE);
        f.setVersion("1.0");
        f.setEncoding("UTF-8");
        helpTestExpression("xmlserialize(x as BLOB encoding \"UTF-8\" version '1.0' INCLUDING xmldeclaration)",
                           "XMLSERIALIZE(x AS BLOB ENCODING \"UTF-8\" VERSION '1.0' INCLUDING XMLDECLARATION)",
                           f);
    }

    @Test
    public void testWindowedExpression() {
        String expectedMsg = buildErrorMessage("over", 1, 18, "Cannot window a non-aggregate expression");
        String sql = "SELECT foo(x, y) over ()";
        helpException(sql, expectedMsg);
    }

    @Test
    public void testInvalidLimit() {
        String expectedMsg = buildErrorMessage("-", 1, 28, "Value must be absolute and not be prefixed with + or -");
        helpException("SELECT * FROM pm1.g1 LIMIT -5", expectedMsg);
    }

    @Test
    public void testInvalidLimit_Offset() {
        String expectedMsg = buildErrorMessage("-", 1, 28, "Value must be absolute and not be prefixed with + or -");
        helpException("SELECT * FROM pm1.g1 LIMIT -1, 100", expectedMsg);
    }

    @Test
    public void testTextTableNegativeWidth() {
        String expectedMsg = buildErrorMessage("-", 1, 53, "Value must be absolute and not be prefixed with + or -");
        helpException("SELECT * from texttable(null columns x string width -1) as x", expectedMsg);
    }

    @Test
    public void testTextTableNonNumericalWidthValue() {
        String expectedMsg = buildDefaultErrorMessage("p", 1, 53);
    
        helpException("SELECT * from texttable(null columns x string width p) as x", expectedMsg);
    }

    @Test
    public void testBlockExceptionHandling() throws Exception {
        Select select = getFactory().newSelectWithMultileElementSymbol();
        From from = getFactory().newFrom();
        from.setClauses(Arrays.asList(getFactory().newUnaryFromClause("x")));
        Query query = getFactory().newQuery(select, from);
        CommandStatement cmdStmt = getFactory().newCommandStatement(query);
        AssignmentStatement assigStmt = getFactory().newAssignmentStatement(getFactory().newElementSymbol("a"), getFactory().newConstant(new Integer(1)));
        RaiseStatement errStmt = getFactory().newNode(ASTNodes.RAISE_STATEMENT);
        ExceptionExpression ee = getFactory().newExceptionExpression();
        ee.setMessage(getFactory().newConstant("My Error"));
        errStmt.setExpression(ee);
        Block b = getFactory().newBlock();
        b.setExceptionGroup("e");
        b.addStatement(cmdStmt);
        b.addStatement(assigStmt);
        b.addStatement(errStmt, true);
        helpStmtTest("BEGIN\nselect * from x;\na = 1;\nexception e\nERROR 'My Error';\nEND", "BEGIN\nSELECT * FROM x;\na = 1;\nEXCEPTION e\nRAISE SQLEXCEPTION 'My Error';\nEND", b);
    }

    @Test
    public void testJSONObject() throws Exception {
        JSONObject f = getFactory().newJSONObject(Arrays.asList(getFactory().newDerivedColumn("table", getFactory().newElementSymbol("a"))));
        helpTestExpression("jsonObject(a as \"table\")", "JSONOBJECT(a AS \"table\")", f);
    }

    @Test
    public void testVirtualProcedure(){        
        ElementSymbol x = getFactory().newElementSymbol("x");
        String intType = new String("integer");
        Statement dStmt = getFactory().newDeclareStatement(x, intType);
        
        GroupSymbol g = getFactory().newGroupSymbol("m.g");
        From from = getFactory().newFrom();
        from.addGroup(g);
        
        Select select = getFactory().newSelect();
        ElementSymbol c1 = getFactory().newElementSymbol("c1");
        select.addSymbol(c1);
        select.addSymbol(getFactory().newElementSymbol("c2"));

        Query query = getFactory().newQuery(select, from);

        x = getFactory().newElementSymbol("x");
        c1 = getFactory().newElementSymbol("mycursor.c1");
        Statement assignmentStmt = getFactory().newAssignmentStatement(x, c1);
        Block block = getFactory().newBlock();
        block.addStatement(assignmentStmt);
        
        Block ifBlock = getFactory().newBlock();
        Statement continueStmt = getFactory().newBranchingStatement(BranchingMode.CONTINUE);
        ifBlock.addStatement(continueStmt);
        Criteria crit = getFactory().newCompareCriteria(getFactory().newObject(x), Operator.GT,  getFactory().newConstant(new Integer(5)));
        IfStatement ifStmt = getFactory().newIfStatement(crit, ifBlock);
        block.addStatement(ifStmt);
        
        String cursor = "mycursor";
        LoopStatement loopStmt = getFactory().newLoopStatement(block, query, cursor);
        
        block = getFactory().newBlock();
        block.addStatement(dStmt);
        block.addStatement(loopStmt);
        CommandStatement cmdStmt = getFactory().newCommandStatement(getFactory().newObject(query));
        block.addStatement(cmdStmt);
        
        CreateProcedureCommand virtualProcedureCommand = getFactory().newCreateProcedureCommand();
        virtualProcedureCommand.setBlock(block);
        
        helpTest("CREATE VIRTUAL PROCEDURE BEGIN DECLARE integer x; LOOP ON (SELECT c1, c2 FROM m.g) AS mycursor BEGIN x=mycursor.c1; IF(x > 5) BEGIN CONTINUE; END END SELECT c1, c2 FROM m.g; END",
        "CREATE VIRTUAL PROCEDURE\nBEGIN\nDECLARE integer x;\n"
        + "LOOP ON (SELECT c1, c2 FROM m.g) AS mycursor\nBEGIN\n"
        + "x = mycursor.c1;\nIF(x > 5)\nBEGIN\nCONTINUE;\nEND\nEND\n"
        + "SELECT c1, c2 FROM m.g;\nEND", virtualProcedureCommand);

    }

    @Test
    public void testIfElseWithoutBeginEnd() {
        String sql = "CREATE VIRTUAL PROCEDURE BEGIN IF (x > 1) select 1; IF (x > 1) select 1; ELSE select 1; END"; //$NON-NLS-1$
        String expected = "CREATE VIRTUAL PROCEDURE\nBEGIN\nIF(x > 1)\nBEGIN\nSELECT 1;\nEND\nIF(x > 1)\nBEGIN\nSELECT 1;\nEND\nELSE\nBEGIN\nSELECT 1;\nEND\nEND"; //$NON-NLS-1$

        Query query = getFactory().newQuery();
        Expression expr = getFactory().wrapExpression(getFactory().newConstant(1));
        query.setSelect(getFactory().newSelect(Arrays.asList(expr))); 

        CommandStatement commandStmt = getFactory().newCommandStatement(query);
        CompareCriteria criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("x"), CriteriaOperator.Operator.GT, getFactory().newConstant(1)); //$NON-NLS-1$
        Block block = getFactory().newBlock();
        block.addStatement(commandStmt);
        
        IfStatement ifStmt = getFactory().newIfStatement(criteria, block);
        IfStatement ifStmt1 = getFactory().newObject(ifStmt);
        
        Block block2 = getFactory().newBlock();
        block2.addStatement(getFactory().newObject(commandStmt));
        ifStmt1.setElseBlock(block2);
        Block block3 = getFactory().newBlock();
        block3.addStatement(ifStmt);
        block3.addStatement(ifStmt1);
        
        CreateProcedureCommand command = getFactory().newCreateProcedureCommand();
        command.setBlock(block3);
        helpTest(sql, expected, command);
    }

    private String buildProcedureVersionMsg() {
        return "Syntax is for Teiid Version 8.4.0+ while current Teiid Version is " + this.teiidVersion.toString() + ". Required Syntax: CREATE VIRTUAL PROCEDURE BEGIN";
    }
    
    @Test
    public void testIfElseWithoutBeginAndWithoutCreateVirtualProcedurePrefix() {
        String mainMsg = buildErrorMessage("BEGIN", 1, 1, "The SQL expression starting with \"BEGIN\" is not valid.");
        String sql = "BEGIN IF (x > 1) select 1; IF (x > 1) select 1; ELSE select 1; END"; //$NON-NLS-1$

        Version[] versions = new Version[] {
            Version.TEIID_8_0,
            Version.TEIID_8_1,
            Version.TEIID_8_2,
            Version.TEIID_8_3
        };

        for (Version version : versions) {
            this.teiidVersion = version.get();
            this.parser = new SQQueryParser(teiidVersion);

            /* CREATE VIRTUAL PROCEDURE is a required prefix for version 8.0 - 8.4 */
            helpException(sql, mainMsg + SPACE + buildProcedureVersionMsg());

            /* Check that the rest of the query is parsed despite the missing prefix by adding in deliberate error later on */
            String fullMsg = mainMsg + SPACE + buildProcedureVersionMsg() + NEW_LINE
                                + buildDefaultErrorMessage("ELSEselect", 1, 49) + NEW_LINE
                                + "Was expecting one of:" + NEW_LINE
                                + "    \"else\" ..." + NEW_LINE
                                + "    \"end\" ..." + NEW_LINE
                                + "    \"exception\" ..." + NEW_LINE
                                + "    ";
        
            /* Deliberate mistake for ELSE keyword in IF clause */
            String sql2 = "BEGIN IF (x > 1) select 1; IF (x > 1) select 1; ELSEselect 1; END"; //$NON-NLS-1$
            /* CREATE VIRTUAL PROCEDURE is a required prefix for version 8.0 - 8.4 */
            helpException(sql2, fullMsg);
        }
    }

    @Test
    public void testGroupByRollup() {
        String expectedMsg = buildErrorMessage("rollup", 1, 28, "Syntax is for Teiid Version 8.5.0+ while current Teiid Version is " + teiidVersion.toString());
        helpException("SELECT a FROM m.g GROUP BY rollup(b, c)", expectedMsg);
    }
}
