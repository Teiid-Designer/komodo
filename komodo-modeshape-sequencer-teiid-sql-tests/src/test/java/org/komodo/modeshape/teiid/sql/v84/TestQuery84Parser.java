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
package org.komodo.modeshape.teiid.sql.v84;

import java.util.Arrays;
import org.junit.Test;
import org.komodo.modeshape.teiid.sql.lang.CompareCriteriaImpl;
import org.komodo.modeshape.teiid.sql.lang.CriteriaImpl;
import org.komodo.modeshape.teiid.sql.lang.CriteriaOperator;
import org.komodo.modeshape.teiid.sql.lang.CriteriaOperator.Operator;
import org.komodo.modeshape.teiid.sql.lang.FromImpl;
import org.komodo.modeshape.teiid.sql.lang.QueryImpl;
import org.komodo.modeshape.teiid.sql.lang.SelectImpl;
import org.komodo.modeshape.teiid.sql.proc.BlockImpl;
import org.komodo.modeshape.teiid.sql.proc.BranchingStatementImpl.BranchingMode;
import org.komodo.modeshape.teiid.sql.proc.CommandStatementImpl;
import org.komodo.modeshape.teiid.sql.proc.CreateProcedureCommandImpl;
import org.komodo.modeshape.teiid.sql.proc.IfStatementImpl;
import org.komodo.modeshape.teiid.sql.proc.LoopStatementImpl;
import org.komodo.modeshape.teiid.sql.proc.StatementImpl;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbolImpl;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbolImpl;
import org.komodo.modeshape.teiid.sql.v8.TestQuery8Parser;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;

/**
 *
 */
@SuppressWarnings( {"nls", "javadoc"} )
public class TestQuery84Parser extends TestQuery8Parser {

    protected TestQuery84Parser(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public TestQuery84Parser() {
        this(Version.TEIID_8_4.get());
    }

    /**
     * Drops the CREATE VIRTUAL PROCEDURE prefix
     */
    @Override
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
        CriteriaImpl crit = getFactory().newCompareCriteria(getFactory().newObject(x), Operator.GT, getFactory().newConstant(new Integer(5)));
        IfStatementImpl ifStmt = getFactory().newIfStatement(crit, ifBlock);
        block.addStatement(ifStmt);

        String cursor = "mycursor";
        LoopStatementImpl loopStmt = getFactory().newLoopStatement(block, query, cursor);

        block = getFactory().newBlock();
        block.addStatement(dStmt);
        block.addStatement(loopStmt);
        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(getFactory().newObject(query));
        block.addStatement(cmdStmt);

        CreateProcedureCommandImpl virtualProcedureCommand = getFactory().newCreateProcedureCommand();
        virtualProcedureCommand.setBlock(block);

        helpTest("BEGIN DECLARE integer x; LOOP ON (SELECT c1, c2 FROM m.g) AS mycursor BEGIN x=mycursor.c1; IF(x > 5) BEGIN CONTINUE; END END SELECT c1, c2 FROM m.g; END",
                 "BEGIN\nDECLARE integer x;\n" + "LOOP ON (SELECT c1, c2 FROM m.g) AS mycursor\nBEGIN\n"
                 + "x = mycursor.c1;\nIF(x > 5)\nBEGIN\nCONTINUE;\nEND\nEND\n" + "SELECT c1, c2 FROM m.g;\nEND",
                 virtualProcedureCommand);
    }

    @Override
    @Test
    public void testIfElseWithoutBeginEnd() {
        String sql = "BEGIN IF (x > 1) select 1; IF (x > 1) select 1; ELSE select 1; END"; //$NON-NLS-1$
        String expected = "BEGIN\nIF(x > 1)\nBEGIN\nSELECT 1;\nEND\nIF(x > 1)\nBEGIN\nSELECT 1;\nEND\nELSE\nBEGIN\nSELECT 1;\nEND\nEND"; //$NON-NLS-1$

        QueryImpl query = getFactory().newQuery();
        BaseExpression expr = getFactory().wrapExpression(getFactory().newConstant(1));
        query.setSelect(getFactory().newSelect(Arrays.asList(expr))); 

        CommandStatementImpl commandStmt = getFactory().newCommandStatement(query);
        CompareCriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("x"), CriteriaOperator.Operator.GT, getFactory().newConstant(1)); //$NON-NLS-1$
        BlockImpl block = getFactory().newBlock();
        block.addStatement(commandStmt);

        IfStatementImpl ifStmt = getFactory().newIfStatement(criteria, block);
        IfStatementImpl ifStmt1 = getFactory().newObject(ifStmt);

        BlockImpl block2 = getFactory().newBlock();
        block2.addStatement(getFactory().newObject(commandStmt));
        ifStmt1.setElseBlock(block2);
        BlockImpl block3 = getFactory().newBlock();
        block3.addStatement(ifStmt);
        block3.addStatement(ifStmt1);

        CreateProcedureCommandImpl command = getFactory().newCreateProcedureCommand();
        command.setBlock(block3);
        helpTest(sql, expected, command);
    }

    @Override
    @Test
    public void testIfElseWithoutBeginAndWithoutCreateVirtualProcedurePrefix() {
        String sql = "BEGIN IF (x > 1) select 1; IF (x > 1) select 1; ELSE select 1; END"; //$NON-NLS-1$
        String expected = "BEGIN\nIF(x > 1)\nBEGIN\nSELECT 1;\nEND\nIF(x > 1)\nBEGIN\nSELECT 1;\nEND\nELSE\nBEGIN\nSELECT 1;\nEND\nEND"; //$NON-NLS-1$

        QueryImpl query = getFactory().newQuery();
        BaseExpression expr = getFactory().wrapExpression(getFactory().newConstant(1));
        query.setSelect(getFactory().newSelect(Arrays.asList(expr))); 

        CommandStatementImpl commandStmt = getFactory().newCommandStatement(query);
        CompareCriteriaImpl criteria = getFactory().newCompareCriteria(getFactory().newElementSymbol("x"), CriteriaOperator.Operator.GT, getFactory().newConstant(1)); //$NON-NLS-1$
        BlockImpl block = getFactory().newBlock();
        block.addStatement(commandStmt);

        IfStatementImpl ifStmt = getFactory().newIfStatement(criteria, block);
        IfStatementImpl ifStmt1 = getFactory().newObject(ifStmt);

        BlockImpl block2 = getFactory().newBlock();
        block2.addStatement(getFactory().newObject(commandStmt));
        ifStmt1.setElseBlock(block2);
        BlockImpl block3 = getFactory().newBlock();
        block3.addStatement(ifStmt);
        block3.addStatement(ifStmt1);

        CreateProcedureCommandImpl command = getFactory().newCreateProcedureCommand();
        command.setBlock(block3);
        helpTest(sql, expected, command);
    }
}
