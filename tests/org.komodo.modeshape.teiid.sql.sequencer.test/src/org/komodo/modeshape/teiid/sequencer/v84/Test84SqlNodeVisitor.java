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
package org.komodo.modeshape.teiid.sequencer.v84;

import org.junit.Test;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.sequencer.v8.Test8SqlNodeVisitor;
import org.komodo.modeshape.teiid.sql.lang.DeleteImpl;
import org.komodo.modeshape.teiid.sql.proc.AssignmentStatementImpl;
import org.komodo.modeshape.teiid.sql.proc.BlockImpl;
import org.komodo.modeshape.teiid.sql.proc.CommandStatementImpl;
import org.komodo.modeshape.teiid.sql.proc.CreateProcedureCommandImpl;
import org.komodo.modeshape.teiid.sql.proc.RaiseStatementImpl;
import org.komodo.modeshape.teiid.sql.proc.StatementImpl;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class Test84SqlNodeVisitor extends Test8SqlNodeVisitor {

    protected Test84SqlNodeVisitor(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public Test84SqlNodeVisitor() {
        this(Version.TEIID_8_4.get());
    }

    @Override
    @Test
    public void testCreateUpdateProcedure1() {
        DeleteImpl d1 = getFactory().newNode(ASTNodes.DELETE);
        d1.setGroup(getFactory().newGroupSymbol("g"));
        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(d1);
        AssignmentStatementImpl assigStmt = getFactory().newAssignmentStatement(getFactory().newElementSymbol("a"), getFactory().newConstant(new Integer(1)));
        RaiseStatementImpl errStmt = getFactory().newRaiseStatement(getFactory().newConstant("My Error"));
        BlockImpl b = getFactory().newBlock();
        b.addStatement(cmdStmt);
        b.addStatement(assigStmt);
        b.addStatement(errStmt);
        CreateProcedureCommandImpl cup = getFactory().newCreateProcedureCommand(b);
        helpTest(cup, "BEGIN\nDELETE FROM g;\na = 1;\nRAISE 'My Error';\nEND");
    }

    @Override
    @Test
    public void testCreateUpdateProcedure2() {
        DeleteImpl d1 = getFactory().newNode(ASTNodes.DELETE);
        d1.setGroup(getFactory().newGroupSymbol("g"));
        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(d1);
        AssignmentStatementImpl assigStmt = getFactory().newAssignmentStatement(getFactory().newElementSymbol("a"), getFactory().newConstant(new Integer(1)));
        RaiseStatementImpl errStmt = getFactory().newRaiseStatement(getFactory().newConstant("My Error"));
        BlockImpl b = getFactory().newBlock();
        b.addStatement(cmdStmt);
        b.addStatement(assigStmt);
        b.addStatement(errStmt);
        CreateProcedureCommandImpl cup = getFactory().newCreateProcedureCommand(b);
        helpTest(cup, "BEGIN\nDELETE FROM g;\na = 1;\nRAISE 'My Error';\nEND");
    }

    @Override
    @Test
    public void testCreateUpdateProcedure3() {
        DeleteImpl d1 = getFactory().newNode(ASTNodes.DELETE);
        d1.setGroup(getFactory().newGroupSymbol("g"));
        CommandStatementImpl cmdStmt = getFactory().newCommandStatement(d1);
        AssignmentStatementImpl assigStmt = getFactory().newAssignmentStatement(getFactory().newElementSymbol("a"), getFactory().newConstant(new Integer(1)));
        StatementImpl errStmt = getFactory().newRaiseStatement(getFactory().newConstant("My Error"));
        BlockImpl b = getFactory().newBlock();
        b.addStatement(cmdStmt);
        b.addStatement(assigStmt);
        b.addStatement(errStmt);
        CreateProcedureCommandImpl cup = getFactory().newCreateProcedureCommand(b);
        helpTest(cup, "BEGIN\nDELETE FROM g;\na = 1;\nRAISE 'My Error';\nEND");
    }

}
