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
package org.teiid.query.resolver.v7;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.junit.Test;
import org.teiid.core.types.DefaultDataTypeManager;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.resolver.AbstractTestResolver;
import org.teiid.query.sql.AbstractTestFactory;
import org.teiid.query.sql.ProcedureReservedWords;
import org.teiid.query.sql.lang.CommandImpl;
import org.teiid.query.sql.lang.CompareCriteriaImpl;
import org.teiid.query.sql.lang.QueryImpl;
import org.teiid.query.sql.lang.SPParameterImpl;
import org.teiid.query.sql.lang.StoredProcedureImpl;
import org.teiid.query.sql.proc.CommandStatementImpl;
import org.teiid.query.sql.proc.CreateUpdateProcedureCommandImpl;
import org.teiid.query.sql.symbol.ElementSymbolImpl;
import org.teiid.query.sql.symbol.BaseExpression;
import org.teiid.query.sql.symbol.SymbolImpl;
import org.teiid.query.sql.v7.Test7Factory;

/**
 *
 */
@SuppressWarnings( {"nls" , "javadoc"})
public class Test7Resolver extends AbstractTestResolver {

    private Test7Factory factory;

    /**
     *
     */
    public Test7Resolver() {
        super(Version.TEIID_7_7.get());
    }

    @Override
    protected AbstractTestFactory getFactory() {
        if (factory == null)
            factory = new Test7Factory(getQueryParser());

        return factory;
    }

    @Test
    public void testSelectExpressions() {
        QueryImpl resolvedQuery = (QueryImpl)helpResolve("SELECT e1, concat(e1, 's'), concat(e1, 's') as c FROM pm1.g1"); //$NON-NLS-1$
        helpCheckFrom(resolvedQuery, new String[] {"pm1.g1"}); //$NON-NLS-1$
        helpCheckSelect(resolvedQuery, new String[] {"pm1.g1.e1", "expr", "c"}); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        helpCheckElements(resolvedQuery.getSelect(), new String[] {"pm1.g1.e1", "pm1.g1.e1", "pm1.g1.e1"}, //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                          new String[] {"pm1.g1.e1", "pm1.g1.e1", "pm1.g1.e1"}); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    @Test
    public void testSelectCountStar() {
        QueryImpl resolvedQuery = (QueryImpl)helpResolve("SELECT count(*) FROM pm1.g1"); //$NON-NLS-1$
        helpCheckFrom(resolvedQuery, new String[] {"pm1.g1"}); //$NON-NLS-1$
        helpCheckSelect(resolvedQuery, new String[] {"count"}); //$NON-NLS-1$
        helpCheckElements(resolvedQuery.getSelect(), new String[] {}, new String[] {});
    }

    @Test
    public void testConversionNotPossible() {
        helpResolveException("SELECT dayofmonth('2002-01-01') FROM pm1.g1", "Error Code:ERR.015.008.0040 Message:The function 'dayofmonth('2002-01-01')' is a valid function form, but the arguments do not match a known type signature and cannot be converted using implicit type conversions."); //$NON-NLS-1$ //$NON-NLS-2$
    }

    @Test
    public void testResolveParameters() throws Exception {
        List bindings = new ArrayList();
        bindings.add("pm1.g2.e1"); //$NON-NLS-1$
        bindings.add("pm1.g2.e2"); //$NON-NLS-1$

        QueryImpl resolvedQuery = (QueryImpl)helpResolveWithBindings("SELECT pm1.g1.e1, ? FROM pm1.g1 WHERE pm1.g1.e1 = ?", metadata, bindings); //$NON-NLS-1$

        helpCheckFrom(resolvedQuery, new String[] {"pm1.g1"}); //$NON-NLS-1$
        helpCheckSelect(resolvedQuery, new String[] {"pm1.g1.e1", "expr"}); //$NON-NLS-1$ //$NON-NLS-2$
        helpCheckElements(resolvedQuery.getCriteria(), new String[] {"pm1.g1.e1", "pm1.g2.e2"}, //$NON-NLS-1$
                          new String[] {"pm1.g1.e1", "pm1.g2.e2"}); //$NON-NLS-1$

    }

    @Test
    public void testStoredQuery1() {
        StoredProcedureImpl proc = (StoredProcedureImpl)helpResolve("EXEC pm1.sq2('abc')"); //$NON-NLS-1$

        // Check number of resolved parameters
        Collection<SPParameterImpl> params = proc.getParameters();
        assertEquals("Did not get expected parameter count", 2, proc.getParameterCount()); //$NON-NLS-1$

        // Check resolved parameters
        Iterator<SPParameterImpl> iterator = params.iterator();
        SPParameterImpl param1 = iterator.next();
        helpCheckParameter(param1,
                           SPParameterImpl.IN,
                           1,
                           "pm1.sq2.in", DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass(), getFactory().newConstant("abc")); //$NON-NLS-1$ //$NON-NLS-2$

        SPParameterImpl param2 = iterator.next();
        helpCheckParameter(param2, SPParameterImpl.RESULT_SET, 2, "pm1.sq2.ret", java.sql.ResultSet.class, null); //$NON-NLS-1$
    }

    /**
     * per defect 8211 - Input params do not have to be numbered sequentially in metadata.  For example,
     * the first input param can be #1 and the second input param can be #3.  (This occurs in 
     * QueryBuilder's metadata where the return param happens to be created in between the two
     * input params and is numbered #2, but is not loaded into QueryBuilder's runtime env).  
     * When the user's query is parsed and resolved, the placeholder
     * input params are numbered #1 and #2.  This test tests that this disparity in ordering should not
     * be a problem as long as RELATIVE ordering is in synch.
     */
    @Test
    public void testStoredQueryParamOrdering_8211() {
        StoredProcedureImpl proc = (StoredProcedureImpl)helpResolve("EXEC pm1.sq3a('abc', 123)"); //$NON-NLS-1$

     // Check number of resolved parameters
        Collection<SPParameterImpl> params = proc.getParameters();
        assertEquals("Did not get expected parameter count", 3, params.size()); //$NON-NLS-1$

        // Check resolved parameters
        Iterator<SPParameterImpl> parameters = params.iterator();
        SPParameterImpl param1 = parameters.next();
        helpCheckParameter(param1,
                           SPParameterImpl.IN,
                           1,
                           "pm1.sq3a.in", DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass(), getFactory().newConstant("abc")); //$NON-NLS-1$ //$NON-NLS-2$

        SPParameterImpl param2 = parameters.next();
        helpCheckParameter(param2,
                           SPParameterImpl.IN,
                           2,
                           "pm1.sq3a.in2", DefaultDataTypeManager.DefaultDataTypes.INTEGER.getTypeClass(), getFactory().newConstant(new Integer(123))); //$NON-NLS-1$
    }

    @Test
    public void testInputToInputsConversion() throws Exception {
        String procedure = "CREATE PROCEDURE  "; //$NON-NLS-1$
        procedure = procedure + "BEGIN\n"; //$NON-NLS-1$
        procedure = procedure + "DECLARE integer var1;\n"; //$NON-NLS-1$
        procedure = procedure + "ROWS_UPDATED = (Select pm1.g1.e2 from pm1.g1 where e2=INPUTS.e2);\n"; //$NON-NLS-1$
        procedure = procedure + "END\n"; //$NON-NLS-1$

        String userUpdateStr = "UPDATE vm1.g1 SET e2=40"; //$NON-NLS-1$

        CommandImpl command = helpResolveUpdateProcedure(procedure, userUpdateStr);
        assertEquals("CREATE PROCEDURE\nBEGIN\nDECLARE integer var1;\nROWS_UPDATED = (SELECT pm1.g1.e2 FROM pm1.g1 WHERE e2 = INPUTS.e2);\nEND",
                     command.toString());
    }

    @Test
    public void testCaseOverInlineView() throws Exception {
        String sql = "SELECT CASE WHEN x > 0 THEN 1.0 ELSE 2.0 END FROM (SELECT e2 AS x FROM pm1.g1) AS g"; //$NON-NLS-1$
        CommandImpl c = helpResolve(sql);
        assertEquals(sql, c.toString());
        verifyProjectedTypes(c, new Class[] {Double.class});
    }

    @Test
    public void testXMLQueryWithVariable() {
        String sql = "CREATE VIRTUAL PROCEDURE " //$NON-NLS-1$
                     + "BEGIN " //$NON-NLS-1$
                     + "declare string x = '1'; " //$NON-NLS-1$
                     + "select * from xmltest.doc1 where node1 = x; " //$NON-NLS-1$
                     + "end "; //$NON-NLS-1$

        CreateUpdateProcedureCommandImpl command = (CreateUpdateProcedureCommandImpl)helpResolve(sql);

        CommandStatementImpl cmdStmt = (CommandStatementImpl)command.getBlock().getStatements().get(1);

        CompareCriteriaImpl criteria = (CompareCriteriaImpl)((QueryImpl)cmdStmt.getCommand()).getCriteria();

        assertEquals(ProcedureReservedWords.VARIABLES,
                     ((ElementSymbolImpl)criteria.getRightExpression()).getGroupSymbol().getCanonicalName());
    }

    @Test
    public void testPowerWithLong_Fails() throws Exception {
        String sql = "SELECT power(10, 999999999999)"; //$NON-NLS-1$

        helpResolveException(sql);
    }

    @Test
    public void testUpdateError() {
        String userUpdateStr = "UPDATE vm1.g2 SET e1='x'"; //$NON-NLS-1$

        helpResolveException(userUpdateStr,
                             metadata,
                             "Error Code:ERR.015.008.0009 Message:Update is not allowed on the view vm1.g2: a procedure must be defined to handle the Update."); //$NON-NLS-1$
    }

    @Test
    public void testInsertError() {
        String userUpdateStr = "INSERT into vm1.g2 (e1) values ('x')"; //$NON-NLS-1$

        helpResolveException(userUpdateStr,
                             metadata,
                             "Error Code:ERR.015.008.0009 Message:Insert is not allowed on the view vm1.g2: a procedure must be defined to handle the Insert."); //$NON-NLS-1$
    }

    @Test
    public void testDeleteError() {
        String userUpdateStr = "DELETE from vm1.g2 where e1='x'"; //$NON-NLS-1$

        helpResolveException(userUpdateStr,
                             metadata,
                             "Error Code:ERR.015.008.0009 Message:Delete is not allowed on the view vm1.g2: a procedure must be defined to handle the Delete."); //$NON-NLS-1$
    }

    @Test
    public void testImplicitTempInsertWithNoColumns() {
        StringBuffer proc = new StringBuffer("CREATE VIRTUAL PROCEDURE") //$NON-NLS-1$
        .append("\nBEGIN") //$NON-NLS-1$
        .append("\n  create local temporary table #matt (x integer);") //$NON-NLS-1$
        .append("\n  insert into #matt values (1);") //$NON-NLS-1$
        .append("\nEND"); //$NON-NLS-1$

        CommandImpl cmd = helpResolve(proc.toString());

        String sExpected = "CREATE VIRTUAL PROCEDURE\nBEGIN\nCREATE LOCAL TEMPORARY TABLE #matt (x integer);\nINSERT INTO #matt (#matt.x) VALUES (1);\nEND\n\tCREATE LOCAL TEMPORARY TABLE #matt (x integer)\n\tINSERT INTO #matt (#matt.x) VALUES (1)\n"; //$NON-NLS-1$
        String sActual = cmd.printCommandTree();
        assertEquals(sExpected, sActual);
    }

    //return should be first, then out
    @Test
    public void testParamOrder() {
        QueryImpl resolvedQuery = (QueryImpl)helpResolve("SELECT * FROM (exec pm4.spRetOut()) as a", getMetadataFactory().exampleBQTCached()); //$NON-NLS-1$

        List<BaseExpression> projectedSymbols = resolvedQuery.getProjectedSymbols();
        assertFalse(projectedSymbols.isEmpty());
        BaseExpression symbol = projectedSymbols.get(0);
        assertTrue(symbol instanceof SymbolImpl);
        assertEquals("a.ret", ((SymbolImpl) symbol).getName());
    }
}
