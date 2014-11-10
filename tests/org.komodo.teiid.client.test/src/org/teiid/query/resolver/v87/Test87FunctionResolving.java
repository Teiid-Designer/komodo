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
package org.teiid.query.resolver.v87;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;
import org.teiid.api.exception.query.QueryResolverException;
import org.teiid.core.types.DefaultDataTypeManager;
import org.komodo.spi.query.metadata.QueryMetadataInterface;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.metadata.TransformationMetadata;
import org.teiid.query.resolver.util.ResolverVisitor;
import org.teiid.query.resolver.v86.Test86FunctionResolving;
import org.teiid.query.sql.symbol.Function;
import org.teiid.query.unittest.RealMetadataFactory.DDLHolder;

@SuppressWarnings( {"nls", "javadoc"} )
public class Test87FunctionResolving extends Test86FunctionResolving {

    protected Test87FunctionResolving(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public Test87FunctionResolving() {
        this(Version.TEIID_8_7.get());
    }

    /*
     * UDF function for testVarArgsFunction
     */
    public static String vararg(Object... vals) {
        return String.valueOf(vals.length);
    }

    @Test
    public void testAmbiguousUDF() throws Exception {
        TransformationMetadata tm = getMetadataFactory().fromDDL("x",
                                                                new DDLHolder("y", "create foreign function f () returns string"),
                                                                new DDLHolder("z", "create foreign function f () returns string"));

        String sql = "f()";
        Function func = (Function)getQueryParser().parseExpression(sql);

        ResolverVisitor resolver = new ResolverVisitor(getTeiidVersion());
        try {
            resolver.resolveLanguageObject(func, tm);
            fail();
        } catch (QueryResolverException e) {
            // successful test
        }

        sql = "z.f()";
        func = (Function)getQueryParser().parseExpression(sql);
        resolver = new ResolverVisitor(getTeiidVersion());
        resolver.resolveLanguageObject(func, tm);
    }

    @Test
    public void testUDFResolveOrder() throws Exception {

        QueryMetadataInterface tm = getMetadataFactory().fromDDL("create foreign function func(x object) returns object; "
                                                                + " create foreign function func(x string) returns string;"
                                                                + " create foreign function func1(x object) returns double;"
                                                                + " create foreign function func1(x string[]) returns bigdecimal;",
                                                                "x",
                                                                "y");

        String sql = "func('a')";

        Function func = (Function) getQueryParser().parseExpression(sql);
        ResolverVisitor resolver = new ResolverVisitor(getTeiidVersion());
        resolver.resolveLanguageObject(func, tm);
        assertEquals(DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass(), func.getArgs()[0].getType());
        assertEquals(DefaultDataTypeManager.DefaultDataTypes.STRING.getTypeClass(), func.getType());

        sql = "func1(('1',))";

        func = (Function) getQueryParser().parseExpression(sql);
        resolver = new ResolverVisitor(getTeiidVersion());
        resolver.resolveLanguageObject(func, tm);
        System.out.println(func.getType());
    }
}
