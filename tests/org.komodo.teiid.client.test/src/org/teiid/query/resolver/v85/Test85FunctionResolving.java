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
package org.teiid.query.resolver.v85;

import static org.junit.Assert.assertEquals;

import org.junit.Test;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.teiid.query.eval.Evaluator;
import org.teiid.query.metadata.TransformationMetadata;
import org.teiid.query.resolver.util.ResolverVisitor;
import org.teiid.query.resolver.v8.Test8FunctionResolving;
import org.teiid.query.sql.symbol.Function;

@SuppressWarnings( {"nls", "javadoc"} )
public class Test85FunctionResolving extends Test8FunctionResolving {

    protected Test85FunctionResolving(ITeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public Test85FunctionResolving() {
        this(Version.TEIID_8_5.get());
    }

    /*
     * UDF function for testVarArgsFunction
     */
    public static String vararg(Object... vals) {
        return String.valueOf(vals.length);
    }

    @Test
    public void testVarArgsFunction() throws Exception {
        String ddl = "create foreign function func (VARIADIC z object) returns string options (JAVA_CLASS '"
                     + this.getClass().getName() + "', JAVA_METHOD 'vararg');\n";
        TransformationMetadata tm = getMetadataFactory().fromDDL(ddl, "x", "y");

        String sql = "func(('a', 'b'))";

        Function func = (Function)getQueryParser().parseExpression(sql);
        ResolverVisitor visitor = new ResolverVisitor(getTeiidVersion());
        visitor.resolveLanguageObject(func, tm);
        assertEquals(1, func.getArgs().length);

        assertEquals("2", new Evaluator(getTeiidVersion()).evaluate(func));
    }
}
