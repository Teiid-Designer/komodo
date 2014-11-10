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

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.resolver.v86.Test86Resolver;
import org.teiid.query.sql.lang.Query;

@SuppressWarnings( {"nls", "javadoc"} )
public class Test87Resolver extends Test86Resolver {

    protected Test87Resolver(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public Test87Resolver() {
        this(Version.TEIID_8_7.get());
    }

    @Test
    public void testResolveInputParameters() throws Exception {
        List bindings = new ArrayList();
        bindings.add("pm1.g2.e1 as x"); //$NON-NLS-1$

        Query resolvedQuery = (Query)helpResolveWithBindings("SELECT pm1.g1.e1, input.x FROM pm1.g1", metadata, bindings); //$NON-NLS-1$

        helpCheckFrom(resolvedQuery, new String[] {"pm1.g1"}); //$NON-NLS-1$
        assertEquals("SELECT pm1.g1.e1, pm1.g2.e1 AS x FROM pm1.g1", resolvedQuery.toString());
    }
}
