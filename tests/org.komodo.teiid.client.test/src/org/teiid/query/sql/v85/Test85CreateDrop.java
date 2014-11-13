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
package org.teiid.query.sql.v85;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.sql.v8.Test8CreateDrop;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class Test85CreateDrop extends Test8CreateDrop {

    protected Test85CreateDrop(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    public Test85CreateDrop() {
        this(Version.TEIID_8_5.get());
    }

    @Override
    @Test
    public void testCreateTempTable3() {
        String sql = "Create TEMPORARY table tempTable (c1 boolean, c2 byte)";
        try {
            assertEquals("CREATE LOCAL TEMPORARY TABLE tempTable (c1 boolean, c2 byte)",
                     parser.parseCommand(sql).toString()); //$NON-NLS-1$
        } catch (Throwable e) {
            fail(e.getMessage());
        }
    }
}
