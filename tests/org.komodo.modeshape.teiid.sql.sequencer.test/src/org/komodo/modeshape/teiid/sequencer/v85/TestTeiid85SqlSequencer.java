/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.modeshape.teiid.sequencer.v85;

import javax.jcr.Node;
import org.junit.Test;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.From;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.GroupBy;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Query;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon.Select;
import org.komodo.modeshape.teiid.sequencer.v84.TestTeiid84SqlSequencer;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.runtime.version.TeiidVersion;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class TestTeiid85SqlSequencer extends TestTeiid84SqlSequencer {

    /**
     */
    public TestTeiid85SqlSequencer() {
        super(Version.TEIID_8_5.get());
    }

    /**
     * @param teiidVersion
     */
    protected TestTeiid85SqlSequencer(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    @Test
    public void testGroupByRollup() throws Exception {
        String sql = "SELECT a FROM m.g GROUP BY rollup(b, c)";
        Node fileNode = sequenceSql(sql, TSQL_QUERY);

        Node queryNode = verify(fileNode, Query.ID, Query.ID);

        Node selectNode = verify(queryNode, Query.SELECT_REF_NAME, Select.ID);
        verifyElementSymbol(selectNode, Select.SYMBOLS_REF_NAME, "a");

        Node fromNode = verify(queryNode, Query.FROM_REF_NAME, From.ID);
        verifyUnaryFromClauseGroup(fromNode, From.CLAUSES_REF_NAME, 1, "m.g");

        Node groupByNode = verify(queryNode, Query.GROUP_BY_REF_NAME, GroupBy.ID);
        verifyElementSymbol(groupByNode, GroupBy.SYMBOLS_REF_NAME,  1, "b");
        verifyElementSymbol(groupByNode, GroupBy.SYMBOLS_REF_NAME,  2, "c");
        verifyProperty(groupByNode, GroupBy.ROLLUP_PROP_NAME, true);
    }
}
