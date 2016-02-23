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
package org.komodo.modeshape.teiid.sequencer.v87;

import javax.jcr.Node;
import org.junit.Test;
import org.komodo.modeshape.teiid.sequencer.v86.TestTeiid86SqlSequencer;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.komodo.spi.lexicon.TeiidSqlLexicon.Block;
import org.komodo.spi.lexicon.TeiidSqlLexicon.CommandStatement;
import org.komodo.spi.lexicon.TeiidSqlLexicon.CreateProcedureCommand;
import org.komodo.spi.lexicon.TeiidSqlLexicon.SPParameter;
import org.komodo.spi.lexicon.TeiidSqlLexicon.StoredProcedure;
import org.komodo.spi.lexicon.TeiidSqlLexicon.SubqueryContainer;
import org.komodo.spi.runtime.version.TeiidVersion;

/**
 *
 */
@SuppressWarnings( {"javadoc", "nls"} )
public class TestTeiid87SqlSequencer extends TestTeiid86SqlSequencer {

    /**
     */
    public TestTeiid87SqlSequencer() {
        super(Version.TEIID_8_7.get());
    }

    /**
     * @param teiidVersion
     */
    protected TestTeiid87SqlSequencer(TeiidVersion teiidVersion) {
        super(teiidVersion);
    }

    @Test
    public void testStoredQuery2SanityCheck() throws Exception {
        String sql = "BEGIN exec proc1('param1'); END";
        Node fileNode = sequenceSql(sql, TSQL_PROC_CMD);

        Node createProcNode = verify(fileNode, CreateProcedureCommand.ID, CreateProcedureCommand.ID);
        Node outerBlkNode = verify(createProcNode, CreateProcedureCommand.BLOCK_REF_NAME, Block.ID);
        Node cmdStmtNode = verify(outerBlkNode, Block.STATEMENTS_REF_NAME, CommandStatement.ID);

        Node storedProcNode = verify(cmdStmtNode, SubqueryContainer.COMMAND_REF_NAME, StoredProcedure.ID);
        verifyProperty(storedProcNode, StoredProcedure.PROCEDURE_NAME_PROP_NAME, "proc1");

        Node param1Node = verify(storedProcNode, StoredProcedure.PARAMETERS_REF_NAME, SPParameter.ID);
        verifyProperty(param1Node, SPParameter.PARAMETER_TYPE_PROP_NAME, 1);
        verifyProperty(param1Node, SPParameter.INDEX_PROP_NAME, 1);
        verifyConstant(param1Node, SPParameter.EXPRESSION_REF_NAME, "param1");
    }
}
