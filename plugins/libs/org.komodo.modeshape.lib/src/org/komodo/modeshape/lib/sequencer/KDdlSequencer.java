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
package org.komodo.modeshape.lib.sequencer;

import java.io.IOException;
import java.util.Collections;
import java.util.List;
import javax.jcr.NamespaceRegistry;
import javax.jcr.RepositoryException;
import org.modeshape.jcr.api.nodetype.NodeTypeManager;
import org.modeshape.sequencer.ddl.DdlParser;
import org.modeshape.sequencer.ddl.DdlSequencer;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlParser;

/**
 * Subclass of {@link DdlSequencer} that only allows the
 * Teiid DDL dialect, avoiding confusion with other ddl
 * parsers.
 */
public class KDdlSequencer extends DdlSequencer {

    private final DdlParser teiidParser = new TeiidDdlParser();

    @Override
    public void initialize( NamespaceRegistry registry, NodeTypeManager nodeTypeManager ) throws RepositoryException, IOException {
        registerNodeTypes(DdlSequencer.class.getResourceAsStream("StandardDdl.cnd"), nodeTypeManager, true); //$NON-NLS-1$
        registerNodeTypes(DdlSequencer.class.getResourceAsStream("dialect/teiid/TeiidDdl.cnd"), nodeTypeManager, true); //$NON-NLS-1$
    }

    @Override
    protected List<DdlParser> getParserList() {
        return Collections.singletonList(teiidParser);
    }

}
