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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;

import javax.jcr.Binary;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.Property;
import javax.jcr.Session;

import org.modeshape.common.text.ParsingException;
import org.modeshape.common.util.IoUtil;
import org.teiid.modeshape.sequencer.ddl.DdlParser;
import org.teiid.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlParser;
import org.teiid.modeshape.sequencer.ddl.TeiidDdlSequencer;
import org.teiid.modeshape.sequencer.ddl.node.AstNode;
import org.teiid.modeshape.sequencer.ddl.node.AstNodeFactory;

/**
 * Subclass that only allows the Teiid DDL dialect, avoiding confusion with other DDL parsers.
 */
public class KDdlSequencer extends TeiidDdlSequencer {

    private final DdlParser teiidParser = new TeiidDdlParser();

    @Override
    protected List<DdlParser> getParserList() {
        return Collections.singletonList(teiidParser);
    }

    @Override
    public boolean execute(Property inputProperty, Node outputNode, Context context) throws Exception {
        if (! super.execute(inputProperty, outputNode, context)) {

            //
            // We know the sequencer failed to execute but unforunately the parsing exception
            // are handled and simply pushed to the logger. We want to throw them back up to
            // the calling transaction so they get some visibility
            //

            AstNodeFactory nodeFactory = new AstNodeFactory();
            final AstNode tempNode = nodeFactory.node(StandardDdlLexicon.STATEMENTS_CONTAINER);
            Binary ddlContent = inputProperty.getBinary();

            try (InputStream stream = ddlContent.getStream()) {
                teiidParser.parse(IoUtil.read(stream), tempNode, null);
            } catch (ParsingException e) {
                throw new Exception(e);
            } catch (IOException e) {
                throw new Exception(e);
            }

            //
            // Something went wrong but clearly not a parsing exception
            //
            return false;
        }

        if (! outputNode.hasNode(StandardDdlLexicon.STATEMENTS_CONTAINER))
            return false;

        Node ddlStmtsNode = outputNode.getNode(StandardDdlLexicon.STATEMENTS_CONTAINER);
        NodeIterator children = ddlStmtsNode.getNodes();

        Session session = ddlStmtsNode.getSession();
        if (! session.isLive())
            return false;

        while (children.hasNext()) {
            Node child = children.nextNode();
            session.move(child.getPath(), outputNode.getPath() + File.separator + child.getName());
        }

        session.removeItem(ddlStmtsNode.getPath());
        return true;
    }

}
