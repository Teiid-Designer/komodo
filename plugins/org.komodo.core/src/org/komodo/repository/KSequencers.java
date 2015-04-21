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
package org.komodo.repository;

import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import javax.jcr.query.Query;
import javax.jcr.query.QueryManager;
import javax.jcr.query.QueryResult;
import org.komodo.core.KomodoLexicon;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.spi.query.sql.SQLConstants;
import org.komodo.utils.KLog;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.jcr.api.Session;
import org.modeshape.sequencer.ddl.StandardDdlLexicon;
import org.modeshape.sequencer.ddl.dialect.teiid.TeiidDdlLexicon;
import org.modeshape.sequencer.teiid.lexicon.VdbLexicon;


/**
 * Sequencers class responsible for executing all the sequencers in
 * consecutive, synchronous order.
 */
public class KSequencers implements SQLConstants {

    /**
     * The Sequencers executed by {@link KSequencers}
     */
    public static enum Sequencers {
        /**
         * VDB Sequencer
         */
        VDB("VDB Dynamic Sequencer"), //$NON-NLS-1$

        /**
         * DDL Sequencer
         */
        DDL("DDL Sequencer"), //$NON-NLS-1$

        /**
         * Teiid SQL Sequencer
         */
        TSQL("Teiid SQL Sequencer"); //$NON-NLS-1$

        private String id;

        private Sequencers(String id) {
            this.id = id;
        }

        @Override
        public String toString() {
            return id;
        }
    }

    private static KSequencers instance;

    private static final String SELECT_FROM = "SELECT [jcr:path] FROM "; //$NON-NLS-1$

    /**
     * @return singleton instance
     */
    public static KSequencers getInstance() {
        if (instance == null)
            instance = new KSequencers();

        return instance;
    }

    /**
     * @param node node to test
     * @return true if this is a vdb node
     * @throws RepositoryException if error occurs
     */
    public static boolean isVdbNode(Node node) throws RepositoryException {
        return node.getPrimaryNodeType().getName().equals(VdbLexicon.Vdb.VIRTUAL_DATABASE);
    }

    private String queryText(String nodeType, String property, String whereOperand) {
        StringBuffer buffer = new StringBuffer(SELECT_FROM)
                    .append(OPEN_SQUARE_BRACKET + nodeType + CLOSE_SQUARE_BRACKET);

        if (property != null && whereOperand != null) {
            buffer.append(SPACE + WHERE + SPACE)
                     .append(OPEN_SQUARE_BRACKET + property + CLOSE_SQUARE_BRACKET)
                     .append(SPACE + whereOperand);
        }

        return buffer.toString();
    }

    private NodeIterator query(Session session, String queryText) throws RepositoryException {
        QueryManager queryMgr = session.getWorkspace().getQueryManager();
        Query query = queryMgr.createQuery(queryText, Query.JCR_SQL2);
        QueryResult resultSet = query.execute();
        return resultSet.getNodes();
    }

    private boolean isDdlSequenced(Node ddlParentNode) throws Exception {
        boolean alreadySequenced = false;
        NodeIterator children = ddlParentNode.getNodes();
        while(children.hasNext()) {
            Node child = children.nextNode();
            if (child.hasProperty(StandardDdlLexicon.DDL_EXPRESSION)) {
                alreadySequenced = true;
                break;
            }
        }
        return alreadySequenced;
    }

    private boolean isTSqlSequenced(Node tsqlParentNode) throws RepositoryException {
        boolean alreadySequenced = false;
        NodeIterator children = tsqlParentNode.getNodes();
        while(children.hasNext()) {
            Node child = children.nextNode();
            if (child.hasProperty(TeiidSqlLexicon.LanguageObject.TEIID_VERSION_PROP_NAME)) {
                alreadySequenced = true;
                break;
            }
        }
        return alreadySequenced;
    }

    private Property jcrDataProperty(Node node) throws RepositoryException {
        if (! node.hasNode(JcrConstants.JCR_CONTENT))
            return null;

        Node contentNode = node.getNode(JcrConstants.JCR_CONTENT);
        if (! contentNode.hasProperty(JcrConstants.JCR_DATA))
            return null;

        return contentNode.getProperty(JcrConstants.JCR_DATA);
    }

    private boolean execDdlSequencer(Session session, String nodeType, String nodeProperty) throws RepositoryException {
        boolean executeStatus = false;
        String queryText = queryText(nodeType, nodeProperty, "IS NOT NULL"); //$NON-NLS-1$

        NodeIterator iterator = query(session, queryText);
        while (iterator.hasNext()) {
            Node node = iterator.nextNode();

            try {
                if (isDdlSequenced(node))
                    continue;

                if (!node.hasProperty(nodeProperty))
                    continue;

                Property stmtProperty = node.getProperty(nodeProperty);
                boolean status = session.sequence(Sequencers.DDL.toString(), stmtProperty, node);
                if (status) {
                    session.save();
                    executeStatus = status;
                }

            } catch (Exception ex) {
                // Sequencing has failed for this node
                KLog.getLogger().error("Ddl Sequencer failed for node " + node.getPath(), ex); //$NON-NLS-1$
            }
        }

        return executeStatus;
    }

    private boolean execTSqlSequencer(Session session, String nodeType, String nodeProperty) throws RepositoryException {
        boolean executeStatus = false;
        String queryText = queryText(nodeType, nodeProperty, "IS NOT NULL"); //$NON-NLS-1$

        NodeIterator iterator = query(session, queryText);
        while (iterator.hasNext()) {
            Node node = iterator.nextNode();

            try {
                if (isTSqlSequenced(node))
                    continue;

                if (!node.hasProperty(nodeProperty))
                    continue;

                Property stmtProperty = node.getProperty(nodeProperty);

                boolean status = session.sequence(Sequencers.TSQL.toString(), stmtProperty, node);
                if (status) {
                    session.save();
                    executeStatus = status;
                }

            } catch (Exception ex) {
                // Sequencing has failed for this node
                KLog.getLogger().error("TSql Sequencer failed for node " + node.getPath(), ex); //$NON-NLS-1$
            }
        }

        return executeStatus;
    }

    private boolean ddlSequencer(Session session) throws RepositoryException {
        boolean executeStatus = false;

        //
        // ":(//*)[@vdb:modelDefinition] => /$1"
        //
        boolean status = execDdlSequencer(session,
                                                    VdbLexicon.Vdb.DECLARATIVE_MODEL,
                                                    VdbLexicon.Model.MODEL_DEFINITION);
        executeStatus = status ? status : executeStatus;

        //
        // ":(//*)[@tko:rendition] => /$1"
        //
        status = execDdlSequencer(session,
                                                          KomodoLexicon.Schema.NODE_TYPE,
                                                          KomodoLexicon.Schema.RENDITION);
        executeStatus = status ? status : executeStatus;

        return executeStatus;
    }

    private boolean tsqlSequencer(Session session) throws RepositoryException {
        boolean executeStatus = false;

        //
        // ":(//*)[@teiidddl:queryExpression] => /$1",
        //
        boolean status = execTSqlSequencer(session,
                                                          TeiidDdlLexicon.CreateTable.TABLE_STATEMENT,
                                                          TeiidDdlLexicon.CreateTable.QUERY_EXPRESSION);
        executeStatus = status ? status : executeStatus;

        status = execTSqlSequencer(session,
                                                          TeiidDdlLexicon.CreateTable.VIEW_STATEMENT,
                                                          TeiidDdlLexicon.CreateTable.QUERY_EXPRESSION);
        executeStatus = status ? status : executeStatus;

        //
        // ":(//*)[@teiidddl:statement] => /$1"
        //
        status = execTSqlSequencer(session,
                                              TeiidDdlLexicon.CreateProcedure.PROCEDURE_STATEMENT,
                                              TeiidDdlLexicon.CreateProcedure.STATEMENT);
        executeStatus = status ? status : executeStatus;

        return executeStatus;
    }

    private boolean vdbSequencer(Session session) throws RepositoryException {
        boolean executeStatus = false;

        String queryText = queryText(VdbLexicon.Vdb.VIRTUAL_DATABASE, null, null);

        NodeIterator iterator = query(session, queryText);
        while (iterator.hasNext()) {
            Node vdbNode = iterator.nextNode();

            try {
                if (! isVdbNode(vdbNode))
                    return false;

                Property dataProperty = jcrDataProperty(vdbNode);
                if (dataProperty == null)
                    return false;

                boolean status = session.sequence(Sequencers.VDB.toString(), dataProperty, vdbNode);
                if (status) {
                    session.save();
                    executeStatus = status;
                }

            } catch (Exception ex) {
                // Sequencing has failed for this node
                KLog.getLogger().error("Vdb Sequencer failed for node " + vdbNode.getPath(), ex); //$NON-NLS-1$
            }
        }

        return executeStatus;
    }

    /**
     * Execute the sequencers synchronously and consecutively
     *
     * @param session the session to sequence
     */
    public void sequence(javax.jcr.Session session) {
        if (! (session instanceof Session))
            return;

        try {
            vdbSequencer((Session) session);
        } catch (RepositoryException ex) {
            KLog.getLogger().error("Vdb Sequencer repository exception", ex); //$NON-NLS-1$
        }

        try {
            ddlSequencer((Session)session);
        } catch (RepositoryException ex) {
            KLog.getLogger().error("Ddl Sequencer repository exception", ex); //$NON-NLS-1$
        }

        try {
            tsqlSequencer((Session)session);
        } catch (RepositoryException ex) {
            KLog.getLogger().error("TSql Sequencer repository exception", ex); //$NON-NLS-1$
        }
    }
}
