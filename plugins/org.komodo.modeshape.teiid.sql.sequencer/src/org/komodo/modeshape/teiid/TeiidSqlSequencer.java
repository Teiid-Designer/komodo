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
package org.komodo.modeshape.teiid;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import javax.jcr.Binary;
import javax.jcr.NamespaceRegistry;
import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import javax.jcr.Value;
import javax.jcr.ValueFactory;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.QueryParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion;
import org.komodo.spi.runtime.version.TeiidVersion.Version;
import org.komodo.utils.KLog;
import org.modeshape.common.annotation.NotThreadSafe;
import org.modeshape.common.text.ParsingException;
import org.modeshape.common.util.CheckArg;
import org.modeshape.common.util.IoUtil;
import org.modeshape.jcr.api.JcrConstants;
import org.modeshape.jcr.api.Session;
import org.modeshape.jcr.api.nodetype.NodeTypeManager;
import org.modeshape.jcr.api.sequencer.Sequencer;


/**
 * A sequencer for Teiid SQL files.
 */
@NotThreadSafe
public class TeiidSqlSequencer extends Sequencer {

    private static ITeiidVersion DEFAULT_TEIID_VERSION = Version.TEIID_8_7.get();

    private static final KLog LOGGER = KLog.getLogger();

    private ITeiidVersion teiidVersion = DEFAULT_TEIID_VERSION;

    /**
     * @return the teiidVersion
     */
    public ITeiidVersion getTeiidVersion() {
        return this.teiidVersion;
    }

    /**
     * @param teiidVersion the teiidVersion to set
     */
    public void setTeiidVersion(ITeiidVersion teiidVersion) {
        this.teiidVersion = teiidVersion;
    }

    /**
     * Create a query parser for parsing the SQL string
     *
     * @return instance of {@link QueryParser} for the provided teiid version
     */
    protected QueryParser createParser() {
        QueryParser queryParser = new QueryParser(getTeiidVersion());
        return queryParser;
    }

    @Override
    public void initialize(NamespaceRegistry registry, NodeTypeManager nodeTypeManager) throws RepositoryException, IOException {
        registerNodeTypes("cnd/TeiidSql.cnd", nodeTypeManager, true); //$NON-NLS-1$
    }

    @Override
    public boolean execute(Property inputProperty, Node outputNode, Context context) throws Exception {
        Binary sqlContent = inputProperty.getBinary();
        CheckArg.isNotNull(sqlContent, "teiid sql content binary value"); //$NON-NLS-1$

        if (outputNode.hasProperty(TeiidSqlLexicon.TEIID_VERSION_PROPERTY)) {
            Property versionProp = outputNode.getProperty(TeiidSqlLexicon.TEIID_VERSION_PROPERTY);
            setTeiidVersion(new TeiidVersion(versionProp.getString()));
        }

        // Perform the parsing
        final ASTNode rootNode;
        QueryParser parser = createParser();
        InputStream stream = sqlContent.getStream();
        try {
            String sql = IoUtil.read(stream);
            rootNode = parser.parseDesignerCommand(sql);
        } catch (ParsingException e) {
            LOGGER.error(Messages.getString(Messages.TeiidSqlSequencer.ErrorParsingContent), e, e.getLocalizedMessage());
            return false;
        } catch (IOException e) {
            LOGGER.error(Messages.getString(Messages.TeiidSqlSequencer.ErrorSequencingContent), e, e.getLocalizedMessage());
            return false;
        } finally {
            stream.close();
        }

        convertASTNode(rootNode, outputNode);
        return true;
    }

    /**
     * Converts the given {@link ASTNode} to a jcr {@link Node} and appends
     * it to the given output node.
     *
     * @param rootNode ast node to be converted
     * @param outputNode node to have results appended to
     * @throws RepositoryException if conversion is invalid
     */
    public void convertASTNode(final ASTNode rootNode, Node outputNode) throws RepositoryException {
        Queue<ASTNode> queue = new LinkedList<ASTNode>();
        queue.add(rootNode);

        while (queue.peek() != null) {
            ASTNode astNode = queue.poll();
            Node sequenceNode = createFromASTNode(outputNode, astNode);
            appendNodeProperties(astNode, sequenceNode);

            // Add the children to the queue ...
            Iterator<ASTNode> childIter = astNode.getChildren();
            while(childIter.hasNext()) {
                queue.add(childIter.next());
            }
        }
    }

    private void appendNodeProperties(ASTNode astNode, Node sequenceNode) throws RepositoryException {
        ValueFactory valueFactory = sequenceNode.getSession().getValueFactory();

        for (String propertyName : astNode.getPropertyNames()) {
            Object astNodePropertyValue = astNode.getProperty(propertyName);
            if (astNodePropertyValue == null) {
                sequenceNode.setProperty(propertyName, (Value) null);
                continue;
            }

            List<Value> valuesList = convertToPropertyValues(astNodePropertyValue, valueFactory);
            if (valuesList.size() == 1) {
                sequenceNode.setProperty(propertyName, valuesList.get(0));
            } else {
                sequenceNode.setProperty(propertyName, valuesList.toArray(new Value[0]));
            }
        }
    }

    private Node createFromASTNode(Node parent, ASTNode astNode) throws RepositoryException {
        String relativePath = astNode.getAbsolutePath().substring(1);
        Node sequenceNode = null;

        // for SNS the absolute path will use first node it finds as the parent so find real parent if possible
        Node parentNode = getNode(astNode.getParent());

        if (parentNode == null) {
            sequenceNode = parent.addNode(relativePath, astNode.getPrimaryType());
        } else {
            final Session session = (Session)parentNode.getSession();
            String jcrName = astNode.astIdentifier();
            // if first character is a '{' then the name is prefixed by the namespace URL
            if ((jcrName.charAt(0) == '{') && (jcrName.indexOf('}') != -1)) {
                final int index = jcrName.indexOf('}');
                String localName = jcrName.substring(index + 1);
                localName = session.encode(localName);

                jcrName = jcrName.substring(0, (index + 1)) + localName;
            } else {
                jcrName = session.encode(jcrName);
            }

            sequenceNode = parentNode.addNode(jcrName, astNode.getPrimaryType());
        }

        astNode.setSequencedNode(sequenceNode);
        for (String mixin : astNode.getMixins()) {
            sequenceNode.addMixin(mixin);
        }
        astNode.removeProperty(JcrConstants.JCR_MIXIN_TYPES);
        astNode.removeProperty(JcrConstants.JCR_PRIMARY_TYPE);
        return sequenceNode;
    }

    private List<Value> convertToPropertyValues(Object objectValue, ValueFactory valueFactory) throws RepositoryException {
        List<Value> result = new ArrayList<Value>();
        if (objectValue instanceof Collection) {
            Collection<?> objects = (Collection<?>)objectValue;
            for (Object childObjectValue : objects) {
                List<Value> childValues = convertToPropertyValues(childObjectValue, valueFactory);
                result.addAll(childValues);
            }
        } else if (objectValue instanceof Boolean) {
            result.add(valueFactory.createValue((Boolean)objectValue));
        } else if (objectValue instanceof Integer) {
            result.add(valueFactory.createValue((Integer)objectValue));
        } else if (objectValue instanceof Long) {
            result.add(valueFactory.createValue((Long)objectValue));
        } else if (objectValue instanceof Double) {
            result.add(valueFactory.createValue((Double)objectValue));
        } else if (objectValue instanceof Float) {
            result.add(valueFactory.createValue((Float)objectValue));
        } else if (objectValue instanceof ASTNode) {
            result.add(valueFactory.createValue(getNode((ASTNode)objectValue)));
        } else {
            result.add(valueFactory.createValue(objectValue.toString()));
        }
        return result;
    }

    private Node getNode(final ASTNode node) {
        if (node == null)
            return null;

        Node sequencedNode = node.getSequencedNode();
        return sequencedNode;
    }
}
