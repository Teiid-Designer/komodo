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
package org.komodo.repository.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.RepositoryException;
import javax.jcr.nodetype.NodeType;
import org.komodo.repository.Messages;
import org.komodo.spi.KException;
import org.komodo.spi.constants.StringConstants;
import org.komodo.utils.KLog;
import org.modeshape.jcr.JcrRepository;
import org.modeshape.jcr.ModeShapeEngine;
import org.modeshape.jcr.api.Session;

/**
 *
 */
public class ModeshapeUtils implements StringConstants {

    private ModeshapeUtils() {}

    /**
     * @param engine the engine to check
     *
     * @return true if engine is running
     */
    public static boolean isEngineRunning(ModeShapeEngine engine) {
        if (engine == null)
            return false;

        return ModeShapeEngine.State.RUNNING.equals(engine.getState());
    }

    /**
     * @param repository the repository to check
     *
     * @return true if repository is running
     */
    public static boolean isRepositoryRunning(JcrRepository repository) {
        if (repository == null)
            return false;

        return ModeShapeEngine.State.RUNNING.equals(repository.getState());
    }

    /**
     * @param identifier identifier of the workspace
     *
     * @return a new session for the given repository
     * @throws Exception if an error occurs
     */
    public static Session createSession(WorkspaceIdentifier identifier) throws Exception {
        if (identifier.getRepository() == null || ! isRepositoryRunning(identifier.getRepository())) {
            throw new KException(Messages.getString(Messages.LocalRepository.Repository_Not_Running));
        }

        // the workspace name must agree with the config file
        final Session session = identifier.getRepository().login(null, identifier.getWorkspace());
        KLog.getLogger().debug("ModeShapeUtils.createSession: {0}", session.hashCode()); //$NON-NLS-1$
        return session;
    }

    /**
     * @param node the node
     *
     * @return the primary and mixin node types of the node
     * @throws RepositoryException if error occurs
     */
    public static List<NodeType> getAllNodeTypes(Node node) throws RepositoryException {
        List<NodeType> nodeTypes = new ArrayList<NodeType>();
        nodeTypes.add(node.getPrimaryNodeType());
        nodeTypes.addAll(Arrays.asList(node.getMixinNodeTypes()));
        return nodeTypes;
    }

    /**
     * @param node the node
     * @param namespace the type namespace
     * @return true if the node has any types with the given namespace
     * @throws RepositoryException if error occurs
     */
    public static boolean hasTypeNamespace(Node node, String namespace) throws RepositoryException {
        for (String name : getAllNodeTypeNames(node)) {
            if (name.startsWith(namespace + COLON))
                return true;
        }

        return false;
    }

    /**
     * @param node the node
     *
     * @return the primary and mixin node types of the node
     * @throws RepositoryException if error occurs
     */
    public static List<String> getAllNodeTypeNames(Node node) throws RepositoryException {
        List<String> nodeTypes = new ArrayList<String>();

        nodeTypes.add(node.getPrimaryNodeType().getName());
        for (NodeType nodeType : node.getMixinNodeTypes()) {
            nodeTypes.add(nodeType.getName());
        }

        return nodeTypes;
    }

    /**
     * @param node the node
     *
     * @return number of children belonging to this node
     * @throws RepositoryException if error occurs
     */
    public static int childrenCount(Node node) throws RepositoryException {
        NodeIterator iterator = node.getNodes();
        int count = 0;

        while (iterator.hasNext()) {
            count++;
            iterator.next();
        }

        return count;
    }

}
