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
 * but WITHOUClass<?>ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.modeshape.teiid.scripts;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.komodo.modeshape.teiid.parser.TeiidSQLConstants;
import org.komodo.spi.constants.StringConstants;

/**
 * Implementation of a tree of a set of java classes and interfaces
 */
@SuppressWarnings( "nls" )
public class CTree implements StringConstants {

    /**
     * Callback for executing a function on all nodes in the tree
     */
    public interface CTreeCallback {

        /**
         * @param node
         * @throws Exception
         */
        void run(Node node) throws Exception;

    }

    /**
     *
     * @param <T>
     */
    public static abstract class Node<T extends Node> implements StringConstants {

        protected final CTree tree;

        private final Class<?> klazz;

        private final Map<Class<?>, T> parents = new LinkedHashMap<Class<?>, T>();

        private final Map<Class<?>, T> children = new LinkedHashMap<Class<?>, T>();

        /**
         * @param tree 
         * @param klazz
         */
        public Node(CTree tree, Class<?> klazz) {
            this.tree = tree;
            this.klazz = klazz;
        }

        /**
         * @return tree
         */
        public CTree getTree() {
            return tree;
        }

        /**
         * @return the klazz
         */
        public Class<?> klazz() {
            return this.klazz;
        }

        /**
         * @param objClass
         * @return this or its children contain the given class
         */
        public boolean containsClass(Class<?> objClass) {
            if (klazz().equals(objClass))
                return true;

            for (T node : getChildren()) {
                if (node.containsClass(objClass))
                    return true;
            }

            return false;
        }

        /**
         * @return the parent
         */
        public Collection<T> getParents() {
            return this.parents.values();
        }

        /**
         * @param parent the parent to set
         */
        protected void addParent(T parent) {
            this.parents.put(parent.klazz(), parent);
        }

        /**
         * @return the children
         */
        public Collection<T> getChildren() {
            return this.children.values();
        }

        /**
         * @param klazz
         * @return child node with class of klazz or null
         */
        public T getChild(Class<?> klazz) {
            return children.get(klazz);
        }

        /**
         * @param child
         * @return child or the existing child
         */
        public T addChild(T child) {
            T node = children.get(child.klazz());
            if (node == null) {
                children.put(child.klazz(), child);
                child.addParent(this);
                node = child;
            }

            return node;
        }

        protected String toString(String prefix) {
            StringBuffer buffer = new StringBuffer();
            buffer.append(klazz.toString());
            buffer.append(NEW_LINE);

            prefix = TAB + prefix;
            for (T child : getChildren()) {
                buffer.append(prefix);
                buffer.append(child.toString(prefix));
            }

            return buffer.toString();
        }

        @Override
        public String toString() {
            return toString("|-" + TAB);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((this.klazz == null) ? 0 : this.klazz.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Node other = (Node)obj;
            if (this.klazz == null) {
                if (other.klazz != null)
                    return false;
            } else if (!this.klazz.equals(other.klazz))
                return false;
            return true;
        }

        /**
         * @param callback
         * @throws Exception
         */
        public void execute(CTreeCallback callback) throws Exception {
            callback.run(this);

            for (Node childNode : getChildren()) {
                childNode.execute(callback);
            }
        }
    }

    /**
     * Interface Node
     */
    public static class INode extends Node<INode> {

        /**
         * @param tree
         * @param klazz
         */
        public INode(CTree tree, Class<?> klazz) {
            super(tree, klazz);
        }

        /**
         * @param iNode
         * @return this node is an ancestor of the given node
         */
        public boolean isAssignable(INode iNode) {
            if (this.equals(iNode))
                return true;

            for (INode parent : iNode.getParents()) {
                if (isAssignable(parent))
                    return true;
            }

            return false;
        }
    }

    /**
     * Class Node
     */
    public static class CNode extends Node<CNode> {

        private List<INode> classInterfaces = new ArrayList<INode>();

        /**
         * @param tree
         * @param klazz
         */
        public CNode(CTree tree, Class<?> klazz) {
            super(tree, klazz);
        }

        /**
         * @return parents of this node
         */
        public CNode getParent() {
            Iterator<CNode> iterator = getParents().iterator();
            if (iterator.hasNext())
                return iterator.next();

            return null;
        }

        /**
         * @return the classInterfaces
         */
        public List<INode> getClassInterfaces() {
            return this.classInterfaces;
        }

        /**
         * @param iNode
         */
        public void addInterface(INode iNode) {
            classInterfaces.add(iNode);
        }

        @Override
        protected String toString(String prefix) {
            StringBuffer buffer = new StringBuffer();
            buffer.append(klazz().toString());

            if (!classInterfaces.isEmpty()) {
                buffer.append(TAB);
                buffer.append(CLOSE_ANGLE_BRACKET);
                buffer.append(TAB);

                for (int i = 0; i < classInterfaces.size(); ++i) {
                    buffer.append(classInterfaces.get(i).klazz());
                    if (i < (classInterfaces.size() - 1)) {
                        buffer.append(COMMA);
                        buffer.append(SPACE);
                    }
                }
            }

            buffer.append(NEW_LINE);

            prefix = TAB + prefix;
            for (CNode child : getChildren()) {
                buffer.append(prefix);
                buffer.append(child.toString(prefix));
            }

            return buffer.toString();
        }
    }

    private CNode root;

    private Map<Class<?>, INode> interfaceNodes = new HashMap<Class<?>, INode>();

    private Set<Class<?>> classes = new HashSet<Class<?>>();

    /**
     * @param rootClass
     */
    public CTree(Class<?> rootClass) {
        root = new CNode(this, rootClass);
        analyseInterfaces(root);
    }

    private void analyseInterfaces(CNode cNode) {
        for (Class<?> iface : cNode.klazz().getInterfaces()) {
            INode iNode = addInterface(iface);
            
            if (iNode != null)
                cNode.addInterface(iNode);
        }
    }

    /**
     * @return the root
     */
    public CNode getRoot() {
        return this.root;
    }

    /**
     * @param klazz
     * @return whether klazz is root's klazz
     */
    public boolean isRootData(Class<?> klazz) {
        return getRoot().klazz().equals(klazz);
    }

    @Override
    public String toString() {
        StringBuffer buffer = new StringBuffer();
        for (INode iNode : interfaceNodes.values()) {
            buffer.append(iNode.toString());
        }

        buffer.append(NEW_LINE);
        buffer.append(getRoot().toString());

        return buffer.toString();
    }

    /**
     * @return the interfaces
     */
    public Collection<INode> getInterfaceNodes() {
        return this.interfaceNodes.values();
    }


    /**
     * @param objClass
     * @return whether class is part of this tree
     */
    public boolean containsClass(Class<?> objClass) {
        for (INode iNode : interfaceNodes.values()) {
            if (iNode.containsClass(objClass))
                return true;
        }

        return getRoot().containsClass(objClass);
    }

    /**
     * @return set of all classes registered to this tree
     */
    public Set<Class<?>>getRegisteredClasses() {
        return classes;
    }

    /**
     * @param objClass
     * @return is the given class part of the SPI rather than a language object
     */
    public boolean isSPILanguageInterface(Class<?> objClass) {
        if (objClass == null || objClass.getPackage() == null || objClass.getPackage().getName() == null)
            return false;

        if (objClass.isEnum())
            return false;

        return objClass.getPackage().getName().contains("spi.query.sql");
    }

    private boolean isSQLInterface(Class<?> objClass) {
        String pkgName = objClass.getPackage().getName();
        if (! pkgName.contains(KOMODO))
            return false;

        if (StringConstants.class.equals(objClass))
            // Ignore StringConstants
            return false;

        if (TeiidSQLConstants.Reserved.class.equals(objClass) ||
                TeiidSQLConstants.NonReserved.class.equals(objClass) ||
                TeiidSQLConstants.Tokens.class.equals(objClass))
            // Ignore SQLConstants interfaces
            return false;

        // Ignore spi interfaces
        return ! isSPILanguageInterface(objClass);
    }

    private INode addInterface(Class<?> iface) {
        if (!iface.isInterface())
            throw new RuntimeException("Adding an interface that's not an interface!");

        if (! isSQLInterface(iface))
            return null;

        // Need to add the interface but first add all its parents
        Collection<INode> parentIfaces = new HashSet<INode>();
        for (Class<?> parentIface : iface.getInterfaces()) {
            INode pINode = addInterface(parentIface);
            if (pINode == null)
                continue;

            parentIfaces.add(pINode);
        }

        INode iNode = null;
        if (parentIfaces.isEmpty()) {
            iNode = interfaceNodes.get(iface);
            if (iNode == null) {
                iNode = new INode(this, iface);
                interfaceNodes.put(iface, iNode);
            }
        } else {
            for (INode pINode : parentIfaces) {
                iNode = pINode.getChild(iface);
                if (iNode == null) {
                    iNode = new INode(this, iface);
                    pINode.addChild(iNode);
                }
            }
        }
        return iNode;
    }

    private CNode addClass(Class<?> objClass) {
        CNode parentNode;
        if (isRootData(objClass))
            return getRoot();

        // Index parents prior to children
        Class<?> parentClass = objClass.getSuperclass();
        parentNode = addClass(parentClass);

        CNode cNode = new CNode(this, objClass);
        analyseInterfaces(cNode);

        return parentNode.addChild(cNode);
    }

    /**
     * @param objClass
     */
    public void add(Class<?> objClass) {
        if (objClass == null)
            throw new RuntimeException("Object class should never be null!");

        if (objClass.isInterface())
            addInterface(objClass);
        else
            addClass(objClass);

        classes.add(objClass);
    }

    /**
     * Perform a function callback on all nodes in this tree
     *
     * @param callback
     * @throws Exception
     */
    public void execute(CTreeCallback callback) throws Exception {
        for (Node node : getInterfaceNodes()) {
            node.execute(callback);
        }

        for (Node node : root.getChildren()) {
            node.execute(callback);
        }
    }
}
