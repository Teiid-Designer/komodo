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

package org.komodo.modeshape.teiid.sql.lang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.runtime.version.ITeiidVersion;
import org.komodo.spi.type.IDataTypeManagerService;

public class SimpleNode implements Node, LanguageObject {

    protected Node parent;
    protected Node[] children;
    protected int id;
    protected Object value;
    protected TeiidParser parser;

    public SimpleNode(TeiidParser p, int i) {
        id = i;
        parser = p;
    }

    @Override
    public TeiidParser getTeiidParser() {
        return parser;
    }

    /**
     * @return the id
     */
    public int getId() {
        return this.getId();
    }

    @Override
    public ITeiidVersion getTeiidVersion() {
        return parser.getVersion();
    }

    public IDataTypeManagerService getDataTypeService() {
        return parser.getDataTypeService();
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public void jjtOpen() {
    }

    @Override
    public void jjtClose() {
    }

    @Override
    public void jjtSetParent(Node n) {
        parent = n;
    }

    @Override
    public Node jjtGetParent() {
        return parent;
    }

    @Override
    public void jjtAddChild(Node n, int i) {
        if (children == null) {
            children = new Node[i + 1];
        } else if (i >= children.length) {
            Node c[] = new Node[i + 1];
            System.arraycopy(children, 0, c, 0, children.length);
            children = c;
        }
        children[i] = n;
    }

    @Override
    public Node jjtGetChild(int i) {
        return children[i];
    }

    @Override
    public int jjtGetNumChildren() {
        return (children == null) ? 0 : children.length;
    }

    public void jjtSetValue(Object value) {
        this.value = value;
    }

    public Object jjtGetValue() {
        return value;
    }

    @Override
    public SimpleNode clone() {
        SimpleNode clone = new SimpleNode(this.getTeiidParser(), this.getId());
        return clone;
    }

    protected <T extends LanguageObject> Collection<T> cloneCollection(Collection<T> collection) {
        if (collection == null)
            return null;

        Collection<T> cloned = new HashSet<T>();
        for (T item : collection) {
            cloned.add((T) item.clone());
        }

        return cloned;
    }

    protected <T extends LanguageObject> List<T> cloneList(List<T> list) {
        if (list == null)
            return null;

        List<T> cloned = new ArrayList<T>();
        for (T item : list) {
            cloned.add((T) item.clone());
        }

        return cloned;
    }

    /* You can override these two methods in subclasses of SimpleNode to
       customize the way the node appears when the tree is dumped.  If
       your output uses more than one line you should override
       toString(String), otherwise overriding toString() is probably all
       you need to do. */

    @Override
    public String toString() {
        return Teiid8ParserTreeConstants.jjtNodeName[id];
    }

    public String toString(String prefix) {
        return prefix + toString();
    }

    /* Override this method if you want to customize how the node dumps
       out its children. */

    public void dump(String prefix) {
        System.out.println(toString(prefix));
        if (children != null) {
            for (int i = 0; i < children.length; ++i) {
                SimpleNode n = (SimpleNode)children[i];
                if (n != null) {
                    n.dump(prefix + StringConstants.SPACE);
                }
            }
        }
    }
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + this.getId();
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        SimpleNode other = (SimpleNode)obj;
        if (this.getId() != other.getId()) return false;
        return true;
    }

}
