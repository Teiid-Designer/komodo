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
package org.komodo.spi.storage;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.komodo.spi.constants.StringConstants;

/**
 *
 * @param <T>
 */
public abstract class StorageParent<T> implements StringConstants {

    private final StorageParent<T> parent;

    private List<StorageNode<T>> children;

    /**
     * @param parent
     */
    public StorageParent(StorageParent<T> parent) {
        this.parent = parent;
    }

    /**
     * @return parent
     */
    public StorageParent<T> getParent() {
        return parent;
    }

    /**
     * @return true if this has children, false otherwise
     */
    public boolean hasChildren() {
        return children != null && ! children.isEmpty();
    }

    /**
     * @return children
     */
    public List<StorageNode<T>> getChildren() {
        if (children == null)
            return Collections.emptyList();

        return Collections.unmodifiableList(children);
    }

    /**
     * @param data
     * @return new storage node added as child with the given data
     */
    public StorageNode<T> addChild(T data) {
        if (children == null)
            children = new ArrayList<>();

        StorageNode<T> child = new StorageNode<T>(this, data);
        children.add(child);
        return child;
    }

    public abstract String getPath();

    public String printTree() {
        StringBuffer buf = new StringBuffer();

        buf.append(toString()).append(NEW_LINE);
        for (StorageNode<T> child : getChildren())
            buf.append(child.printTree());
        
        return buf.toString();
    }

    @Override
    public String toString() {
        return getPath();
    }
}
