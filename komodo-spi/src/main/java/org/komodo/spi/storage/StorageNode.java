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

/**
 * @param <T>
 */
public class StorageNode<T> extends StorageParent<T> {

    private final T data;

    /**
     * @param parent
     * @param data
     */
    public StorageNode(StorageParent<T> parent, T data) {
        super(parent);
        this.data = data;
    }

    /**
     * @return the data
     */
    public T getData() {
        return data;
    }

    /**
     * @return canonical path
     */
    @Override
    public String getPath() {
        StringBuffer buf = new StringBuffer();
        StorageParent<T> parent = getParent();
        String path = parent.getPath();

        buf.append(path);

        if (! path.endsWith(FORWARD_SLASH))
              buf.append(FORWARD_SLASH);

        buf.append(getData());

        return buf.toString();
    }

    @Override
    public String toString() {
        return getPath();
    }
}
