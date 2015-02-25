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
package org.komodo.relational;

import org.komodo.spi.utils.KeyInValueHashMap;
import org.komodo.spi.utils.KeyInValueHashMap.KeyFromValueAdapter;

/**
 *
 */
public class RelationalProperties {

    private KeyFromValueAdapter<String, RelationalProperty> adapter = new KeyFromValueAdapter<String, RelationalProperty>() {

        @Override
        public String getKey(RelationalProperty value) {
            return value.getName();
        }
    };

    private KeyInValueHashMap<String, RelationalProperty> properties =
            new KeyInValueHashMap<String, RelationalProperty>(adapter);

    /**
     * Adds a {@link RelationalProperty}
     *
     * @param property the new property
     */
    public void add(RelationalProperty property) {
        properties.add(property);
    }

    /**
     * @param name the name of {@link RelationalProperty}
     * @return value associated with this name or <code>null</code> if no property
     */
    public Object getValue(String name) {
        RelationalProperty property = properties.get(name);
        return property != null ? property.getValue() : null;        
    }
}
