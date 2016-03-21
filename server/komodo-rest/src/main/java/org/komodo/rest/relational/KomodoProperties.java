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
package org.komodo.rest.relational;

import java.util.HashMap;

/**
 * Convenience properties object that provides key as a {@link String}
 */
public class KomodoProperties extends HashMap<String, Object> {

    /**
     *
     */
    private static final long serialVersionUID = 1L;

    /**
     * @param key the property key
     * @param value the property value
     */
    public void addProperty(String key, Object value) {
        this.put(key, value);
    }

    /**
     * @param key the property key
     * @return the value for the given property key
     */
    public Object getProperty(String key) {
        return this.get(key);
    }

    /**
     * @param key the property key
     * @param defaultValue the default value if no property has been set
     * @return the value of the property or the default value
     */
    @SuppressWarnings( "unchecked" )
    public <T> T getProperty(String key, T defaultValue) {
        if (! this.containsKey(key))
            return defaultValue;

        return (T) this.get(key);
    }
}
