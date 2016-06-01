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
package org.komodo.spi.utils;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * A {@link HashMap} which stores values that also reference their
 * own keys. More memory efficient than storing separate references
 * to the keys and values.
 *
 * @param <K> key
 * @param <V> value
 */
@SuppressWarnings( "unchecked" )
public class KeyInValueHashMap<K, V> extends AbstractMap<K, V> {

    /**
     * Adapter interface that clients of the class should implement
     * and pass to its constructor so that the key (K) can be derived
     * from the value (V).
     *
     * @param <K> key
     * @param <V> value
     */
    public interface KeyFromValueAdapter<K, V> {

        /**
         * Get the key from the value
         *
         * @param value
         *
         * @return key (K) from the value (V)
         */
        K getKey(V value);

    }

    private class EntryWrapper implements Map.Entry<K, V> {

        V value;

        /**
         * @param value the Value
         */
        public EntryWrapper(V value) {
            this.value = value;
        }

        @Override
        public K getKey() {
            return (K) adapter.getKey(value);
        }

        @Override
        public V getValue() {
            return value;
        }

        @Override
        public V setValue(V value) {
            throw new UnsupportedOperationException();
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((this.value == null) ? 0 : this.value.hashCode());
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
            EntryWrapper other = (EntryWrapper)obj;
            if (!getOuterType().equals(other.getOuterType()))
                return false;
            if (this.value == null) {
                if (other.value != null)
                    return false;
            } else if (!this.value.equals(other.value))
                return false;
            return true;
        }

        private KeyInValueHashMap<K, V> getOuterType() {
            return KeyInValueHashMap.this;
        }
    }

    private Set<Map.Entry<K, V>> entrySet = new HashSet<Map.Entry<K, V>>();

    private KeyFromValueAdapter<K, V> adapter;

    /**
     * Create a new instance
     *
     * @param adapter that can convert from the value into the key
     */
    public KeyInValueHashMap(KeyFromValueAdapter<K, V> adapter) {
        this.adapter = adapter;
    }

    @Override
    public V put(K key, V value) {
        throw new UnsupportedOperationException("Use add rather than put since the key is part of the value"); //$NON-NLS-1$
    }

    /**
     * Add a value to this map where its key will be derived
     * by the {@link KeyFromValueAdapter}
     *
     * @param value the Value
     *
     * @return true if the value was added.
     */
    public boolean add(V value) {
        EntryWrapper entry = new EntryWrapper(value);
        return entrySet.add(entry);
    }

    /**
     * Remove a value from this map where the key will
     * be determined by the {@link KeyFromValueAdapter}
     *
     * @param value the Value
     *
     * @return removed value or null.
     */
    @Override
    public V remove(Object value) {
        EntryWrapper entry = new EntryWrapper((V)value);
        if (entrySet.remove(entry))
            return (V)value;

        return null;
    }

    @Override
    public Set<Map.Entry<K, V>> entrySet() {
        return entrySet;
    }

}
