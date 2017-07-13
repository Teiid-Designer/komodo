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
package org.komodo.rest;

import org.komodo.spi.constants.StringConstants;

/**
 *
 */
public class RestProperty implements StringConstants {

    /**
     * Label used to describe name
     */
    public static final String NAME_LABEL = "name"; //$NON-NLS-1$

    /**
     * Label used to describe value
     */
    public static final String VALUE_LABEL = "value"; //$NON-NLS-1$

    private final String name;

    private final Object value;

    /**
     * @param name the name
     * @param value the value
     *
     */
    public RestProperty(String name, Object value) {
        this.name = name;
        this.value = value;
    }

    /**
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    /**
     * @return the value
     */
    public Object getValue() {
        return value;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
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
        RestProperty other = (RestProperty)obj;
        if (this.name == null) {
            if (other.name != null)
                return false;
        } else
            if (!this.name.equals(other.name))
                return false;
        if (this.value == null) {
            if (other.value != null)
                return false;
        } else
            if (!this.value.equals(other.value))
                return false;
        return true;
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "KomodoRestProperty [name=" + this.name + ", value=" + this.value + "]";
    }
}
