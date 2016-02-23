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
package org.komodo.spi.query;

/**
 * Enumerator for types of parameters 
 */
public enum ParameterInfo {
    /** Constant identifying an IN parameter */
    IN,

    /** Constant identifying an OUT parameter */
    OUT,

    /** Constant identifying an INOUT parameter */
    INOUT,

    /** Constant identifying a RETURN parameter */
    RETURN_VALUE,

    /** Constant identifying a RESULT SET parameter */
    RESULT_SET;

    /**
     * Get the index of the enumerator. For compatibility
     * with existing code, the index starts at 1 rather than 0.
     * 
     * @return value of index
     */
    public int index() {
        return ordinal() + 1;
    }

    public static ParameterInfo valueOf(int type) {
        for (ParameterInfo info : ParameterInfo.values()) {
            if (info.index() == type)
                return info;
        }

        throw new IllegalArgumentException();
    }
}
