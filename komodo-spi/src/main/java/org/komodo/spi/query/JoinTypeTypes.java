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

import org.komodo.spi.constants.StringConstants;

/**
 * Delineation of the category of join type
 */
public enum JoinTypeTypes {
    /** Represents an inner join:  a INNER JOIN b */
    JOIN_INNER(false),

    /** Represents a right outer join:  a RIGHT OUTER JOIN b */
    JOIN_RIGHT_OUTER(true),

    /** Represents a left outer join:  a LEFT OUTER JOIN b */
    JOIN_LEFT_OUTER(true),

    /** Represents a full outer join:  a FULL OUTER JOIN b */
    JOIN_FULL_OUTER(true),

    /** Represents a cross join:  a CROSS JOIN b */
    JOIN_CROSS(false),

    /** Represents a union join:  a UNION JOIN b - not used after rewrite */
    JOIN_UNION(true),

    /** internal SEMI Join type */
    JOIN_SEMI(false),

    /** internal ANTI SEMI Join type */
    JOIN_ANTI_SEMI(true);

    private final boolean outer;

    private JoinTypeTypes(boolean outer) {
        this.outer = outer;
    }

    public int getTypeCode() {
        return this.ordinal();
    }

    public boolean isOuter() {
        return this.outer;
    }

    public String toPrintStatement() {
        String name = name();
        String JOIN = "JOIN"; //$NON-NLS-1$

        name = name.substring((JOIN + StringConstants.UNDERSCORE).length());
        name = name.replaceAll(StringConstants.UNDERSCORE, StringConstants.SPACE);
        name = name + StringConstants.SPACE + JOIN;
        return name;
    }

    /**
     * @param name
     * @return Types representing the given name
     */
    public static JoinTypeTypes findType(String name) {
        if (name == null)
            return null;

        name = name.toUpperCase();
        for (JoinTypeTypes type : values()) {
            if (type.name().equals(name))
                return type;
        }

        return null;
    }

    /**
     * @param index
     * @return Types representing the given index
     */
    public static JoinTypeTypes findType(int index) {
        for (JoinTypeTypes type : values()) {
            if (type.ordinal() == index)
                return type;
        }

        return null;
    }
}
