/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
 ************************************************************************************/
package org.komodo.modeshape.teiid.language;

/**
 *
 */
public interface SortSpecification {

    /**
     * How to sort on a specific expression, eg. {code:sql}col1 NULLS FIRST{code}
     */
    enum NullOrdering {
        FIRST,
        LAST;

        /**
         * @param name string version of a NullOrdering enum value
         * @return value with given name
         */
        public static NullOrdering findNullOrdering(String name) {
            if (name == null)
                return null;

            name = name.toUpperCase();
            for (NullOrdering no : values()) {
                if (no.name().equals(name))
                    return no;
            }
            return null;
        }
    }
}
