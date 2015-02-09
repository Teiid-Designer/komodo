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
package org.komodo.repository.search;

import org.komodo.utils.ArgCheck;
import org.komodo.utils.StringUtils;

/**
 * A Path Clause
 */
class PathClause extends Clause {

    private final static String PATH = "path"; //$NON-NLS-1$

    /**
     * @param parent parent searcher
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias of the selector
     * @param path path used in the clause
     */
    public PathClause(ObjectSearcher parent, LogicalOperator operator, String alias, String path) {
        super(parent, operator);

        ArgCheck.isNotEmpty(path, "Where Path clause requires a path"); //$NON-NLS-1$

        setAlias(alias);
        setPath(path);
    }

    public String getPath() {
        return properties.get(PATH);
    }

    /**
     * @param path the path to be added
     */
    public void setPath(String path) {
        properties.put(PATH, path);
    }

    @Override
    public String clauseString(int position) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(position, buffer);

        setAlias(checkWhereAlias(getAlias()));

        buffer.append("PATH"); //$NON-NLS-1$
        buffer.append(OPEN_BRACKET);

        if (! StringUtils.isEmpty(getAlias())) {
            buffer.append(getAlias());
        }

        buffer.append(CLOSE_BRACKET);
        buffer.append(SPACE);
        buffer.append(EQUALS);
        buffer.append(SPACE);
        buffer.append(QUOTE_MARK);
        buffer.append(getPath());
        buffer.append(QUOTE_MARK);

        return buffer.toString();
    }

    
}
