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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.komodo.utils.ArgCheck;

/**
 * A Path Clause
 *
 * 
 */
class PathClause extends Clause {

    private final List<String> paths = new ArrayList<String>();

    /**
     * @param parent parent searcher
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     * @param paths set of paths used in the clause
     */
    public PathClause(ObjectSearcher parent, LogicalOperator operator, String... paths) {
        super(parent, operator);

        ArgCheck.isNotEmpty(paths, "Where Path clause requires at least 1 value"); //$NON-NLS-1$

        for (String path : paths) {
            addPath(path);
        }
    }

    /**
     * @param path the path to be added
     */
    public void addPath(String path) {
        this.paths.add(path);
    }

    @Override
    public String clauseString(int position) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(position, buffer);

        buffer.append("PATH"); //$NON-NLS-1$
        buffer.append(OPEN_BRACKET);
        buffer.append(CLOSE_BRACKET);
        buffer.append(SPACE);
        buffer.append(IN);
        buffer.append(OPEN_BRACKET);

        Iterator<String> pathIter = paths.iterator();
        appendStringValues(buffer, pathIter);

        buffer.append(CLOSE_BRACKET);

        return buffer.toString();
    }

    
}
