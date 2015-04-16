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

import org.komodo.utils.StringUtils;
import org.modeshape.jcr.api.JcrConstants;

/**
 * A Parent Path Clause
 */
class ParentPathClause extends PathClause {

    /**
     * @param parent parent searcher
     * @param operator the logical operator preceding this clause (can be null if this is the only clause)
     * @param alias the alias of the selector
     * @param path path used in the clause
     */
    public ParentPathClause(ObjectSearcher parent, LogicalOperator operator, String alias, String path) {
        super(parent, operator, alias, path);
    }

    @Override
    public String clauseString(int position) {
        StringBuffer buffer = new StringBuffer();

        appendLogicalOperator(position, buffer);

        setAlias(checkWhereAlias(getAlias()));

//        where e.[jcr:path] LIKE '/inf:patient[265]%'

        if (! StringUtils.isEmpty(getAlias())) {
            buffer.append(getAlias());
            buffer.append(DOT);
        }

        buffer.append(OPEN_SQUARE_BRACKET);
        buffer.append(JcrConstants.JCR_PATH);
        buffer.append(CLOSE_SQUARE_BRACKET);

        buffer.append(SPACE);
        buffer.append(LIKE);
        buffer.append(SPACE);

        buffer.append(QUOTE_MARK);
        buffer.append(getPath());
        
        if (! getPath().endsWith(CLOSE_SQUARE_BRACKET) &&
             ! getPath().endsWith(FORWARD_SLASH))
            buffer.append(FORWARD_SLASH);

        buffer.append(PERCENT);
        buffer.append(QUOTE_MARK);

        return buffer.toString();
    }    
}
