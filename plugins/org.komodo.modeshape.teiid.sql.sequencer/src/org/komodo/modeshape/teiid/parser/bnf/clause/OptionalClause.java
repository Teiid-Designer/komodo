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
package org.komodo.modeshape.teiid.parser.bnf.clause;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 *
 */
public class OptionalClause extends AbstractGroupClause {

    @Override
    public List<String> getAppendStatements() {
        List<String> appendStatements = new ArrayList<String>();

        for (IClause clause : getClauseStack()) {
            appendStatements.addAll(clause.getAppendStatements());
        }

        return appendStatements;
    }

    @Override
    public List<TokenClause> getFirstTokenClauses() {
        List<TokenClause> tokenClauses = new ArrayList<TokenClause>();

        IClause firstClause = getClauseStack().get(0);
        tokenClauses.addAll(firstClause.getFirstTokenClauses());

        ClauseStack clauseStack = getOwningStack();
        IClause nextClause = clauseStack.nextClause(this);
        if (nextClause != null)
            tokenClauses.addAll(nextClause.getFirstTokenClauses());

        return tokenClauses;
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer();
        buf.append(OPEN_SQUARE_BRACKET + NEW_LINE);

        Iterator<IClause> clauseIter = getClauseStack().iterator();
        while(clauseIter.hasNext()) {
            buf.append(clauseIter.next().toString() + NEW_LINE);
        }

        buf.append(CLOSE_SQUARE_BRACKET);
        return buf.toString();
    }

}
