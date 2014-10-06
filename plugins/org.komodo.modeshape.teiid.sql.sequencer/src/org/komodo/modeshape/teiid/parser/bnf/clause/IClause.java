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

import java.util.List;
import org.komodo.spi.constants.StringConstants;

/**
 *
 */
public interface IClause extends StringConstants {

    String BNF_APPEND_PREFIX = "append(bnf, "; //$NON-NLS-1$

    String BREAK_STATEMENT = TAB + TAB + TAB + TAB + "break" + SEMI_COLON + NEW_LINE; //$NON-NLS-1$

    IClause ROOT_CLAUSE = new IClause() {

        @Override
        public IClause nextClause() {
            throw new UnsupportedOperationException();
        }

        @Override
        public <T extends IClause> T getLastClause(Class<T> clauseClass) {
            throw new UnsupportedOperationException();
        }

        @Override
        public <T extends IGroupClause> T findLatestOpenGroupClause(Class<T> groupClass) {
            throw new UnsupportedOperationException();
        }

        @Override
        public List<String> getAppendStatements() {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean hasPPFunction() {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean hasMultiParameterPPFunction() {
            throw new UnsupportedOperationException();
        }

        @Override
        public List<TokenClause> getFirstTokenClauses() {
            throw new UnsupportedOperationException();
        }

        @Override
        public ClauseStack getOwningStack() {
            throw new UnsupportedOperationException();
        }

        @Override
        public void setOwningStack(ClauseStack clauseStack) {
            throw new UnsupportedOperationException();
        }
    };

    /**
     * @return next clause in the sequence
     */
    IClause nextClause();

    /**
     * Get the last clause in the clause's sequence
     *
     * @param clauseClass
     * @return last clause in the sequence or null
     */
    <T extends IClause> T getLastClause(Class<T> clauseClass);

    /**
     * @param groupClass
     *
     * @return latest group clause of type group class which is open
     */
    public <T extends IGroupClause> T findLatestOpenGroupClause(Class<T> groupClass);

    /**
     * @return list of all possible append statements
     */
    List<String> getAppendStatements();
    /**
     * @return true if the clause has a pp function or
     *                contains an inner clause that does
     */
    boolean hasPPFunction();

    /**
     * @return true if the clause has a pp function
     *                with multi-parameters or
     *                contains an inner clause that does
     */
    boolean hasMultiParameterPPFunction();

    /**
     * @return the first token clause(s)
     */
    List<TokenClause> getFirstTokenClauses();

    /**
     * @return clauseStack
     */
    ClauseStack getOwningStack();

    /**
     * @param clauseStack
     */
    void setOwningStack(ClauseStack clauseStack);

}
