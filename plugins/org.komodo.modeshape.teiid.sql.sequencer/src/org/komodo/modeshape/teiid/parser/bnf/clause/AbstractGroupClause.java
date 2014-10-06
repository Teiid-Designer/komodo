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

import org.komodo.spi.constants.StringConstants;

/**
 *
 */
public abstract class AbstractGroupClause implements IGroupClause, StringConstants {

    private boolean open = true;

    private ClauseStack clauseStack = new ClauseStack(this);

    private ClauseStack owningStack;

    /**
     * 
     */
    public AbstractGroupClause() {
        super();
    }

    @Override
    public ClauseStack getOwningStack() {
        return owningStack;
    }

    @Override
    public void setOwningStack(ClauseStack clauseStack) {
        this.owningStack = clauseStack;
    }

    @Override
    public IClause nextClause() {
        return getOwningStack().nextClause(this);
    }

    @Override
    public ClauseStack getClauseStack() {
        return clauseStack;
    }

    /**
     * @param clause
     */
    @Override
    public void addClause(IClause clause) {
        if (clauseStack.isEmpty() && clause instanceof OrClause)
            throw new RuntimeException("Cannot add an or clause to an empty stack"); //$NON-NLS-1$

        clauseStack.push(clause);
    }

    @Override
    public boolean isOpen() {
        return open;
    }

    @Override
    public boolean isClosed() {
        return !open;
    }

    @Override
    public void closeClause(Class<? extends IClause> clauseClass) {
        if (! clauseStack.isEmpty()) {
            IClause topClause = clauseStack.peek();
            if (topClause instanceof IGroupClause) {
                IGroupClause gClause = (IGroupClause) topClause;
                if (gClause.isOpen()) {
                    gClause.closeClause(clauseClass);
                    return;
                }
            }
        }

        // Get to here then all the group clauses in the clause stack
        // were closed so the close call is for this
        this.open = false;
    }

    @Override
    public <T extends IClause> T getLastClause(Class<T> clauseClass) {
        if (clauseStack.isEmpty())
            return null;
    
        IClause topClause = clauseStack.peek();
        T lastClause = topClause.getLastClause(clauseClass);
        if (lastClause != null)
            return lastClause;
        else if (clauseClass.isInstance(topClause))
          return (T) topClause;
        else if (clauseClass.isInstance(this))
            return (T) this;
        else
            return null;
    }

    @Override
    public <T extends IGroupClause> T findLatestOpenGroupClause(Class<T> groupClass) {
        T innerClause = getClauseStack().getLatestOpenGroupClause(groupClass);
        if (innerClause != null)
            return innerClause;
        else if (groupClass.isInstance(this) && this.isOpen())
            return (T) this;

        return null;
    }

    @Override
    public boolean hasPPFunction() {
        for (IClause clause : getClauseStack()) {
            if (clause.hasPPFunction())
                return true;
        }

        return false;
    }

    @Override
    public boolean hasMultiParameterPPFunction() {
        for (IClause clause : getClauseStack()) {
            if (clause.hasMultiParameterPPFunction())
                return true;
        }

        return false;

    }

}
