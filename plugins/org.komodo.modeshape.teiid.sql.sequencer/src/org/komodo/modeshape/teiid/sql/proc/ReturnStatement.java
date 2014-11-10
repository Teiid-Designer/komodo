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

package org.komodo.modeshape.teiid.sql.proc;

import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.proc.IReturnStatement;

/**
 *
 */
public class ReturnStatement extends AssignmentStatement implements IReturnStatement<Expression, LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public ReturnStatement(TeiidSeqParser p, int id) {
        super(p, id);
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public ReturnStatement clone() {
        ReturnStatement clone = new ReturnStatement(this.getTeiidParser(), this.getId());

        if (getExpression() != null)
            clone.setExpression(getExpression().clone());
        if (getCommand() != null)
            clone.setCommand(getCommand().clone());
        if (getVariable() != null)
            clone.setVariable(getVariable().clone());
        if (getValue() != null)
            clone.setValue(getValue().clone());

        return clone;
    }

}
