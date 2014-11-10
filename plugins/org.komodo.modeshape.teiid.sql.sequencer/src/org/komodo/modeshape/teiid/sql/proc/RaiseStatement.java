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

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.proc.IRaiseStatement;

/**
 *
 */
public class RaiseStatement extends Statement implements ExpressionStatement, IRaiseStatement<LanguageVisitor, Expression> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public RaiseStatement(TeiidSeqParser p, int id) {
        super(p, id);
        setType(StatementType.TYPE_ERROR);
    }

    @Override
    public Expression getExpression() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.ExpressionStatement.EXPRESSION_REF_NAME, Expression.class);
    }

    @Override
    public void setExpression(Expression expr) {
        setChild(TeiidSqlLexicon.ExpressionStatement.EXPRESSION_REF_NAME, expr);
    }

    public boolean isWarning() {
        Object property = getProperty(TeiidSqlLexicon.RaiseStatement.WARNING_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setWarning(boolean warning) {
        setProperty(TeiidSqlLexicon.RaiseStatement.WARNING_PROP_NAME, warning);
    }

    @Override
    public Class<?> getExpectedType() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getExpression() == null) ? 0 : this.getExpression().hashCode());
        result = prime * result + (this.isWarning() ? 1231 : 1237);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (!super.equals(obj))
            return false;
        if (getClass() != obj.getClass())
            return false;
        RaiseStatement other = (RaiseStatement)obj;
        if (this.getExpression() == null) {
            if (other.getExpression() != null)
                return false;
        } else if (!this.getExpression().equals(other.getExpression()))
            return false;
        if (this.isWarning() != other.isWarning())
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public RaiseStatement clone() {
        RaiseStatement clone = new RaiseStatement(this.getTeiidParser(), this.getId());

        if (getExpression() != null)
            clone.setExpression(getExpression().clone());
        clone.setWarning(isWarning());

        return clone;
    }

}
