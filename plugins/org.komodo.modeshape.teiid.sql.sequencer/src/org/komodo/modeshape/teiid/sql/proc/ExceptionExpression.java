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
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.modeshape.teiid.sql.lang.ASTNode;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.proc.IExceptionExpression;
import org.komodo.spi.type.IDataTypeManagerService.DataTypeName;

public class ExceptionExpression extends ASTNode implements Expression, IExceptionExpression<LanguageVisitor> {

    public ExceptionExpression(ITeiidParser p, int id) {
        super(p, id);
        assignTypeName(DataTypeName.OBJECT);
    }

    @Override
    public Class<?> getType() {
        return convertTypeClassPropertyToClass(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME);
    }

    private void assignTypeName(DataTypeName dataTypeName) {
        setProperty(TeiidSqlLexicon.Expression.TYPE_CLASS_PROP_NAME, dataTypeName.name());
    }

    public Expression getMessage() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.ExceptionExpression.MESSAGE_REF_NAME, Expression.class);
    }

    public void setMessage(Expression errMsg) {
        setChild(TeiidSqlLexicon.ExceptionExpression.MESSAGE_REF_NAME, errMsg);
    }

    public Expression getSqlState() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.ExceptionExpression.SQL_STATE_REF_NAME, Expression.class);
    }

    public void setSqlState(Expression sqlState) {
        setChild(TeiidSqlLexicon.ExceptionExpression.SQL_STATE_REF_NAME, sqlState);
    }

    public Expression getErrorCode() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.ExceptionExpression.ERROR_CODE_REF_NAME, Expression.class);
    }

    public void setErrorCode(Expression errCode) {
        setChild(TeiidSqlLexicon.ExceptionExpression.ERROR_CODE_REF_NAME, errCode);
    }

    public Expression getParentExpression() {
        return getChildforIdentifierAndRefType(
                                               TeiidSqlLexicon.ExceptionExpression.PARENT_EXPRESSION_REF_NAME, Expression.class);
    }

    public void setParentExpression(Expression parent) {
        setChild(TeiidSqlLexicon.ExceptionExpression.PARENT_EXPRESSION_REF_NAME, parent);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.getErrorCode() == null) ? 0 : this.getErrorCode().hashCode());
        result = prime * result + ((this.getMessage() == null) ? 0 : this.getMessage().hashCode());
        result = prime * result + ((this.getParent() == null) ? 0 : this.getParent().hashCode());
        result = prime * result + ((this.getSqlState() == null) ? 0 : this.getSqlState().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        ExceptionExpression other = (ExceptionExpression)obj;
        if (this.getErrorCode() == null) {
            if (other.getErrorCode() != null) return false;
        } else if (!this.getErrorCode().equals(other.getErrorCode())) return false;
        if (this.getMessage() == null) {
            if (other.getMessage() != null) return false;
        } else if (!this.getMessage().equals(other.getMessage())) return false;
        if (this.getParentExpression() == null) {
            if (other.getParentExpression() != null) return false;
        } else if (!this.getParentExpression().equals(other.getParentExpression())) return false;
        if (this.getSqlState() == null) {
            if (other.getSqlState() != null) return false;
        } else if (!this.getSqlState().equals(other.getSqlState())) return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public ExceptionExpression clone() {
        ExceptionExpression clone = new ExceptionExpression(this.getTeiidParser(), this.getId());

        if(getErrorCode() != null)
            clone.setErrorCode(getErrorCode().clone());
        if(getSqlState() != null)
            clone.setSqlState(getSqlState().clone());
        if(getMessage() != null)
            clone.setMessage(getMessage().clone());

        return clone;
    }

}
