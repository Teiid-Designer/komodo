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
package org.teiid.query.sql.proc;

import org.komodo.spi.query.sql.proc.IExceptionExpression;
import org.teiid.core.types.DefaultDataTypeManager;
import org.teiid.query.parser.TCLanguageVisitorImpl;
import org.teiid.query.parser.TeiidClientParser;
import org.teiid.query.sql.lang.SimpleNode;
import org.teiid.query.sql.symbol.Expression;

/**
 *
 */
public class ExceptionExpression extends SimpleNode implements Expression, IExceptionExpression<TCLanguageVisitorImpl> {

    /**
     * @param teiidParser 
     * @param i
     */
    public ExceptionExpression(TeiidClientParser teiidParser, int i) {
        super(teiidParser, i);
    }

    private Expression message;
    private Expression sqlState;
    private Expression errorCode;
    private Expression parent;
    
    @Override
    public Class<?> getType() {
        return DefaultDataTypeManager.DefaultDataTypes.OBJECT.getClass();
    }

    /**
     * @return error code
     */
    public Expression getErrorCode() {
        return errorCode;
    }
    
    /**
     * @param errCode
     */
    public void setErrorCode(Expression errCode) {
        this.errorCode = errCode;
    }
    
    /**
     * @return sql state
     */
    public Expression getSqlState() {
        return sqlState;
    }
    
    /**
     * @param sqlState
     */
    public void setSqlState(Expression sqlState) {
        this.sqlState = sqlState;
    }
    
    /**
     * @return message
     */
    public Expression getMessage() {
        return message;
    }
    
    /**
     * @param message
     */
    public void setMessage(Expression message) {
        this.message = message;
    }
    
    /**
     * @return parent expression
     */
    public Expression getParent() {
        return parent;
    }
    
    /**
     * @param parent
     */
    public void setParent(Expression parent) {
        this.parent = parent;
    }

    /**
     * @return default sql state
     */
    public String getDefaultSQLState() {
        return "50001"; //$NON-NLS-1$
    }

    @SuppressWarnings( "nls" )
    @Override
    public String toString() {
        return "ExceptionExpression [message=" + this.message + ", sqlState=" + this.sqlState + ", errorCode=" + this.errorCode
               + ", parent=" + this.parent + "]";
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        ExceptionExpression other = (ExceptionExpression)obj;
        if (this.errorCode == null) {
            if (other.errorCode != null) return false;
        } else if (!this.errorCode.equals(other.errorCode)) return false;
        if (this.message == null) {
            if (other.message != null) return false;
        } else if (!this.message.equals(other.message)) return false;
        if (this.parent == null) {
            if (other.parent != null) return false;
        } else if (!this.parent.equals(other.parent)) return false;
        if (this.sqlState == null) {
            if (other.sqlState != null) return false;
        } else if (!this.sqlState.equals(other.sqlState)) return false;
        return true;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.errorCode == null) ? 0 : this.errorCode.hashCode());
        result = prime * result + ((this.message == null) ? 0 : this.message.hashCode());
        result = prime * result + ((this.parent == null) ? 0 : this.parent.hashCode());
        result = prime * result + ((this.sqlState == null) ? 0 : this.sqlState.hashCode());
        return result;
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(TCLanguageVisitorImpl visitor) {
        visitor.visit(this);
    }

    @Override
    public ExceptionExpression clone() {
        ExceptionExpression clone = new ExceptionExpression(this.parser, this.id);

        if(getErrorCode() != null)
            clone.setErrorCode(getErrorCode().clone());
        if(getSqlState() != null)
            clone.setSqlState(getSqlState().clone());
        if(getMessage() != null)
            clone.setMessage(getMessage().clone());
        if(getParent() != null)
            clone.setParent(getParent().clone());

        return clone;
    }

}
