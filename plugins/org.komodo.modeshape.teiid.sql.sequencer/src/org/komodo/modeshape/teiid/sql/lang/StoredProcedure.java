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

package org.komodo.modeshape.teiid.sql.lang;

import java.util.List;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.lang.IStoredProcedure;

public class StoredProcedure extends ProcedureContainer implements TargetedCommand, IStoredProcedure<SPParameter, Expression, LanguageVisitor> {

    public StoredProcedure(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public int getType() {
        throw new UnsupportedOperationException();
    }

    public boolean isCalledWithReturn() {
        return false;
    }

    /**
     * @param b
     */
    public void setCalledWithReturn(boolean b) {
    }

    public boolean isCallableStatement() {
        return false;
    }

    /**
     * @param b
     */
    public void setCallableStatement(boolean b) {
    }

    public boolean isDisplayNamedParameters() {
        return false;
    }

    @Override
    public void setDisplayNamedParameters(boolean b) {
    }

    @Override
    public Object getProcedureID() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setProcedureID(Object procedureID) {
    }

    @Override
    public GroupSymbol getGroup() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getGroupName() {
        throw new UnsupportedOperationException();
    }

    public void setGroup(GroupSymbol group) {

    }

    @Override
    public List<SPParameter> getInputParameters() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setParameter(SPParameter parameter) {
    }

    @Override
    public String getProcedureCallableName() {
        throw new UnsupportedOperationException();
    }

    @Override
    public void setProcedureName(String procFullName) {
    }

    @Override
    public List<Expression> getProjectedSymbols() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return
     */
    private Object getResultSetParameterKey() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return
     */
    private Object getProcedureName() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return
     */
    private Object getMapOfParameters() {
        throw new UnsupportedOperationException();
    }

    /**
     * @return
     */
    private List<SPParameter> getParameters() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isCalledWithReturn() ? 1231 : 1237);
        result = prime * result + (this.isDisplayNamedParameters() ? 1231 : 1237);
        result = prime * result + (this.isCallableStatement() ? 1231 : 1237);
        result = prime * result + ((this.getMapOfParameters() == null) ? 0 : this.getMapOfParameters().hashCode());
        result = prime * result + ((this.getProcedureName() == null) ? 0 : this.getProcedureName().hashCode());
        result = prime * result + ((this.getResultSetParameterKey() == null) ? 0 : this.getResultSetParameterKey().hashCode());
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
        StoredProcedure other = (StoredProcedure)obj;
        if (this.isCalledWithReturn() != other.isCalledWithReturn())
            return false;
        if (this.isDisplayNamedParameters() != other.isDisplayNamedParameters())
            return false;
        if (this.isCallableStatement() != other.isCallableStatement())
            return false;
        if (this.getMapOfParameters() == null) {
            if (other.getMapOfParameters() != null)
                return false;
        } else if (!this.getMapOfParameters().equals(other.getMapOfParameters()))
            return false;
        if (this.getProcedureName() == null) {
            if (other.getProcedureName() != null)
                return false;
        } else if (!this.getProcedureName().equals(other.getProcedureName()))
            return false;
        if (this.getResultSetParameterKey() == null) {
            if (other.getResultSetParameterKey() != null)
                return false;
        } else if (!this.getResultSetParameterKey().equals(other.getResultSetParameterKey()))
            return false;
        return true;
    }

    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public StoredProcedure clone() {
        StoredProcedure clone = new StoredProcedure(this.getTeiidParser(), this.getId());

        clone.setCallableStatement(isCallableStatement());

        if (getParameters() != null) {
            for (SPParameter parameter : getParameters()) {
                clone.setParameter(parameter.clone());
            }
        }

        clone.setCalledWithReturn(isCalledWithReturn());
        clone.setDisplayNamedParameters(isDisplayNamedParameters());
        if (getSourceHint() != null)
            clone.setSourceHint(getSourceHint());
        if (getOption() != null)
            clone.setOption(getOption().clone());

        copyMetadataState(clone);
        return clone;
    }

}
