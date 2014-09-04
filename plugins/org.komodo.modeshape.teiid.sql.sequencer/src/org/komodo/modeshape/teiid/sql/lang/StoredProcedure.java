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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.modeshape.teiid.sql.symbol.GroupSymbol;
import org.komodo.spi.query.sql.lang.ISPParameter.ParameterInfo;
import org.komodo.spi.query.sql.lang.IStoredProcedure;

public class StoredProcedure extends ProcedureContainer implements TargetedCommand, IStoredProcedure<SPParameter, Expression, LanguageVisitor> {

    public StoredProcedure(TeiidParser p, int id) {
        super(p, id);
        setType(TYPE_STORED_PROCEDURE);
    }

    public boolean isCalledWithReturn() {
        Object property = getProperty(TeiidSqlLexicon.StoredProcedure.CALLED_WITH_RETURN_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setCalledWithReturn(boolean calledWithReturn) {
        setProperty(TeiidSqlLexicon.StoredProcedure.CALLED_WITH_RETURN_PROP_NAME, calledWithReturn);
    }

    public boolean isCallableStatement() {
        Object property = getProperty(TeiidSqlLexicon.StoredProcedure.CALLABLE_STATEMENT_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setCallableStatement(boolean callableStatement) {
        setProperty(TeiidSqlLexicon.StoredProcedure.CALLABLE_STATEMENT_PROP_NAME, callableStatement);
    }

    public boolean isDisplayNamedParameters() {
        Object property = getProperty(TeiidSqlLexicon.StoredProcedure.DISPLAY_NAMED_PARAMETERS_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    @Override
    public void setDisplayNamedParameters(boolean displayNamedParameters) {
        setProperty(TeiidSqlLexicon.StoredProcedure.DISPLAY_NAMED_PARAMETERS_PROP_NAME, displayNamedParameters);
    }

    @Override
    public Object getProcedureID() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.StoredProcedure.PROCEDUREID_PROP_NAME, Object.class);
    }

    @Override
    public void setProcedureID(Object procedureID) {
        setProperty(TeiidSqlLexicon.StoredProcedure.PROCEDUREID_PROP_NAME, procedureID);
    }

    @Override
    public GroupSymbol getGroup() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.TargetedCommand.GROUP_REF_NAME, GroupSymbol.class);
    }

    @Override
    public String getGroupName() {
        GroupSymbol group = getGroup();
        return group == null ? null : group.getName();
    }

    public void setGroup(GroupSymbol group) {
        setChild(TeiidSqlLexicon.TargetedCommand.GROUP_REF_NAME, group);
    }

    @Override
    public List<SPParameter> getInputParameters() {
        List<SPParameter> parameters = new ArrayList<SPParameter>(getParameters());
        Iterator<SPParameter> params = parameters.iterator();
        while (params.hasNext()) {
            SPParameter param = params.next();
            if(param.getParameterType() != ParameterInfo.IN.index() && param.getParameterType() != ParameterInfo.INOUT.index()) {
                params.remove();
            }
        }
        return parameters;
    }

    public List<SPParameter> getParameters() {
        return getChildrenforIdentifierAndRefType(
                                                  TeiidSqlLexicon.StoredProcedure.PARAMETER_REF_NAME, SPParameter.class);
    }

    @Override
    public void addParameter(SPParameter parameter) {
        setChild(TeiidSqlLexicon.StoredProcedure.PARAMETER_REF_NAME, parameter);
    }

    @Override
    public String getProcedureCallableName() {
        Object property = getProperty(TeiidSqlLexicon.StoredProcedure.CALLABLE_STATEMENT_PROP_NAME);
        return property == null ? getProcedureName() : property.toString();
    }

    public void setProcedureCallableName(String callableName) {
        setProperty(TeiidSqlLexicon.StoredProcedure.CALLABLE_STATEMENT_PROP_NAME, callableName);
    }

    public String getProcedureName() {
        Object property = getProperty(TeiidSqlLexicon.StoredProcedure.PROCEDURE_NAME_PROP_NAME);
        return property == null ? null : property.toString();
    }

    @Override
    public void setProcedureName(String procFullName) {
        setProperty(TeiidSqlLexicon.StoredProcedure.PROCEDURE_NAME_PROP_NAME, procFullName);
    }

    @Override
    public List<Expression> getProjectedSymbols() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + (this.isCalledWithReturn() ? 1231 : 1237);
        result = prime * result + (this.isDisplayNamedParameters() ? 1231 : 1237);
        result = prime * result + (this.isCallableStatement() ? 1231 : 1237);
        result = prime * result + ((this.getProcedureName() == null) ? 0 : this.getProcedureName().hashCode());
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
        if (this.getProcedureName() == null) {
            if (other.getProcedureName() != null)
                return false;
        } else if (!this.getProcedureName().equals(other.getProcedureName()))
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
                clone.addParameter(parameter.clone());
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
