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
import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.TeiidSeqParser;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbolImpl;
import org.komodo.modeshape.teiid.sql.symbol.BaseExpression;
import org.komodo.spi.query.sql.lang.SPParameter;
import org.komodo.utils.KLog;

/**
* Represents a StoredProcedure's parameter for encapsulation in the Query framework
* This is basically a holder object set from the Server's implementation of
* a stored procedure.
* The connector will utilize this class to set the appropriate values at the
* datasource layer.
*/
public class SPParameterImpl extends ASTNode implements SPParameter<ElementSymbolImpl> {

    /** Constant identifying an IN parameter */
    public static final int IN = ParameterInfo.IN.index();

    /** Constant identifying an OUT parameter */
    public static final int OUT = ParameterInfo.OUT.index();

    /** Constant identifying an INOUT parameter */
    public static final int INOUT = ParameterInfo.INOUT.index();

    /** Constant identifying a RETURN parameter */
    public static final int RETURN_VALUE = ParameterInfo.RETURN_VALUE.index();

    /** Constant identifying a RESULT SET parameter */
    public static final int RESULT_SET = ParameterInfo.RESULT_SET.index();

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public SPParameterImpl(TeiidSeqParser p, int id) {
        super(p, id);
    }

    private ElementSymbolImpl createElementSymbol(String name) {
        ElementSymbolImpl symbol = getTeiidParser().createASTNode(ASTNodes.ELEMENT_SYMBOL);
        symbol.setName(name);
        return symbol;
    }

    /**
     * Get full parameter name,.  If unknown, null is returned.
     * @return Parameter name
     */
    @Override
    public String getName() {
        Object property = getProperty(TeiidSqlLexicon.SPParameter.NAME_PROP_NAME);
        return property == null ? null : property.toString();
    }

    /**
     * Set full parameter name
     * @param name Parameter name
     */
    @Override
    public void setName(String name) {
        setProperty(TeiidSqlLexicon.SPParameter.NAME_PROP_NAME, name);
    }

    /**
     * Get type of parameter according to class constants.
     * @return Parameter type
     */
    @Override
    public int getParameterType() {
        Object property = getProperty(TeiidSqlLexicon.SPParameter.PARAMETER_TYPE_PROP_NAME);
        return property == null ? -1 : Integer.parseInt(property.toString());
    }

    /**
     * Set parameter type according to class constants.
     *
     * @param parameterType Type to set
     */
    public void setParameterType(int parameterType) {
        setProperty(TeiidSqlLexicon.SPParameter.PARAMETER_TYPE_PROP_NAME, parameterType);
    }

    /**
     * @param parameterInfo value
     */
    @Override
    public void setParameterType(ParameterInfo parameterInfo) {
        setParameterType(parameterInfo.index());
    }

    /**
     * Get class type
     * @return Runtime type description
     */
    @Override
    public Class<?> getClassType() {
        Object property = getProperty(TeiidSqlLexicon.SPParameter.CLASS_TYPE_CLASS_PROP_NAME);
        try {
            return property == null ? null : Class.forName(property.toString());
        } catch (ClassNotFoundException ex) {
            KLog.getLogger().error(ex.getLocalizedMessage(), ex);
            return null;
        }
    }

    /**
     * Set class type
     *
     * @param classType for types
     */
    @Override
    public void setClassType(Class<?> classType) {
       setProperty(TeiidSqlLexicon.SPParameter.CLASS_TYPE_CLASS_PROP_NAME, classType.getCanonicalName()); 
    }

    /**
     * Return the expression defining the value of this parameter
     * @return Expression defining the value of this parameter
     */
    public BaseExpression getExpression() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.SPParameter.EXPRESSION_REF_NAME, BaseExpression.class);
    }

    /**
     * Set the expression defining this parameter
     * @param expression The expression defining this parameter's value
     */
    public void setExpression(BaseExpression expression) {
        setChild(TeiidSqlLexicon.SPParameter.EXPRESSION_REF_NAME, expression);
    }

    /**
     * Return the index of this parameter
     * @return The index
     */
    public int getIndex() {
        Object property = getProperty(TeiidSqlLexicon.SPParameter.INDEX_PROP_NAME);
        return property == null ? 0 : Integer.parseInt(property.toString());
    }

    /**
     * Set the positional index of this parameter
     * @param index The positional index of this parameter
     */
    public void setIndex(int index) {
        setProperty(TeiidSqlLexicon.SPParameter.INDEX_PROP_NAME, index);
    }

    /**
     * Get the result set columns.  If none exist, return empty list.
     * @return List of ElementSymbol representing result set columns
     */
    @Override
    public List<ElementSymbolImpl> getResultSetColumns() {
        return getChildrenforIdentifierAndRefType(
                                               TeiidSqlLexicon.SPParameter.RESULT_SET_COLUMN_REF_NAME, ElementSymbolImpl.class);
    }

    /**
     * Add a result set column if this parameter is a return
     * result set.
     * @param colName Name of column
     * @param type Type of column
     * @param id Object Id
     */
    @Override
    public void addResultSetColumn(String colName, Class<?> type, Object id) {
        ElementSymbolImpl elementSymbol = createElementSymbol(colName);
        elementSymbol.setType(type);
        elementSymbol.setMetadataID(id);
        addLastChild(TeiidSqlLexicon.SPParameter.RESULT_SET_COLUMN_REF_NAME, elementSymbol);
    }

    public void setResultSetColumn(List<ElementSymbolImpl> resultColumns) {
        setChildren(TeiidSqlLexicon.SPParameter.RESULT_SET_COLUMN_REF_NAME, resultColumns);
    }

    /**
     * Get actual metadataID for this parameter
     * @return Actual metadata ID for this parameter
     */
    @Override
    public Object getMetadataID() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.SPParameter.METADATAID_PROP_NAME, Object.class);
    }

    /**
     * Set actual metadataID for this parameter
     * @param metadataID Actual metadataID
     */
    @Override
    public void setMetadataID(Object metadataID) {
        setProperty(TeiidSqlLexicon.SPParameter.METADATAID_PROP_NAME, metadataID);
    }

    /**
     * Get element symbol representing this parameter.  The symbol will have the
     * same name and type as the parameter.
     * @return Element symbol representing the parameter
     */
    @Override
    public ElementSymbolImpl getParameterSymbol() {
        ElementSymbolImpl elementSymbol = createElementSymbol(getName());
        elementSymbol.setMetadataID(getMetadataID());
        elementSymbol.setType(getClassType());
        return elementSymbol;
    }

    public boolean isUsingDefault() {
        Object property = getProperty(TeiidSqlLexicon.SPParameter.USING_DEFAULT_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setUsingDefault(boolean usingDefault) {
        setProperty(TeiidSqlLexicon.SPParameter.USING_DEFAULT_PROP_NAME, usingDefault);
    }

    public boolean isVarArg() {
        Object property = getProperty(TeiidSqlLexicon.SPParameter.VAR_ARG_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setVarArg(boolean varArg) {
        setProperty(TeiidSqlLexicon.SPParameter.VAR_ARG_PROP_NAME, varArg);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.getExpression() == null) ? 0 : this.getExpression().hashCode());
        result = prime * result + this.getIndex();
        result = prime * result + ((this.getTeiidParser() == null) ? 0 : this.getTeiidParser().hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SPParameterImpl other = (SPParameterImpl)obj;
        if (this.getExpression() == null) {
            if (other.getExpression() != null)
                return false;
        } else if (!this.getExpression().equals(other.getExpression()))
            return false;
        if (this.getIndex() != other.getIndex())
            return false;
        if (this.getTeiidParser() == null) {
            if (other.getTeiidParser() != null)
                return false;
        } else if (!this.getTeiidParser().equals(other.getTeiidParser()))
            return false;
        return true;
    }

    /**
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        if (this.getExpression() != null) {
            return this.getExpression().toString();
        }
        return "?"; //$NON-NLS-1$
    }

    @Override
    public SPParameterImpl clone() {
        SPParameterImpl clone = new SPParameterImpl(getTeiidParser(), getId());
        clone.setIndex(getIndex());
        clone.setParameterType(getParameterType());

        if (getExpression() != null) {
            clone.setExpression(getExpression().clone());
        }

        clone.setUsingDefault(isUsingDefault());
        clone.setVarArg(isVarArg());
        return clone;
    }
}
