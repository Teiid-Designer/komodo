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

import java.util.Iterator;
import java.util.List;
import org.komodo.modeshape.teiid.parser.TeiidNodeFactory.ASTNodes;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.symbol.ElementSymbol;
import org.komodo.modeshape.teiid.sql.symbol.Expression;
import org.komodo.spi.query.sql.lang.ISPParameter;

/**
* Represents a StoredProcedure's parameter for encapsulation in the Query framework
* This is basically a holder object set from the Server's implementation of
* a stored procedure.
* The connector will utilize this class to set the appropriate values at the
* datasource layer.
*/
public class SPParameter implements ISPParameter<ElementSymbol> {

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

    private TeiidParser teiidParser;

    /**
     * Constructor used when constructing a parameter during execution.  In this case we
     * know what the parameter is being filled with but no metadata about the parameter.
     *
     * @param teiidParser
     * @param index the positional index of this parameter
     * @param expression
     */
    public SPParameter(TeiidParser teiidParser, int index, Expression expression) {
        this.teiidParser = teiidParser;
        setIndex(index);
        setExpression(expression);
    }

    /**
     * Constructor used when constructing a parameter from metadata.
     * In this case we specify the description of the parameter but
     * no notion of what it is being filled with.
     *
     * @param teiidParser
     * @param index Parameter index
     * @param parameterType Type of parameter based on class constant - IN, OUT, etc
     * @param name Full name of parameter (including proc name)
     */
    public SPParameter(TeiidParser teiidParser, int index, int parameterType, String name) {
        this.teiidParser = teiidParser;
        setIndex(index);
        setParameterType(parameterType);
        setName(name);
    }

    /**
     * @return the teiidParser
     */
    public TeiidParser getTeiidParser() {
        return this.teiidParser;
    }

    @SuppressWarnings( "unused" )
    private ElementSymbol createElementSymbol(String name) {
        ElementSymbol symbol = teiidParser.createASTNode(ASTNodes.ELEMENT_SYMBOL);
        symbol.setName(name);
        return symbol;
    }

    /**
     * Get full parameter name,.  If unknown, null is returned.
     * @return Parameter name
     */
    @Override
    public String getName() {
        return this.getParameterSymbol().getName();
    }

    /**
     * Set full parameter name
     * @param name Parameter name
     */
    @Override
    public void setName(String name) {
    }

    /**
     * @param parameterInfo
     */
    @Override
    public void setParameterType(ParameterInfo parameterInfo) {
    }

    /**
     * Set parameter type according to class constants.
     * @param parameterType Type to set
     */
    public void setParameterType(int parameterType) {
    }

    /**
     * Get type of parameter according to class constants.
     * @return Parameter type
     */
    @Override
    public int getParameterType() {
        return this.getParameterType();
    }

    /**
     * Set class type - MetaMatrix runtime types.
     * @param classType
     * for types
     */
    @Override
    public void setClassType(Class<?> classType) {
    }

    /**
     * Get class type - MetaMatrix runtime types.
     * @return MetaMatrix runtime type description
     */
    @Override
    public Class<?> getClassType() {
        return null;
    }

    /**
     * Set the expression defining this parameter
     * @param expression The expression defining this parameter's value
     */
    public void setExpression(Expression expression) {
    }

    /**
     * Return the expression defining the value of this parameter
     * @return Expression defining the value of this parameter
     */
    public Expression getExpression() {
        return null;
    }

    /**
     * Set the positional index of this parameter
     * @param index The positional index of this parameter
     */
    public void setIndex(int index) {

    }

    /**
     * Return the index of this parameter
     * @return The index
     */
    public int getIndex() {
        return 0;

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
    }

    /**
     * Get the result set columns.  If none exist, return empty list.
     * @return List of ElementSymbol representing result set columns
     */
    @Override
    public List<ElementSymbol> getResultSetColumns() {
        return null;
    }

    /**
     * Get the result set metadata IDs.  If none exist, return empty list.
     * @return List of Object representing result set metadata IDs
     */
    public List<Object> getResultSetIDs() {
        return null;
    }

    /**
     * Get a particular result set column at the specified position.
     * @param position Position of the result set column
     * @return Element symbol representing the result set column at position
     * @throws IllegalArgumentException If column doesn't exist
     */
    public ElementSymbol getResultSetColumn(int position) {
        return null;
    }

    /**
     * Get actual metadataID for this parameter
     * @return Actual metadata ID for this parameter
     */
    @Override
    public Object getMetadataID() {
        return teiidParser;
    }

    /**
     * Set actual metadataID for this parameter
     * @param metadataID Actual metadataID
     */
    @Override
    public void setMetadataID(Object metadataID) {
    }

    /**
     * Get element symbol representing this parameter.  The symbol will have the
     * same name and type as the parameter.
     * @return Element symbol representing the parameter
     */
    @Override
    public ElementSymbol getParameterSymbol() {
        return null;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        SPParameter other = (SPParameter)obj;
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

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.getExpression() == null) ? 0 : this.getExpression().hashCode());
        result = prime * result + this.getIndex();
        result = prime * result + ((this.getTeiidParser() == null) ? 0 : this.getTeiidParser().hashCode());
        return result;
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

    /**
     * @return usingDefault
     */
    public boolean isUsingDefault() {
        return false;
        
    }

    /**
     * @param usingDefault
     */
    public void setUsingDefault(boolean usingDefault) {
        
    }

    /**
     * @param varArg
     */
    public void setVarArg(boolean varArg) {
        
    }

    /**
     * @return varArg
     */
    public boolean isVarArg() {
        return false;
        
    }

    @Override
    public SPParameter clone() {
        SPParameter clone = new SPParameter(this.getTeiidParser(), getIndex(), getExpression());
        clone.setParameterType(getParameterType());

        if (getExpression() != null) {
            clone.setExpression(getExpression().clone());
        }
        if (this.getResultSetColumns() != null) {
            Iterator<ElementSymbol> iter = this.getResultSetColumns().iterator();
            Iterator<Object> idIter = this.getResultSetIDs().iterator();
            while (iter.hasNext()) {
                ElementSymbol column = iter.next();
                clone.addResultSetColumn(column.getName(), column.getType(), idIter.next());
            }
        }
        clone.setUsingDefault(isUsingDefault());
        clone.setVarArg(isVarArg());
        return clone;
    }
}
