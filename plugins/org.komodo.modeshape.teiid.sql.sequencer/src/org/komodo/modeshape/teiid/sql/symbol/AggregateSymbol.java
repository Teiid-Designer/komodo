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

package org.komodo.modeshape.teiid.sql.symbol;

import org.komodo.modeshape.teiid.cnd.TeiidSqlLexicon;
import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.ITeiidParser;
import org.komodo.modeshape.teiid.sql.lang.Criteria;
import org.komodo.modeshape.teiid.sql.lang.OrderBy;
import org.komodo.spi.query.sql.symbol.IAggregateSymbol;
import org.komodo.spi.type.DataTypeManager.DataTypeName;

/**
 *
 */
public class AggregateSymbol extends Function implements IAggregateSymbol<LanguageVisitor> {

    /**
     * @param p teiid parser
     * @param id node type id
     */
    public AggregateSymbol(ITeiidParser p, int id) {
        super(p, id);
    }

    private void calculateType() {
        IAggregateSymbol.Type aggregateFunction = getAggregateFunction();
        if (aggregateFunction == null)
            return;

        switch (aggregateFunction) {
            case COUNT:
                assignDataTypeName(getDataTypeService().getCountType(), false);
                break;
            case SUM:
                if (this.getArg(0) != null) {
                    Class<?> expressionType = this.getArg(0).getType();
                    DataTypeName dataTypeName = getDataTypeService().retrieveDataTypeName(expressionType);
                    assignDataTypeName(getDataTypeService().getSumReturnType(dataTypeName), false);
                }
                break;
            case AVG:
                if (this.getArg(0) != null) {
                    Class<?> expressionType = this.getArg(0).getType();
                    DataTypeName dataTypeName = getDataTypeService().retrieveDataTypeName(expressionType);
                    assignDataTypeName(getDataTypeService().getAverageReturnType(dataTypeName), false);
                }
                break;
            case ARRAY_AGG:
                if (this.getArg(0) != null) {
                    Class<?> expressionType = this.getArg(0).getType();
                    DataTypeName dataTypeName = getDataTypeService().retrieveDataTypeName(expressionType);
                    assignDataTypeName(dataTypeName, true);
                }
                break;
            case TEXTAGG:
                assignDataTypeName(DataTypeName.BLOB, false);
                break;
            case USER_DEFINED:
                // TODO need to consider if we ever need to define this.
            case JSONARRAY_AGG:
                assignDataTypeName(DataTypeName.CLOB, false);
                break;
            case STRING_AGG:
                setType(super.getType());
                break;
            case EVERY:
            case SOME:
            case ANY:
                assignDataTypeName(DataTypeName.BOOLEAN, false);
                break;
            case STDDEV_POP: 
            case STDDEV_SAMP:
            case VAR_SAMP:
            case VAR_POP:
                assignDataTypeName(DataTypeName.DOUBLE, false);
                break;
            case RANK:
            case ROW_NUMBER:
            case DENSE_RANK:
                assignDataTypeName(DataTypeName.INTEGER, false);
                break;
            default:
                // ignore and carry on
        }

        if (this.getArgs().length == 0) {
            return;
        }

        setType(this.getArg(0).getType());
    }

    @Override
    public void setName(String name) {
        super.setName(name);
        if (getAggregateFunction() == null) {
            Type aggregateFunction = Type.findAggregateFunction(name);
            setAggregateFunction(aggregateFunction == null ? Type.USER_DEFINED : aggregateFunction);
        }
    }

    @Override
    public Type getAggregateFunction() {
        Object property = getProperty(TeiidSqlLexicon.AggregateSymbol.AGGREGATE_FUNCTION_PROP_NAME);
        return property == null ? null : Type.findAggregateFunction(property.toString());
    }

    public void setAggregateFunction(String name) {
        IAggregateSymbol.Type funcType = IAggregateSymbol.Type.findAggregateFunction(name);
        if (funcType == null)
            funcType = Type.USER_DEFINED;

        setProperty(TeiidSqlLexicon.AggregateSymbol.AGGREGATE_FUNCTION_PROP_NAME, funcType.name());
        calculateType();
    }

    @Override
    public void setAggregateFunction(Type aggregateFunction) {
        setAggregateFunction(aggregateFunction.name());
    }

    public OrderBy getOrderBy() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.AggregateSymbol.ORDER_BY_REF_NAME, OrderBy.class);
    }

    public void setOrderBy(OrderBy orderBy) {
        setChild(TeiidSqlLexicon.AggregateSymbol.ORDER_BY_REF_NAME, orderBy);
    }

    public Criteria getCondition() {
        return getChildforIdentifierAndRefType(TeiidSqlLexicon.AggregateSymbol.CONDITION_REF_NAME, Criteria.class);
    }

    public void setCondition(Expression condition) {
        setChild(TeiidSqlLexicon.AggregateSymbol.CONDITION_REF_NAME, condition);
    }

    public boolean isDistinct() {
        Object property = getProperty(TeiidSqlLexicon.AggregateSymbol.DISTINCT_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setDistinct(boolean distinct) {
        setProperty(TeiidSqlLexicon.AggregateSymbol.DISTINCT_PROP_NAME, distinct);
    }

    @Override
    public void setArgs(Expression[] args) {
        super.setArgs(args);
        calculateType();
    }

    public boolean isWindowed() {
        Object property = getProperty(TeiidSqlLexicon.AggregateSymbol.WINDOWED_PROP_NAME);
        return property == null ? false : Boolean.parseBoolean(property.toString());
    }

    public void setWindowed(boolean windowed) {
        setProperty(TeiidSqlLexicon.AggregateSymbol.WINDOWED_PROP_NAME, windowed);
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = super.hashCode();
        result = prime * result + ((this.getAggregateFunction() == null) ? 0 : this.getAggregateFunction().hashCode());
        result = prime * result + ((this.getCondition() == null) ? 0 : this.getCondition().hashCode());
        result = prime * result + (this.isDistinct() ? 1231 : 1237);
        result = prime * result + (this.isWindowed() ? 1231 : 1237);
        result = prime * result + ((this.getOrderBy() == null) ? 0 : this.getOrderBy().hashCode());
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
        AggregateSymbol other = (AggregateSymbol)obj;
        if (this.getAggregateFunction() != other.getAggregateFunction())
            return false;
        if (this.getCondition() == null) {
            if (other.getCondition() != null)
                return false;
        } else if (!this.getCondition().equals(other.getCondition()))
            return false;
        if (this.isDistinct() != other.isDistinct())
            return false;
        if (this.isWindowed() != other.isWindowed())
            return false;
        if (this.getOrderBy() == null) {
            if (other.getOrderBy() != null)
                return false;
        } else if (!this.getOrderBy().equals(other.getOrderBy()))
            return false;
        return true;
    }

    /** Accept the visitor. **/
    @Override
    public void acceptVisitor(LanguageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public AggregateSymbol clone() {
        AggregateSymbol clone = new AggregateSymbol(this.getTeiidParser(), this.getId());

        if (getAggregateFunction() != null)
            clone.setAggregateFunction(getAggregateFunction());
        if (getAggregateFunction() != null)
            clone.setAggregateFunction(getAggregateFunction());
        clone.setDistinct(isDistinct());
        if (getOrderBy() != null)
            clone.setOrderBy(getOrderBy().clone());
        if (getCondition() != null)
            clone.setCondition(getCondition().clone());
        clone.setWindowed(isWindowed());
        if (getName() != null)
            clone.setName(getName());
        if (getArgs() != null) {
            Expression[] cloned = new Expression[getArgs().length];
            for (int i = 0; i < getArgs().length; ++i) {
                cloned[i] = getArgs()[i].clone();
            }
            clone.setArgs(cloned);
        }
        if (getType() != null)
            clone.setType(getType());

        return clone;
    }

}
