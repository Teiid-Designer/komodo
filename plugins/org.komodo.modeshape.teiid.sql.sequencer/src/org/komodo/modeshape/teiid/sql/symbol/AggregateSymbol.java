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

import org.komodo.modeshape.teiid.parser.LanguageVisitor;
import org.komodo.modeshape.teiid.parser.TeiidParser;
import org.komodo.modeshape.teiid.sql.lang.Criteria;
import org.komodo.modeshape.teiid.sql.lang.OrderBy;
import org.komodo.spi.query.sql.lang.IExpression;
import org.komodo.spi.query.sql.symbol.IAggregateSymbol;

public class AggregateSymbol extends Function implements IAggregateSymbol<LanguageVisitor> {

    public AggregateSymbol(TeiidParser p, int id) {
        super(p, id);
    }

    @Override
    public <T> Class<T> getType() {
        return null;
    }

    @Override
    public FunctionDescriptor getFunctionDescriptor() {
        return null;
    }

    @Override
    public void setFunctionDescriptor(FunctionDescriptor fd) {
    }

    @Override
    public org.komodo.spi.query.sql.symbol.IAggregateSymbol.Type getAggregateFunction() {
        return null;
    }

    /**
     * @param name
     */
    public void setAggregateFunction(String name) {
    }

    @Override
    public void setAggregateFunction(org.komodo.spi.query.sql.symbol.IAggregateSymbol.Type aggregateFunction) {
    }

    /**
     * @return
     */
    public OrderBy getOrderBy() {
        return null;
    }

    /**
     * @param orderBy
     */
    public void setOrderBy(OrderBy orderBy) {
    }

    /**
     * @return
     */
    public Criteria getCondition() {
        return null;
    }

    /**
     * @param condition
     */
    public void setCondition(Expression condition) {
    }

    /**
     * @return
     */
    public boolean isDistinct() {
        return false;
    }

    /**
     * @param b
     */
    public void setDistinct(boolean b) {
    }

    @Override
    public Expression[] getArgs() {
        return null;
    }

    @Override
    public Expression getArg(int index) {
        return null;
    }

    /**
     * @param args
     */
    public void setArgs(IExpression[] args) {
    }

    @Override
    public boolean isImplicit() {
        return false;
    }

    public void setImplicit(boolean b) {

    }

    @Override
    public void setType(Class<?> type) {
    }

    /**
     * @return
     */
    public boolean isWindowed() {
        return false;
    }

    /**
     * @param windowed
     */
    public void setWindowed(boolean windowed) {
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
