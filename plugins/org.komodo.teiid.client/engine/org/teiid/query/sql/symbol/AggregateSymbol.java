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
package org.teiid.query.sql.symbol;

import org.komodo.spi.annotation.Removed;
import org.komodo.spi.annotation.Since;
import org.komodo.spi.query.sql.symbol.IAggregateSymbol;
import org.komodo.spi.runtime.version.DefaultTeiidVersion.Version;
import org.teiid.query.function.FunctionDescriptor;
import org.teiid.query.parser.LanguageVisitor;
import org.teiid.query.sql.lang.Node;
import org.teiid.query.sql.lang.OrderBy;
import org.teiid.query.sql.lang.SingleElementSymbol;

/**
 *
 */
@SuppressWarnings( "unused" )
public interface AggregateSymbol extends Node, SingleElementSymbol, Expression, IAggregateSymbol<LanguageVisitor> {

    /**
     * @return name
     */
    String getName();

    /**
     * @param name
     */
    void setName(String name);

    /**
     * @return distinct
     */
    boolean isDistinct();

    /**
     * @param isDistinct
     */
    void setDistinct(boolean isDistinct);

    /**
     * @return true if a boolean type
     */
    boolean isBoolean();

    /**
     * @return true if enhanced numeric
     */
    boolean isEnhancedNumeric();

    /**
     * Get function arguments
     *
     * @return Get function arguments
     */
    @Since(Version.TEIID_8_0)
    Expression[] getArgs();

    /**
     * Get argument at specified index
     * @param index Index of argument
     * @return expression
     */
    @Since(Version.TEIID_8_0)
    Expression getArg(int index);

    /**
     * @param arguments
     */
    @Since(Version.TEIID_8_0)
    void setArgs(Expression[] arguments);

    /**
     * Set type of function
     *
     * @param type New type
     */
    @Since(Version.TEIID_8_0)
    void setType(Class<?> type);

    /**
     * @return order by
     */
    public OrderBy getOrderBy();

    /**
     * @param orderBy
     */
    public void setOrderBy(OrderBy orderBy);

    /**
     * @return condition
     */
    public Expression getCondition();
    
    /**
     * @param condition
     */
    void setCondition(Expression condition);
    
    /**
     * @return isWindowed
     */
    boolean isWindowed();
    
    /**
     * @param isWindowed
     */
    void setWindowed(boolean isWindowed);

    /**
     * @return canonicalName
     */
    @Removed(Version.TEIID_8_0)
    String getCanonicalName();

    /**
     * @param canonicalName
     */
    @Removed(Version.TEIID_8_0)
    void setCanonicalName(String canonicalName);

    /**
     * Get the aggregate function type - this will map to one of the reserved words
     * for the aggregate functions.
     * @return Aggregate function type
     */
    @Override
    Type getAggregateFunction();

    /**
     * @param aggregateFunction
     */
    void setAggregateFunction(String aggregateFunction);

    /**
     * Set the aggregate function.  If the aggregate function is an invalid value, an
     * IllegalArgumentException is thrown.
     * @param aggregateFunction Aggregate function type
     * @see org.teiid.language.SQLConstants.NonReserved#COUNT
     * @see org.teiid.language.SQLConstants.NonReserved#SUM
     * @see org.teiid.language.SQLConstants.NonReserved#AVG
     * @see org.teiid.language.SQLConstants.NonReserved#MIN
     * @see org.teiid.language.SQLConstants.NonReserved#MAX
     */
    @Override
    void setAggregateFunction(Type aggregateFunction);

    /**
     * Get the expression for this symbol
     * @return Expression for this symbol
     */
    @Removed(Version.TEIID_8_0)
    Expression getExpression();

    /**
     * Set the expression represented by this symbol.
     * @param expression Expression for this expression symbol
     */
    @Removed(Version.TEIID_8_0)
    void setExpression(Expression expression);

    /**
     * Get the function descriptor that this function resolves to.
     * @return Descriptor or null if resolution has not yet occurred
     */
    @Since(Version.TEIID_8_0)
    FunctionDescriptor getFunctionDescriptor();

    /**
     * @param functionDescriptor
     */
    @Since(Version.TEIID_8_0)
    void setFunctionDescriptor(FunctionDescriptor functionDescriptor);

    /**
     * Clone this aggregate symbol
     */
    @Override
    AggregateSymbol clone();

}
