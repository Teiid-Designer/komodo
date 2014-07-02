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
package org.komodo.spi.query.sql.lang;

import org.komodo.spi.query.sql.ILanguageVisitor;


/**
 *
 */
public interface ICompareCriteria<E extends IExpression, LV extends ILanguageVisitor>
    extends IPredicateCriteria<LV> {

    /** Constant indicating the two operands are equal. */
    int EQ = 1;

    /** Constant indicating the two operands are not equal. */
    int NE = 2;

    /** Constant indicating the first operand is less than the second. */
    int LT = 3;

    /** Constant indicating the first operand is greater than the second. */
    int GT = 4;

    /** Constant indicating the first operand is less than or equal to the second. */
    int LE = 5;

    /** Constant indicating the first operand is greater than or equal to the second. */
    int GE = 6;

    /**
     * Returns the operator.
     * @return The operator
     */
    int getOperator();

    /**
     * Set the operator
     * 
     * @param operator
     */
    void setOperator(int operator);
    
    /**
     * Get the string version of the operator
     * 
     * @return operator string
     */
    String getOperatorAsString();
    
    /**
     * Get left expression.
     * @return Left expression
     */
    E getLeftExpression();

    /**
     * Set the left expression
     * 
     * @param expression
     */
    void setLeftExpression(E expression);
    
    /**
     * Get right expression.
     * 
     * @return Right expression
     */
    E getRightExpression();

    /**
     * Set the right expression
     * 
     * @param expression
     */
    void setRightExpression(E expression);
    
}
