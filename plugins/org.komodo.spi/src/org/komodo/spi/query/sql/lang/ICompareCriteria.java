/*************************************************************************************
 * Copyright (c) 2014 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
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
