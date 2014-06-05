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
public interface IBetweenCriteria<LV extends ILanguageVisitor> extends IPredicateCriteria<LV> {

    /**
     * Has this been negated
     * 
     * @return true if negated
     */
    boolean isNegated();
    
    /**
     * Inverse the negation
     * 
     * @param value
     */
    void setNegated(boolean value);

    /**
     * Get expression.
     * 
     * @return Expression to compare
     */
    IExpression getExpression();

    /**
     * Get the lower expression.
     * 
     * @return the lower expression
     */
    IExpression getLowerExpression();
    
    /**
     * Get the upper expression.
     * 
     * @return the upper expression
     */
    IExpression getUpperExpression();

}
