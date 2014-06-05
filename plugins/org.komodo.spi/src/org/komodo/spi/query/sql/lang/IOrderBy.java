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

import java.util.List;

import org.komodo.spi.query.sql.ILanguageVisitor;


/**
 *
 */
public interface IOrderBy<E extends IExpression, O extends IOrderByItem, LV extends ILanguageVisitor> 
    extends ILanguageObject<LV> {

    /** Constant for the ascending value */
    public static final boolean ASC = true;

    /** Constant for the descending value */
    public static final boolean DESC = false;
    
    /**
     * Returns the number of elements in ORDER BY.
     * 
     * @return Number of variables in ORDER BY
     */
    int getVariableCount();

    /**
     * Get the order by items
     * 
     * @return list of order by items
     */
    List<O> getOrderByItems();
    
    /**
     * Adds a new variable to the list of order by elements.
     * 
     * @param expression to add
     */
    void addVariable(E expression);

    /**
     * Adds a new variable to the list of order by elements
     * 
     * @param element
     * @param orderType
     */
    void addVariable(E element, boolean orderType);
}
