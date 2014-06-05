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
public interface ICompoundCriteria<C extends ICriteria, LV extends ILanguageVisitor> 
    extends ILogicalCriteria<LV> {

    /** Constant indicating the logical "or" of two or more criteria. */
    int OR = 1;

    /** Constant indicating the logical "and" of two or more criteria.*/
    int AND = 0;
        
    /**
     * Returns the list of criteria.
     * 
     * @return List of {@link ICriteria}
     */
    List<C> getCriteria();
    
    /**
     * Get the number of {@link ICriteria}
     * 
     * @return count of criteria
     */
    int getCriteriaCount();

    /**
     * Get the {@link ICriteria} at given index
     * 
     * @param index
     * 
     * @return criteria
     */
    C getCriteria(int index);

    /**
     * Add a criteria
     * 
     * @param criteria
     */
    void addCriteria(C criteria);
    
    /**
     * Get the logical operator
     * 
     * @return int of either AND or OR
     */
    int getOperator();

}
