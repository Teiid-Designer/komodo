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
