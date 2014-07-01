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
import org.komodo.spi.query.sql.symbol.IGroupSymbol;


/**
 *
 */
public interface IFrom<F extends IFromClause, 
                                        G extends IGroupSymbol,
                                        LV extends ILanguageVisitor> extends ILanguageObject<LV> {

    /** 
     * Get all the clauses in FROM
     * 
     * @return List of {@link IFromClause}
     */
    List<F> getClauses();
    
    /** 
     * Set all the clauses
     * 
     * @param clauses List of {@link IFromClause}
     */
    void setClauses(List<? extends F> clauses);

    /**
     * Add a clause to the FROM
     * 
     * @param clause Add a clause to the FROM
     */
    void addClause(F clause);
    
    /**
     * Adds a new group to the list 
     * (it will be wrapped in a UnaryFromClause)
     * 
     * @param group Group to add
     */
    void addGroup(G group);

    /**
     * Returns an ordered list of the groups in all sub-clauses.
     * 
     * @return List of {@link IGroupSymbol}
     */
    List<? extends G> getGroups();
    
    /**
     * Checks if a group is in the From
     * 
     * @param group Group to check for
     * 
     * @return True if the From contains the group
     */
    boolean containsGroup(G group);

}
