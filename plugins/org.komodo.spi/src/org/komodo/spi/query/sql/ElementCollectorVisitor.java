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
package org.komodo.spi.query.sql;

import java.util.Collection;

import org.komodo.spi.query.sql.lang.LanguageObject;
import org.komodo.spi.query.sql.symbol.ElementSymbol;

/**
 *
 */
public interface ElementCollectorVisitor<LO extends LanguageObject, ES extends ElementSymbol> {

    /**
     * Helper to quickly get the elements from obj in a collection.  The
     * removeDuplicates flag affects whether duplicate elements will be
     * filtered out.
     * 
     * @param obj Language object
     * @return Collection of {@link ElementSymbol}
     */
    Collection<? super ES> findElements(LO obj);
    
    /**
     * Helper to quickly get the elements from obj in a collection.  The
     * removeDuplicates flag affects whether duplicate elements will be
     * filtered out.
     * 
     * @param obj Language object
     * @param useDeepIteration indicates whether or not to iterate into nested
     *                 subqueries of the query 
     * 
     * @return Collection of {@link ElementSymbol}
     */
    Collection<? super ES> findElements(LO obj, boolean useDeepIteration);
    
    /**
     * Helper to quickly get the elements from obj in a collection.  The
     * removeDuplicates flag affects whether duplicate elements will be
     * filtered out.
     * 
     * @param obj Language object
     * @param useDeepIteration indicates whether or not to iterate into nested
     *                 subqueries of the query 
     * @param aggsOnly
     * 
     * @return Collection of {@link ElementSymbol}
     */
    Collection<? super ES> findElements(LO obj, boolean useDeepIteration, boolean aggsOnly);
}
