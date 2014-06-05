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
package org.komodo.spi.query.sql;

import java.util.Collection;

import org.komodo.spi.query.sql.lang.ILanguageObject;
import org.komodo.spi.query.sql.symbol.IElementSymbol;

/**
 *
 */
public interface IElementCollectorVisitor<LO extends ILanguageObject, ES extends IElementSymbol> {

    /**
     * Helper to quickly get the elements from obj in a collection.  The
     * removeDuplicates flag affects whether duplicate elements will be
     * filtered out.
     * 
     * @param obj Language object
     * @return Collection of {@link IElementSymbol}
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
     * @return Collection of {@link IElementSymbol}
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
     * @return Collection of {@link IElementSymbol}
     */
    Collection<? super ES> findElements(LO obj, boolean useDeepIteration, boolean aggsOnly);
}
