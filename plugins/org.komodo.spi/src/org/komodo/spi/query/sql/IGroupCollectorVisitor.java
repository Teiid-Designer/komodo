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
import org.komodo.spi.query.sql.symbol.IGroupSymbol;

/**
 *
 */
public interface IGroupCollectorVisitor<LO extends ILanguageObject, GS extends IGroupSymbol> {

    /**
     * Get the groups from obj in a collection.  The
     * removeDuplicates flag affects whether duplicate groups will be
     * filtered out.
     * 
     * @param obj Language object
     * @return Collection of {@link IGroupSymbol}
     */
    Collection<GS> findGroups(LO obj);
    
    /**
     * Get the groups from obj in a collection.  The 
     * removeDuplicates flag affects whether duplicate groups will be 
     * filtered out.
     * 
     * @param obj Language object
     * @return Collection of {@link IGroupSymbol}
     */
    Collection<GS> findGroupsIgnoreInlineViews(LO obj);
}
