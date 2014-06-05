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
import java.util.Set;

import org.komodo.spi.query.sql.lang.ILanguageObject;
import org.komodo.spi.query.sql.symbol.IGroupSymbol;

/**
 *
 */
public interface IGroupsUsedByElementsVisitor<LO extends ILanguageObject, GS extends IGroupSymbol> {

    /**
     * Find the groups containd in the given object.
     * 
     * Duplicates are removed.
     * 
     * @param object Language object
     * 
     * @return Collection of {@link IGroupSymbol}
     */
    Set<GS> findGroups(LO object);
    
    
    /**
     * Find the groups contains in the collection of given objects.
     * 
     * Duplicates are removed.
     * 
     * @param objects Collection of language objects
     * 
     * @return Collection of {@link IGroupSymbol}
     */
    <T extends LO> Set<GS> findGroups(Collection<T> objects);
}
