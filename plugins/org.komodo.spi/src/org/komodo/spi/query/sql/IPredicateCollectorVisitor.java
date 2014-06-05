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

import org.komodo.spi.query.sql.lang.ICriteria;
import org.komodo.spi.query.sql.lang.ILanguageObject;

/**
 *
 */
public interface IPredicateCollectorVisitor<LO extends ILanguageObject, C extends ICriteria> {
    
    /**
     * Get the predicates from obj
     * 
     * @param obj Language object
     * 
     * @return collection of criteria 
     */
    Collection<C> findPredicates(LO obj);

}
