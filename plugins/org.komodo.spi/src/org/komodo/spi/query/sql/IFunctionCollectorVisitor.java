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
import org.komodo.spi.query.sql.symbol.IFunction;

/**
 *
 */
public interface IFunctionCollectorVisitor<LO extends ILanguageObject, F extends IFunction> {

    /**
     * Get the functions from the given language object
     * 
     * @param obj Language object
     * @param removeDuplicates remove duplicates
     * @param deep collect deeply
     * 
     * @return collection of functions
     */
    Collection<F> findFunctions(LO obj, boolean deep);
}
