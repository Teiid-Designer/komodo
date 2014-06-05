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
import org.komodo.spi.query.sql.symbol.IElementSymbol;

/**
 *
 */
public interface IGroupBy<E extends IExpression, LV extends ILanguageVisitor> 
    extends ILanguageObject<LV>{

    /**
     * Returns the number of symbols in the GROUP BY
     * 
     * @return Count of the number of symbols in GROUP BY
     */
    int getCount();
    
    /**
     * Returns an ordered list of the symbols in the GROUP BY
     * 
     * @return List of {@link IElementSymbol}s
     */
    List<E> getSymbols();
    
    /**
     * Adds a new symbol to the list of symbols
     * .
     * @param symbol Symbol to add to GROUP BY
     */
    void addSymbol(E symbol);
}
