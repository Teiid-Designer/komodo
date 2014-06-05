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
public interface ISelect<E extends ILanguageObject, LV extends ILanguageVisitor> 
    extends ILanguageObject<LV> {

    /**
     * Returns an ordered list of the symbols in the select.
     * 
     * @return list of SelectSymbol in SELECT
     */
    List<E> getSymbols();

    /**
     * Sets an ordered list of the symbols in the select.
     * 
     * @param symbols list of SelectSymbol in SELECT
     */
    void setSymbols(List<? extends E> symbols);
    
    /**
     * Add a symbol to this select
     * 
     * @param expression
     */
    void addSymbol(E expression);

    /**
     * Is the select a 'SELECT *'
     * 
     * @return true if a select wildcard
     */
    boolean isStar();

    /**
     * Checks whether the select is distinct
     * 
     * @return True if select is distinct
     */
    boolean isDistinct();
      
    /**
     * Set whether select is distinct.
     * 
     * @param isDistinct True if SELECT is distinct
     */
    void setDistinct(boolean isDistinct);
}
