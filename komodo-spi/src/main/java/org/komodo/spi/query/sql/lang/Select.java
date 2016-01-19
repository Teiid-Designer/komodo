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

import org.komodo.spi.query.sql.LanguageVisitor;

/**
 *
 */
public interface Select<E extends LanguageObject, LV extends LanguageVisitor> 
    extends LanguageObject<LV> {

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
