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

import java.util.Collection;
import java.util.List;

import org.komodo.spi.query.sql.ILanguageVisitor;
import org.komodo.spi.query.sql.symbol.IElementSymbol;
import org.komodo.spi.query.sql.symbol.IGroupSymbol;


/**
 *
 */
public interface IInsert<ES extends IElementSymbol,
                                           E extends IExpression, 
                                           G extends IGroupSymbol, 
                                           Q extends IQueryCommand,
                                           LV extends ILanguageVisitor> extends IProcedureContainer<E, LV> {

    /**
     * Returns the group being inserted into
     * 
     * @return Group being inserted into
     */
    G getGroup();
    
    /**
     * Set the group for this insert statement
     * 
     * @param group Group to be inserted into
     */
    void setGroup(G group);
    
    /**
     * Return an ordered List of variables, may be null if no columns were specified
     * 
     * @return List of {@link IElementSymbol}
     */
    List<ES> getVariables();
    
    /**
     * Add a variable to end of list
     * 
     * @param symbol Variable to add to the list
     */
    void addVariable(ES symbol);
    
    /**
     * Add a collection of variables to end of list
     * 
     * @param symbols Variables to add to the list - collection of ElementSymbol
     */
    void addVariables(Collection<ES> symbols);
    
    /**
     * Returns a list of values to insert
     * to be inserted.
     * 
     * @return List of {@link IExpression}s
     */
    List<E> getValues();
    
    /**
     * Sets the values to be inserted.
     * 
     * @param values List of {@link IExpression}s
     */
    void setValues(List<? extends E> values);
    
    /**
     * Set a collection of variables that replace the existing variables
     * 
     * @param vars Variables to be set on this object (ElementSymbols)
     */
    void setVariables(Collection<ES> vars);

    /**
     * Get the query expression
     * 
     * @return query expression
     */
    Q getQueryExpression();
    
}
