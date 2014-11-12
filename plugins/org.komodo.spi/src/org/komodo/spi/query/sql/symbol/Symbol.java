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
package org.komodo.spi.query.sql.symbol;

import org.komodo.spi.constants.StringConstants;
import org.komodo.spi.query.sql.LanguageVisitor;
import org.komodo.spi.query.sql.lang.LanguageObject;


/**
 *
 */
public interface Symbol <LV extends LanguageVisitor> extends LanguageObject<LV>, StringConstants {

    /**
     * Character used to delimit name components in a symbol
     */
    String SEPARATOR = DOT; 
    
    /**
     * Get the name of the symbol
     * 
     * @return Name of the symbol, never null
     */
    String getName();
    
    /**
     * Get the short name of the element
     * 
     * @return Short name of the symbol (un-dotted)
     */
    String getShortName();
    
    /**
     * Change the symbol's name.  This will change the symbol's hash code
     * and canonical name!!!!!!!!!!!!!!!!!  If this symbol is in a hashed
     * collection, it will be lost!
     * 
     * @param name
     */
    void setShortName(String name);
    
    /**
     * Get the output name
     * 
     * @return output name
     */
    String getOutputName();
    
    /**
     * Set the output name
     * 
     * @param outputName
     */
    void setOutputName(String outputName);
}
