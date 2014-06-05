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
package org.komodo.spi.query.sql.symbol;

import org.komodo.spi.query.sql.ILanguageVisitor;
import org.komodo.spi.query.sql.lang.ILanguageObject;


/**
 *
 */
public interface ISymbol <LV extends ILanguageVisitor> extends ILanguageObject<LV> {

    /**
     * Character used to delimit name components in a symbol
     */
    String SEPARATOR = "."; //$NON-NLS-1$
    
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
