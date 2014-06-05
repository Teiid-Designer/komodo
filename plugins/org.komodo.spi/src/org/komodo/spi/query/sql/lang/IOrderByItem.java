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

import org.komodo.spi.query.sql.ILanguageVisitor;


/**
 *
 */
public interface IOrderByItem<E extends IExpression, LV extends ILanguageVisitor> 
    extends ILanguageObject<LV> {

    /**
     * Get the symbol value
     * 
     * @return symbol value
     */
    E getSymbol();
    
    /**
     * Set the symbol value
     * 
     * @param symbol
     */
    void setSymbol(E symbol);

    /**
     * Is the order ascending
     * 
     * @return true if ascending
     */
    boolean isAscending();
}
