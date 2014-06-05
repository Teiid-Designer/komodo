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
import org.komodo.spi.query.sql.lang.IExpression;

/**
 *
 */
public interface IAliasSymbol<E extends IExpression, LV extends ILanguageVisitor>
    extends ISymbol<LV>, IExpression<LV> {

    /**
     * Get the underlying symbol
     * 
     * @return Underlying symbol
     */
    E getSymbol();

    /**
     * Set the underlying symbol
     * 
     * @param symbol New symbol
     */
    void setSymbol(E symbol);

}
