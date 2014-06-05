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
public interface IExpressionSymbol<E extends IExpression, LV extends ILanguageVisitor>
    extends ISymbol<LV>, IExpression<LV> {

    /**
     * Get the expression represented by this symbol
     * 
     * @return expression
     */
    E getExpression();
    
    /**
     * Set the expression represented by this symbol.
     * 
     * @param expression Expression for this expression symbol
     */
    void setExpression(E expression);

}
