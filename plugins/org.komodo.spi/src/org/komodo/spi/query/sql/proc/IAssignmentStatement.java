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
package org.komodo.spi.query.sql.proc;

import org.komodo.spi.query.sql.ILanguageVisitor;
import org.komodo.spi.query.sql.lang.IExpression;
import org.komodo.spi.query.sql.symbol.IElementSymbol;

/**
 *
 */
public interface IAssignmentStatement<E extends IExpression, LV extends ILanguageVisitor>
    extends IStatement<LV>, IExpressionStatement<E> {

    /**
     * Get the expression giving the value that is assigned to the variable.
     * 
     * @return An <code>Expression</code> with the value
     */
    IElementSymbol getVariable();
    
    /**
     * Get the value of the statement
     */
    E getValue();
    
    /**
     * Set the value of the statement
     * 
     * @param value
     */
    void setValue(E value);
}
