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

import java.util.Collection;

import org.komodo.spi.query.sql.ILanguageVisitor;

/**
 *
 */
public interface ISetCriteria<E extends IExpression, LV extends ILanguageVisitor>
    extends IPredicateCriteria<LV> {

    /**
     * Get the expression
     * 
     * @return expression
     */
    E getExpression();

    /**
     * Set the expression
     * 
     * @param expression
     */
    void setExpression(E expression);

    /**
     * Returns the set of values.  Returns an empty collection if there are
     * currently no values.
     * 
     * @return The collection of Expression values
     */
    Collection<E> getValues();
      
    /**
     * Sets the values in the set.
     * 
     * @param values The set of value Expressions
     */
    void setValues(Collection<E> values);

    /**
     * Inverse the set criteria
     * 
     * @param value
     */
    void setNegated(boolean value);

}
