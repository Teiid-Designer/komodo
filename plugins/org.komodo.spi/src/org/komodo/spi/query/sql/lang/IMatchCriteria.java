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
public interface IMatchCriteria<E extends IExpression, LV extends ILanguageVisitor>
    extends IPredicateCriteria<LV> {

    /** The internal null escape character */
    public static final char NULL_ESCAPE_CHAR = 0;

    public enum MatchMode {
        LIKE,
        SIMILAR,
        /**
         * The escape char is typically not used in regex mode.
         */
        REGEX
    }
    
    /**
     * Get the left expression
     * 
     * @return expression
     */
    E getLeftExpression();
    
    /**
     * Set the left expression
     * 
     * @param expression
     */
    void setLeftExpression(E expression);

    /**
     * Get the right expression
     * 
     * @return expression
     */
    E getRightExpression();
    
    /**
     * Set the right expression
     * 
     * @param expression
     */
    void setRightExpression(E expression);

    /**
     * Get the escape character
     * 
     * @return escape character
     */
    char getEscapeChar();
    
    /**
     * Set the escape character
     * 
     * @param escapeChar
     */
    void setEscapeChar(char escapeChar);

    /**
     * Has this been negated
     * 
     * @return true if negated
     */
    boolean isNegated();
    
    /**
     * Inverse the match
     * 
     * @param value
     */
    void setNegated(boolean value);

    /**
     * Get the mode
     * 
     * @return mode
     */
    MatchMode getMode();
    
}
