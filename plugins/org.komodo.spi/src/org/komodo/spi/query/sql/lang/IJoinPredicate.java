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
public interface IJoinPredicate<T extends IFromClause, LV extends ILanguageVisitor> extends IFromClause<LV> {

    /**
     * Get left clause
     * 
     * @return Left clause
     */
    T getLeftClause();
    
    /**
     * Set left clause 
     * 
     * @param fromClause Left clause to set
     */
    void setLeftClause(T fromClause);
   
    /**
     * Get right clause
     * 
     * @return Right clause
     */
    T getRightClause();
    
    /**
     * Set right clause 
     * 
     * @param fromClause Right clause to set
     */
    void setRightClause(T fromClause);
    
    
}
