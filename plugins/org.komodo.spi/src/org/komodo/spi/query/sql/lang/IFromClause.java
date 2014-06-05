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
public interface IFromClause<LV extends ILanguageVisitor> extends ILanguageObject<LV> {

    /**
     * Is the clause optional
     * 
     * @return true if optional
     */
    boolean isOptional();
    
    /**
     * Set whether the clause is optional
     * 
     * @param optional
     */
    void setOptional(boolean optional);
    
    /**
     * Is make dependent
     * 
     * @return true if make dependent
     */
    boolean isMakeDep();

    /**
     * Set make dependent
     * 
     * @param makeDep
     */
    void setMakeDep(boolean makeDep);
    
    /**
     * Is make not dependent
     * 
     * @return true if make not dependent
     */
    boolean isMakeNotDep();
    
    /**
     * Set make not dependent
     * 
     * @param makeNotDep
     */
    void setMakeNotDep(boolean makeNotDep);
}
