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
public interface ISubqueryFromClause<LV extends ILanguageVisitor, C extends ICommand>
    extends IFromClause<LV>, ISubqueryContainer<C> {

    /**
     * Get name of this clause.
     * 
     * @return Name of clause
     */
    String getName();
    
    /** 
     * Reset the alias for this subquery from clause and it's pseudo-GroupSymbol.  
     * WARNING: this will modify the hashCode and equals semantics and will cause this object
     * to be lost if currently in a HashMap or HashSet.
     * 
     * @param name New name
     */
    void setName(String name);
}
