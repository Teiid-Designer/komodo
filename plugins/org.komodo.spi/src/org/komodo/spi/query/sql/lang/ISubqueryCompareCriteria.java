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
public interface ISubqueryCompareCriteria<LV extends ILanguageVisitor, C extends ICommand>
    extends IPredicateCriteria<LV>, ISubqueryContainer<C>{

    /** "Some" predicate quantifier (equivalent to "Any") */
    public static final int SOME = 2;

    /** "Any" predicate quantifier (equivalent to "Some") */
    public static final int ANY = 3;

    /** "All" predicate quantifier */
    public static final int ALL = 4;
   
}
