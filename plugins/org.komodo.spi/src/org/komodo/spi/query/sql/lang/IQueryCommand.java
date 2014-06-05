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
public interface IQueryCommand<O extends IOrderBy, Q extends IQuery, E extends IExpression, LV extends ILanguageVisitor>
    extends ICommand<E, LV> {

    /**
     * Get the order by clause for the query.
     * 
     * @return order by clause
     */
    O getOrderBy();
    
    /**
     * Set the order by clause for the query.
     * 
     * @param orderBy New order by clause
     */
    void setOrderBy(O orderBy);
    
    /**
     * Get the query
     * 
     * @return query
     */
    Q getProjectedQuery();
}
