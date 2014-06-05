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
public interface IQuery<S extends ISelect, 
                                          F extends IFrom, 
                                          I extends IInto, 
                                          C extends ICriteria, 
                                          G extends IGroupBy,
                                          O extends IOrderBy,
                                          Q extends IQuery,
                                          E extends IExpression, 
                                          LV extends ILanguageVisitor> extends IQueryCommand<O, Q, E, LV> {

    /**
     * Get the select clause for the query.
     * 
     * @return SELECT clause
     */
    S getSelect();

    /**
     * Set the select clause for the query.
     * 
     * @param select SELECT clause
     */
    void setSelect(S select);

    /**
     * Get the from clause for the query.
     * 
     * @return FROM clause
     */
    F getFrom();

    /**
     * Set the from clause for the query.
     * 
     * @param from FROM clause
     */
    void setFrom(F from);

    /**
     * Get the into clause for the query
     * 
     * @return INTO clause
     */
    I getInto();

    /**
     * Set the into clause for the query.
     * 
     * @param into INTO clause
     */
    void setInto(I into);

    /**
     * Get the criteria clause for the query.
     * 
     * @return WHERE clause
     */
    C getCriteria();

    /**
     * Set the where clause
     * 
     * @param where
     */
    void setCriteria(C where);

    /**
     * Get the having clause for the query.
     * 
     * @return HAVING clause
     */
    C getHaving();

    /**
     * Set the having clause
     * 
     * @param having
     */
    void setHaving(C having);

    /**
     * Get the group by
     * 
     * @return GROUPBY clause
     */
    G getGroupBy();

    /**
     * Set the group by clause
     * 
     * @param groupBy
     */
    void setGroupBy(G groupBy);
}
