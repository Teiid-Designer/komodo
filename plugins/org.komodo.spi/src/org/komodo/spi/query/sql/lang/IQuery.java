/*************************************************************************************
 * JBoss, Home of Professional Open Source.
* See the COPYRIGHT.txt file distributed with this work for information
* regarding copyright ownership. Some portions may be licensed
* to Red Hat, Inc. under one or more contributor license agreements.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
* 02110-1301 USA.
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
