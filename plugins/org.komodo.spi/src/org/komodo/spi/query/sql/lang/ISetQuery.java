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

import java.util.List;

import org.komodo.spi.query.sql.ILanguageVisitor;

/**
 *
 */
public interface ISetQuery<QC extends IQueryCommand, 
                                                O extends IOrderBy,
                                                Q extends IQuery,
                                                E extends IExpression, 
                                                LV extends ILanguageVisitor> extends IQueryCommand<O, Q, E, LV> {

    /**
     * Enumerator of types of operation
     */
    public enum Operation {
        /** Represents UNION of two queries */
        UNION,
        /** Represents intersection of two queries */
        INTERSECT,
        /** Represents set difference of two queries */
        EXCEPT
    }
    
    /**
     * Is an all query
     * 
     * @return true if all
     */
    boolean isAll();
    
    /**
     * Set flag that this is an all query
     * 
     * @param value
     */
    void setAll(boolean value);

    /**
     * Get operation for this set
     * 
     * @return Operation as defined in this class
     */
    Operation getOperation();

    /**
     * Get left side of the query
     * 
     * @return left part of query
     */
    QC getLeftQuery();

    /**
     * Set the left side of the query
     * 
     * @param query
     */
    void setLeftQuery(QC query);

    /**
     * Get right side of the query
     * 
     * @return right part of query
     */
    QC getRightQuery();

    /**
     * Set the right side of the query
     * 
     * @param query
     */
    void setRightQuery(QC query);

    /**
     * @return the left and right queries as a list.  This list cannot be modified.
     */
    List<QC> getQueryCommands();

}
