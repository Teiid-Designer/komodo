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
 * @param <E> 
 * @param <LV> 
 */
public interface ICommand<E extends IExpression, LV extends ILanguageVisitor> 
    extends ILanguageObject<LV>{
    
    /** 
     * Represents an unknown type of command 
     */
    public static final int TYPE_UNKNOWN = 0;
    
    /**
     * Represents a SQL SELECT statement
     */
    public static final int TYPE_QUERY = 1;
    
    /**
     * Represents a SQL INSERT statement
     */
    public static final int TYPE_INSERT = 2;

    /**
     * Represents a SQL UPDATE statement
     */
    public static final int TYPE_UPDATE = 3;

    /**
     * Represents a SQL DELETE statement
     */
    public static final int TYPE_DELETE = 4;

    /**
     * Represents a stored procedure command
     */
    public static final int TYPE_STORED_PROCEDURE = 6;
    
    /**
     * Represents a update stored procedure command
     */
    public static final int TYPE_UPDATE_PROCEDURE = 7;

    /**
     * Represents a batched sequence of UPDATE statements
     */
    public static final int TYPE_BATCHED_UPDATE = 9;
    
    public static final int TYPE_DYNAMIC = 10;
    
    public static final int TYPE_CREATE = 11;
    
    public static final int TYPE_DROP = 12;
    
    public static final int TYPE_TRIGGER_ACTION = 13;
    
    public static final int TYPE_ALTER_VIEW = 14;
    
    public static final int TYPE_ALTER_PROC = 15;
    
    public static final int TYPE_ALTER_TRIGGER = 16;

    /**
     * type of command
     * 
     * @return int value signifying type of command
     */
    int getType();

    /**
     * @return
     */
    IOption getOption();

    /**
     * @return
     */
    List<E> getProjectedSymbols();
    
    /**
     * @return
     */
    List<? extends E> getResultSetColumns();

    /**
     * @return
     */
    boolean isResolved();

}
