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

import java.util.List;

import org.komodo.spi.query.sql.LanguageVisitor;

/**
 * @param <E> 
 * @param <LV> 
 */
public interface ICommand<E extends IExpression, LV extends LanguageVisitor> 
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
