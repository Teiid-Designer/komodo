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
package org.komodo.spi.query;


import org.komodo.spi.query.sql.lang.Command;
import org.komodo.spi.query.sql.lang.Criteria;
import org.komodo.spi.query.sql.lang.Expression;


/**
 *
 */
public interface QueryParser {

    /**
     * Parse the given criteria string
     * 
     * @param criteriaString
     * 
     * @return  an instance of {@link Criteria}
     * @throws Exception 
     */
    Criteria parseCriteria(String criteriaString) throws Exception;

    /**
     * Parse the given command string
     * 
     * @param commandString
     * 
     * @return an instance of {@link Command}
     * @throws Exception 
     */
    Command parseCommand(String commandString) throws Exception;
    
    /**
     * Parse the given command string
     * 
     * @param commandString
     * 
     * @return an instance of {@link Command}
     * @throws Exception 
     */
    Command parseDesignerCommand(String commandString) throws Exception;

    /**
     * Parse the given expression string
     * 
     * @param expressionString
     * 
     * @return an instance of {@link Expression}
     * @throws Exception 
     */
    Expression parseExpression(String expressionString) throws Exception;

}
