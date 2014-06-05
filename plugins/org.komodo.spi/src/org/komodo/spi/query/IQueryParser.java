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
package org.komodo.spi.query;


import org.komodo.spi.query.sql.lang.ICommand;
import org.komodo.spi.query.sql.lang.ICriteria;
import org.komodo.spi.query.sql.lang.IExpression;


/**
 *
 */
public interface IQueryParser {

    /**
     * Parse the given criteria string
     * 
     * @param criteriaString
     * 
     * @return  an instance of {@link ICriteria}
     * @throws Exception 
     */
    ICriteria parseCriteria(String criteriaString) throws Exception;

    /**
     * Parse the given command string
     * 
     * @param commandString
     * 
     * @return an instance of {@link ICommand}
     * @throws Exception 
     */
    ICommand parseCommand(String commandString) throws Exception;
    
    /**
     * Parse the given command string
     * 
     * @param commandString
     * 
     * @return an instance of {@link ICommand}
     * @throws Exception 
     */
    ICommand parseDesignerCommand(String commandString) throws Exception;

    /**
     * Parse the given expression string
     * 
     * @param expressionString
     * 
     * @return an instance of {@link IExpression}
     * @throws Exception 
     */
    IExpression parseExpression(String expressionString) throws Exception;

}
