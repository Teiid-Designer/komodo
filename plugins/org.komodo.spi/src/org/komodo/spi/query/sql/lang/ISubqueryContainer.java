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



/**
 * This interface defines a common interface for all SQL objects 
 * that contain subqueries. 
 * 
 * @param <C> 
 * @param <LV> 
 */
public interface ISubqueryContainer<C extends ICommand> {

    /**
     * Returns the subquery Command object
     * @return the subquery Command object
     */
    C getCommand();
    
    /**
     * Sets the subquery Command object
     * @param command the subquery Command object
     */
    void setCommand(C command);
}
