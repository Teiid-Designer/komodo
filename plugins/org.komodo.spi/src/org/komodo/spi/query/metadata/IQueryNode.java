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
package org.komodo.spi.query.metadata;

import java.util.List;

import org.komodo.spi.query.sql.lang.ICommand;


/**
 *
 */
public interface IQueryNode {

    /**
     * @param binding
     */
    void addBinding(String binding);

    /**
     * @return list of bindings
     */
    List<String> getBindings();

    /**
     * @return command
     */
    ICommand getCommand();

    /**
     * @return sql of the query
     */
    String getQuery();

}
