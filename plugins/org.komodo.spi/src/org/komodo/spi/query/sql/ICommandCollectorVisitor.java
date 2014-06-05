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
package org.komodo.spi.query.sql;

import java.util.List;

import org.komodo.spi.query.sql.lang.ICommand;

/**
 *
 */
public interface ICommandCollectorVisitor<C extends ICommand> {

    /**
     * Retrieve the commands from the given command
     * 
     * @param command
     * 
     * @return list of all sub commands
     */
    List<C> findCommands(C command);
}
