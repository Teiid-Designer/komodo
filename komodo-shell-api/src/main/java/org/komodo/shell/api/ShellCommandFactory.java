/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.shell.api;

import java.util.Set;

/**
 *
 */
public interface ShellCommandFactory {

    /**
     * @return an unmodifiable collection of command providers (never <code>null</code>)
     */
    Set< ShellCommandProvider > getCommandProviders();

    /**
     * @param commandName
     *        the name of the available command being requested (cannot be empty)
     * @return the specified command or a "command not found" command (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    ShellCommand getCommand( final String commandName ) throws Exception;

    /**
     * @param commandName
     *        the name of the command not found (cannot be empty)
     * @return the "command not found" command (never <code>null</code>)
     */
    ShellCommand createCommandNotFound( final String commandName );

    /**
     * @return a sorted collection of the valid command names for the current context (never <code>null</code>)
     */
    Set< String > getCommandNamesForCurrentContext();

    /**
     * @return a sorted collection of the valid command names for the current context (never <code>null</code>)
     */
    Set< ShellCommand > getCommandsForCurrentContext();

}
