/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
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
     * @return a sorted collection of the valid command names for the current context (never <code>null</code>)
     * @throws Exception
     *         if error occurs
     */
    Set< String > getCommandNamesForCurrentContext() throws Exception;

}
