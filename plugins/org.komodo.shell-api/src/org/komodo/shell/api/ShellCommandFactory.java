/*
 * JBoss, Home of Professional Open Source.
*
* See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
*
* See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
*/
package org.komodo.shell.api;

import java.util.Collection;
import java.util.List;

/**
 *
 */
public interface ShellCommandFactory {

    /**
     * @return the command providers (never <code>null</code>)
     */
    Collection<ShellCommandProvider> getCommandProviders();
    
    /**
     * @param commandName
     *        the name of the available command being requested (cannot be empty)
     * @return the specified command or a "command not found" command (never <code>null</code>)
     * @throws Exception if an error occurs
     */
    ShellCommand getCommand( final String commandName ) throws Exception;
    
    /**
     * Get valid command names for the current context.
     *
     * @return a list of commands for current context (never <code>null</code>)
     * @throws Exception
     *         if error occurs
     */
    List< String > getCommandNamesForCurrentContext() throws Exception;
    
    /**
     * Registers all built-in and discovered commands.
     *
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public void registerCommands( final WorkspaceStatus wsStatus ) throws Exception;

}
