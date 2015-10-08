/*
 * Copyright 2012 JBoss Inc
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.komodo.shell;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.ServiceLoader;
import java.util.Set;
import org.komodo.core.KEngine;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.CdCommand;
import org.komodo.shell.commands.CommitCommand;
import org.komodo.shell.commands.ExitCommand;
import org.komodo.shell.commands.HelpCommand;
import org.komodo.shell.commands.HomeCommand;
import org.komodo.shell.commands.LibraryCommand;
import org.komodo.shell.commands.ListCommand;
import org.komodo.shell.commands.PlayCommand;
import org.komodo.shell.commands.RenameCommand;
import org.komodo.shell.commands.RollbackCommand;
import org.komodo.shell.commands.SetAutoCommitCommand;
import org.komodo.shell.commands.SetGlobalPropertyCommand;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.shell.commands.SetRecordCommand;
import org.komodo.shell.commands.ShowChildrenCommand;
import org.komodo.shell.commands.ShowGlobalCommand;
import org.komodo.shell.commands.ShowPropertiesCommand;
import org.komodo.shell.commands.ShowPropertyCommand;
import org.komodo.shell.commands.ShowStatusCommand;
import org.komodo.shell.commands.ShowSummaryCommand;
import org.komodo.shell.commands.UnsetPropertyCommand;
import org.komodo.shell.commands.WorkspaceCommand;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;

/**
 * Factory used to create shell commands.
 *
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use WorkspaceStatus.  additional methods added
 * - altered map usage to to change from Shell Context usage
 *
 * @author eric.wittmann@redhat.com
 */
public class ShellCommandFactory {

    private static final String BUILT_IN_PROVIDER_ID = "KOMODO_BUILT_IN"; //$NON-NLS-1$

    private static ShellCommand _commandNotFound;

    // key = command name, value = {key = provider ID, value = command}
    private final Map< String, Map< String, ShellCommand > > commandMap;
    private Collection<ShellCommandProvider> providers = new ArrayList<>();

    /**
     * @throws Exception
     *         if a built-in command cannot be created or if an error occurs
     */
    public ShellCommandFactory( ) throws Exception {
        this.commandMap = new HashMap<>();
        discoverProviders();
    }

    /**
     * @return the command providers (never <code>null</code>)
     */
    public Collection<ShellCommandProvider> getCommandProviders() {
        return providers;
    }

    /**
     * Registers all built-in and discovered commands.
     *
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public void registerCommands( final WorkspaceStatus wsStatus ) throws Exception {
        ArgCheck.isNotNull( wsStatus, "wsStatus" ); //$NON-NLS-1$
        _commandNotFound = new CommandNotFoundCommand( wsStatus );

        // register built-in commands
        registerCommand( BUILT_IN_PROVIDER_ID, CdCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, CommitCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, ExitCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, HelpCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, HomeCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, LibraryCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, ListCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, PlayCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, RollbackCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, RenameCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, ShowChildrenCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, ShowGlobalCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, SetAutoCommitCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, SetGlobalPropertyCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, SetPropertyCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, SetRecordCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, ShowPropertiesCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, ShowPropertyCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, ShowStatusCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, ShowSummaryCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, UnsetPropertyCommand.class, wsStatus );
        registerCommand( BUILT_IN_PROVIDER_ID, WorkspaceCommand.class, wsStatus );

        // register any commands contributed by command providers
        registerContributedCommands( wsStatus );
    }

    private void registerContributedCommands( final WorkspaceStatus wsStatus ) {
        for ( final ShellCommandProvider provider : this.providers ) {
            final Map< String, Class< ? extends ShellCommand > > commands = provider.provideCommands();

            if ( ( commands != null ) && !commands.isEmpty() ) {
                final String providerId = provider.getClass().getName();

                for ( final Map.Entry< String, Class< ? extends ShellCommand > > entry : commands.entrySet() ) {
                    final Class< ? extends ShellCommand > commandClass = entry.getValue();

                    if ( commandClass != null ) {
                        try {
                            registerCommand( providerId, commandClass, wsStatus );
                        } catch ( final Exception e ) {
                            KEngine.getInstance().getErrorHandler().error( e );
                        }
                    }
                }
            }
        }
    }

    private void discoverProviders() {
        final List< ClassLoader > commandClassloaders = new ArrayList< >();
        commandClassloaders.add( Thread.currentThread().getContextClassLoader() );

        // Find providers in the user's commands directory
        final String userHome = System.getProperty( "user.home", "/" ); //$NON-NLS-1$ //$NON-NLS-2$
        final String commandsDirName = System.getProperty( "komodo.shell.commandsDir", userHome + "/.komodo/commands" ); //$NON-NLS-1$ //$NON-NLS-2$
        final File commandsDir = new File( commandsDirName );

        if ( !commandsDir.exists() ) {
            commandsDir.mkdirs();
        }

        if ( commandsDir.isDirectory() ) {
            try {
                final Collection< File > jarFiles = FileUtils.getFilesForPattern( commandsDir.getCanonicalPath(), "", ".jar" ); //$NON-NLS-1$ //$NON-NLS-2$
                final List< URL > jarURLs = new ArrayList< >( jarFiles.size() );

                for ( final File jarFile : jarFiles ) {
                    jarURLs.add( jarFile.toURI().toURL() );
                }

                final URL[] urls = jarURLs.toArray( new URL[ jarURLs.size() ] );
                final ClassLoader extraCommandsCL = new URLClassLoader( urls, Thread.currentThread().getContextClassLoader() );
                commandClassloaders.add( extraCommandsCL );
            } catch ( final IOException e ) {
                KEngine.getInstance().getErrorHandler().error( e );
            }
        }

        // iterate through the ClassLoaders and use the Java ServiceLoader mechanism to load the providers
        for ( final ClassLoader classLoader : commandClassloaders ) {
            for ( final ShellCommandProvider provider : ServiceLoader.load( ShellCommandProvider.class, classLoader ) ) {
                if ( !Modifier.isAbstract( provider.getClass().getModifiers() ) ) {
                    this.providers.add( provider );
                }
            }
        }
    }

    private void registerCommand( final String providerId,
                                  final Class< ? extends ShellCommand > commandClass,
                                  final WorkspaceStatus wsStatus ) throws Exception {
        final Constructor< ? extends ShellCommand > constructor = commandClass.getConstructor( WorkspaceStatus.class );
        final ShellCommand command = constructor.newInstance( wsStatus );
        command.setWriter(wsStatus.getShell().getOutputWriter());

        final String cmdName = command.getName();
        Map<String, ShellCommand> commands = this.commandMap.get( cmdName );

        if (commands == null) {
            commands = new HashMap<>();
            this.commandMap.put( cmdName, commands );
        }

        commands.put( providerId, command );
    }

    /**
     * @param commandName
     *        the name of the available command being requested (cannot be empty)
     * @return the specified command or a "command not found" command (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    public ShellCommand getCommand( final String commandName ) throws Exception {
        ArgCheck.isNotEmpty( commandName, "commandName" ); //$NON-NLS-1$

        // get a list of available commands
        final Set< ShellCommand > availableCommands = getCommandsForCurrentContext();

        // see if there is a match
        for ( final ShellCommand possible : availableCommands ) {
            if ( commandName.equals( possible.getName() ) ) {
                return possible;
            }
        }

        // see if there is a matching alias
        for ( final ShellCommand possible : availableCommands ) {
            if ( Arrays.asList( possible.getAliases() ).contains( commandName ) ) {
                return possible;
            }
        }

        // command can't be found
        return _commandNotFound;
    }

    private Set< ShellCommand > getCommandsForCurrentContext() throws Exception {
        final Set< ShellCommand > availableCommands = new HashSet< >();

        for ( final String cmdName : this.commandMap.keySet() ) {
            final Map< String, ShellCommand > commands = this.commandMap.get( cmdName );
            ShellCommand builtIn = null;
            ShellCommand override = null;

            for ( final Entry< String, ShellCommand > entry : commands.entrySet() ) {
                if ( BUILT_IN_PROVIDER_ID.equals( entry.getKey() ) ) {
                    builtIn = entry.getValue();

                    if ( override != null ) {
                        // found valid command and built-in is overridable so no need to look further
                        if ( builtIn.isOverridable() ) {
                            break;
                        }

                        // remove found valid command if built-in is not overridable
                        availableCommands.remove( override );
                    }

                    if ( builtIn.isValidForCurrentContext() ) {
                        if ( builtIn.isEnabled() ) {
                            availableCommands.add( builtIn );
                        }

                        // no need to look further if built-in is not overridable
                        if ( !builtIn.isOverridable() ) {
                            break;
                        }
                    }
                } else if ( ( override == null ) && entry.getValue().isValidForCurrentContext() ) {
                    override = entry.getValue();

                    if ( override.isEnabled() ) {
                        availableCommands.add( override );
                    }

                    // remove built-in as it is being overridden
                    if ( builtIn != null ) {
                        assert builtIn.isOverridable();
                        availableCommands.remove( builtIn );
                        break; // no need to look further as the built-in has already been found
                    }
                }
            }
        }

        return availableCommands;
    }

    /**
     * Get valid command names for the current context.
     *
     * @return a list of commands for current context (never <code>null</code>)
     * @throws Exception
     *         if error occurs
     */
    public List< String > getCommandNamesForCurrentContext() throws Exception {
        final List< String > commandList = new ArrayList< String >();

        for ( final ShellCommand possible : getCommandsForCurrentContext() ) {
            commandList.add( possible.getName() );
        }

        Collections.sort( commandList );
        return commandList;
    }

    class CommandNotFoundCommand extends BuiltInShellCommand {

        /**
         * @param wsStatus
         *        the workspace status (cannot be <code>null</code>)
         */
        public CommandNotFoundCommand( final WorkspaceStatus wsStatus ) {
            super( wsStatus, "cmd-not-found" ); //$NON-NLS-1$
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#isOverridable()
         */
        @Override
        public boolean isOverridable() {
            return false;
        }

        /**
         * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
         */
        @Override
        public void printUsage(int indent) {
            print( CompletionConstants.MESSAGE_INDENT, Messages.getString( SHELL.COMMAND_NOT_FOUND ) );
        }

        /**
         * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
         */
        @Override
        public void printHelp(int indent) {
            print( CompletionConstants.MESSAGE_INDENT, Messages.getString( SHELL.COMMAND_NOT_FOUND ) );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#doExecute()
         */
        @Override
        protected CommandResult doExecute() {
            return new CommandResultImpl( Messages.getString( SHELL.COMMAND_NOT_FOUND ) );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
         */
        @Override
        protected int getMaxArgCount() {
            return Integer.MAX_VALUE; // set high as we don't want the max arg check to fail
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
         */
        @Override
        public boolean isValidForCurrentContext() {
            return true;
        }

    }

}
