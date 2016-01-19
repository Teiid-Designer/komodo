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
import java.util.TreeSet;
import org.komodo.core.KEngine;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandFactory;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;
import org.komodo.utils.KLog;
import org.komodo.utils.i18n.I18n;

/**
 * Factory used to create shell commands.
 *
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use WorkspaceStatus.  additional methods added
 * - altered map usage to to change from Shell Context usage
 *
 * @author eric.wittmann@redhat.com
 */
public class ShellCommandFactoryImpl implements ShellCommandFactory {

    private static final KLog LOGGER = KLog.getLogger();
    private static final String BUILT_IN_PROVIDER_ID = BuiltInShellCommandProvider.class.getName();

    private static CommandNotFoundCommand _commandNotFound;

    // key = command name, value = {key = provider ID, value = command}
    private final Map< String, Map< String, ShellCommand > > commandMap;
    private Set<ShellCommandProvider> providers;

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     * @throws Exception
     *         if a built-in command cannot be created or if an error occurs
     */
    public ShellCommandFactoryImpl(final WorkspaceStatus wsStatus ) throws Exception {
        ArgCheck.isNotNull( wsStatus, "wsStatus" ); //$NON-NLS-1$

        this.commandMap = new HashMap<>();
        _commandNotFound = new CommandNotFoundCommand( wsStatus );

        discoverProviders( wsStatus );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandFactory#getCommandProviders()
     */
    @Override
    public Set< ShellCommandProvider > getCommandProviders() {
        return this.providers;
    }

    private void registerContributedCommands( final ShellCommandProvider provider,
                                              final WorkspaceStatus wsStatus ) {
        final Collection< Class< ? extends ShellCommand > > commandClasses = provider.provideCommands();
        LOGGER.debug( "ShellCommandFactory.registerContributedCommands: ShellCommandProvider \"{0}\" is contributing {1} commands", //$NON-NLS-1$
                      provider.getClass().getSimpleName(),
                      ( ( commandClasses == null ) ? 0 : commandClasses.size() ) );

        if ( ( commandClasses != null ) && !commandClasses.isEmpty() ) {
            final String providerId = provider.getClass().getName();

            for ( final Class< ? extends ShellCommand > commandClass : commandClasses ) {
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

    private void discoverProviders( final WorkspaceStatus wsStatus ) {
        final List< ClassLoader > commandClassloaders = new ArrayList< >();
        commandClassloaders.add( Thread.currentThread().getContextClassLoader() );

        // Find providers in the user's commands directory
        final String userHome = System.getProperty( "user.home", "/" ); //$NON-NLS-1$ //$NON-NLS-2$
        final String commandsDirName = System.getProperty( "komodo.shell.commandsDir", userHome + "/.komodo/commands" ); //$NON-NLS-1$ //$NON-NLS-2$
        LOGGER.debug( "ShellCommandFactory: commands directory is \"{0}\"", commandsDirName ); //$NON-NLS-1$
        final File commandsDir = new File( commandsDirName );

        if ( !commandsDir.exists() ) {
            commandsDir.mkdirs();
        }

        if ( commandsDir.isDirectory() ) {
            try {
                final Collection< File > jarFiles = FileUtils.getFilesForPattern( commandsDir.getCanonicalPath(), "", ".jar" ); //$NON-NLS-1$ //$NON-NLS-2$

                if ( !jarFiles.isEmpty() ) {
                    final List< URL > jarURLs = new ArrayList< >( jarFiles.size() );

                    for ( final File jarFile : jarFiles ) {
                        final URL jarUrl = jarFile.toURI().toURL();
                        jarURLs.add( jarUrl );
                        LOGGER.debug( "ShellCommandFactory: adding discovered jar \"{0}\"", jarUrl ); //$NON-NLS-1$
                    }

                    final URL[] urls = jarURLs.toArray( new URL[ jarURLs.size() ] );
                    final ClassLoader extraCommandsCL = new URLClassLoader( urls,
                                                                            Thread.currentThread().getContextClassLoader() );
                    commandClassloaders.add( extraCommandsCL );
                }
            } catch ( final IOException e ) {
                KEngine.getInstance().getErrorHandler().error( e );
            }
        }

        // add built-in provider and discover other providers
        final Set< ShellCommandProvider > tempProviders = new HashSet< >();
        final ShellCommandProvider builtInProvider = new BuiltInShellCommandProvider();
        tempProviders.add( builtInProvider );
        registerContributedCommands( builtInProvider, wsStatus );

        // iterate through the ClassLoaders and use the Java ServiceLoader mechanism to load the providers
        for ( final ClassLoader classLoader : commandClassloaders ) {
            for ( final ShellCommandProvider provider : ServiceLoader.load( ShellCommandProvider.class, classLoader ) ) {
                if ( !Modifier.isAbstract( provider.getClass().getModifiers() ) ) {
                    tempProviders.add( provider );
                    LOGGER.debug( "ShellCommandFactory: adding ShellCommandProvider \"{0}\"", provider.getClass().getName() ); //$NON-NLS-1$
                    registerContributedCommands( provider, wsStatus );
                }
            }
        }

        LOGGER.debug( "ShellCommandFactory: found \"{0}\" ShellCommandProviders", tempProviders.size() ); //$NON-NLS-1$
        this.providers = Collections.unmodifiableSet( tempProviders );
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
        LOGGER.debug( "ShellCommandFactory.registerCommand: ShellCommandProvider \"{0}\", command: {1}", //$NON-NLS-1$
                      providerId,
                      commandClass );
    }

    /**
     * @param commandName
     *        the name of the available command being requested (cannot be empty)
     * @return the specified command or a "command not found" command (never <code>null</code>)
     * @throws Exception
     *         if an error occurs
     */
    @Override
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
        return createCommandNotFound(commandName);
    }
    
    /* (non-Javadoc)
     * @see org.komodo.shell.api.ShellCommandFactory#createCommandNotFound(java.lang.String)
     */
    @Override
    public ShellCommand createCommandNotFound(String commandName) {
        _commandNotFound.command = commandName;
        return _commandNotFound;
    }

    @Override
    public Set< ShellCommand > getCommandsForCurrentContext() {
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
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandFactory#getCommandNamesForCurrentContext()
     */
    @Override
    public Set< String > getCommandNamesForCurrentContext() {
        final Set< String > commandNames = new TreeSet< >();

        for ( final ShellCommand possible : getCommandsForCurrentContext() ) {
            commandNames.add( possible.getName() );
        }

        return commandNames;
    }

    class CommandNotFoundCommand extends BuiltInShellCommand {

        public String command;

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
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#doExecute()
         */
        @Override
        protected CommandResult doExecute() {
            return new CommandResultImpl( false, I18n.bind( ShellI18n.commandNotFound, this.command ), null );
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

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
         */
        @Override
        protected void printHelpDescription( final int indent ) {
            // nothing to do
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
         */
        @Override
        protected void printHelpExamples( final int indent ) {
            // nothing to do
        }

        /**
         * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
         */
        @Override
        public void printHelp(int indent) {
            print( CompletionConstants.MESSAGE_INDENT, I18n.bind( ShellI18n.commandNotFound, this.command ) );
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
         */
        @Override
        protected void printHelpUsage( final int indent ) {
            // nothing to do
        }

        /**
         * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
         */
        @Override
        public void printUsage(int indent) {
            print( CompletionConstants.MESSAGE_INDENT, I18n.bind( ShellI18n.commandNotFound, this.command ) );
        }

    }

}
