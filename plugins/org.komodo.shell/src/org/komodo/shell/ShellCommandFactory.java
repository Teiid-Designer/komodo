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
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.TreeMap;
import javax.xml.namespace.QName;
import org.komodo.core.KEngine;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.CommandNotFoundCommand;
import org.komodo.shell.commands.ExitCommand;
import org.komodo.shell.commands.HelpCommand;
import org.komodo.shell.commands.core.AddConstraintColumnCommand;
import org.komodo.shell.commands.core.CdCommand;
import org.komodo.shell.commands.core.CreateCommand;
import org.komodo.shell.commands.core.DeleteCommand;
import org.komodo.shell.commands.core.ExportCommand;
import org.komodo.shell.commands.core.FindCommand;
import org.komodo.shell.commands.core.HomeCommand;
import org.komodo.shell.commands.core.ImportCommand;
import org.komodo.shell.commands.core.ListCommand;
import org.komodo.shell.commands.core.PlayCommand;
import org.komodo.shell.commands.core.RemoveConstraintColumnCommand;
import org.komodo.shell.commands.core.RenameCommand;
import org.komodo.shell.commands.core.ServerCommand;
import org.komodo.shell.commands.core.SetCommand;
import org.komodo.shell.commands.core.ShowCommand;
import org.komodo.shell.commands.core.UnsetPropertyCommand;
import org.komodo.utils.ArgCheck;
import org.komodo.utils.FileUtils;
import org.komodo.utils.StringUtils;

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

	private final WorkspaceStatus wsStatus;
    private final Map<String, ShellCommand> commandMap;
    private final Map<String, ShellCommand> aliasMap; // separate from commandMap so HelpCommand only shows command names

    /**
     * @param wsStatus
     *        the workspace context (cannot be <code>null</code>)
     * @throws Exception
     *         if a built-in command cannot be created or if an error occurs
     */
    public ShellCommandFactory( final WorkspaceStatus wsStatus ) throws Exception {
        ArgCheck.isNotNull( wsStatus, "wsStatus" ); //$NON-NLS-1$
        this.wsStatus = wsStatus;
        this.commandMap = new HashMap< String, ShellCommand >();
        this.aliasMap = new HashMap< String, ShellCommand >();
        registerCommands();
    }

	/**
	 * Registers all known commands.
	 */
    private void registerCommands() throws Exception {
        // register built-in commands
        registerCommand( AddConstraintColumnCommand.class );
        registerCommand( CdCommand.class );
        registerCommand( CreateCommand.class );
        registerCommand( DeleteCommand.class );
        registerCommand( ExitCommand.class );
        registerCommand( ExportCommand.class );
        registerCommand( FindCommand.class );
        registerCommand( HelpCommand.class );
        registerCommand( HomeCommand.class );
        registerCommand( ImportCommand.class );
        registerCommand( ListCommand.class );
        // registerCommand( NavigateCommand.class );
        registerCommand( PlayCommand.class );
        registerCommand( RemoveConstraintColumnCommand.class );
        registerCommand( RenameCommand.class );
        registerCommand( SetCommand.class );
        registerCommand( ShowCommand.class );
        registerCommand( UnsetPropertyCommand.class );
        registerCommand( ServerCommand.class );

        // register any commands contributed by command providers
        discoverContributedCommands();
    }

    /**
     * Discover any contributed commands, both on the classpath and registered
     * in the .komodo/commands.ini file in the user's home directory.
     */
    private void discoverContributedCommands() {
        List<ClassLoader> commandClassloaders = new ArrayList<ClassLoader>();
        commandClassloaders.add(Thread.currentThread().getContextClassLoader());

        // Register commands listed in the user's commands.ini config file
        String userHome = System.getProperty("user.home", "/"); //$NON-NLS-1$ //$NON-NLS-2$
        String commandsDirName = System.getProperty("komodo.shell.commandsDir", //$NON-NLS-1$
                userHome + "/.komodo/commands"); //$NON-NLS-1$
        File commandsDir = new File(commandsDirName);
        if (!commandsDir.exists()) {
            commandsDir.mkdirs();
        }
        if (commandsDir.isDirectory()) {
            try {
            	Collection<File> jarFiles =  FileUtils.getFilesForPattern(commandsDir.getCanonicalPath(), "", ".jar"); //$NON-NLS-1$ //$NON-NLS-2$
                List<URL> jarURLs = new ArrayList<URL>(jarFiles.size());
                for (File jarFile : jarFiles) {
                    jarURLs.add(jarFile.toURI().toURL());
                }
                URL[] urls = jarURLs.toArray(new URL[jarURLs.size()]);
                ClassLoader extraCommandsCL = new URLClassLoader(urls, Thread.currentThread().getContextClassLoader());
                commandClassloaders.add(extraCommandsCL);
            } catch (IOException e) {
                KEngine.getInstance().getErrorHandler().error(e);
            }
        }

        // Now that we have identified all ClassLoaders to check for commands, iterate
        // through them all and use the Java ServiceLoader mechanism to actually
        // load the commands.
        for (ClassLoader classLoader : commandClassloaders) {
            for (ShellCommandProvider provider : ServiceLoader.load(ShellCommandProvider.class, classLoader)) {
                Map<String, Class<? extends ShellCommand>> commands = provider.provideCommands();

                for (Map.Entry<String, Class<? extends ShellCommand>> entry : commands.entrySet()) {
                	Class<? extends ShellCommand> commandClass = entry.getValue();

                	if (commandClass != null) {
        				try {
                            registerCommand( commandClass );
						} catch (Exception e) {
						    KEngine.getInstance().getErrorHandler().error(e);
						}
        			}
                }
            }
        }
    }

    private void registerCommand( final Class< ? extends ShellCommand > commandClass ) throws Exception {
        final Constructor< ? extends ShellCommand > constructor = commandClass.getConstructor( WorkspaceStatus.class );
        final ShellCommand command = constructor.newInstance( this.wsStatus );
        command.initValidWsContextTypes();
        this.commandMap.put( command.getName().toLowerCase(), command );

        // add aliases
        final String[] aliases = command.getAliases();

        if ( ( aliases != null ) && ( aliases.length != 0 ) ) {
            for ( final String alias : aliases ) {
                if ( !StringUtils.isBlank( alias ) ) {
                    this.aliasMap.put( alias.toLowerCase(), command );
                }
            }
        }
    }

    /**
     * @param commandName
     *        the name of the command being requested (cannot be empty)
     * @return the command or a {@link CommandNotFoundCommand} (never <code>null</code>)
     */
    public ShellCommand getCommand( final String commandName ) {
        ArgCheck.isNotEmpty( commandName, "commandName" ); //$NON-NLS-1$
        ShellCommand command = this.commandMap.get( commandName.toLowerCase() );

        if ( command == null ) {
            // see if alias
            command = this.aliasMap.get( commandName.toLowerCase() );

            // if still not found the command can't be found
            if ( command == null ) {
                return new CommandNotFoundCommand( commandName, this.wsStatus );
            }
        }

        if ( command instanceof HelpCommand ) {
            ( ( HelpCommand )command ).setCommands( getCommands() );
        }

        return command;
    }

	/**
	 * Get valid command names for the current context
	 * @return List<String> list of commands for current context
	 * @throws Exception if error occurs
	 */
	public List<String> getCommandsForCurrentContext( ) throws Exception {
		List<String> commandList = new ArrayList<String>();
		for(String mapKey : this.commandMap.keySet()) {
			ShellCommand command = this.commandMap.get(mapKey);
			if(command.isValidForWsContext(this.wsStatus.getCurrentContext().getType())) {
				commandList.add(mapKey);
			}
		}
		Collections.sort(commandList);
		return commandList;
	}

	/**
	 * Gets the available commands, ordered by command {@link QName}.
	 */
	private Map<String, ShellCommand> getCommands() {
		TreeMap<String, ShellCommand> treeMap = new TreeMap<String, ShellCommand>(new Comparator<String>() {
			@Override
			public int compare(String name1, String name2) {
				return name1.compareTo(name2);
			}
		});
		treeMap.putAll(this.commandMap);
		return treeMap;
	}

    /**
     * Returns fileArray {@code ArrayList} of {@code File} objects that match a pattern in the specified directory.
     * @param folderToScan The path to look for the matching files
     * @param startWith The beginning portion of the file name
     * @param endsWith The ending portion of the file name (i.e. ".jar")
     * @return fileArray An ArrayList of
     */
    public final static ArrayList<File> getFilesForPattern(File folderToScan, String startWith, String endsWith) {
	    String target_file ;  // fileThatYouWantToFilter
		File[] listOfFiles = folderToScan.listFiles();
		ArrayList<File> list = new ArrayList<File>();

		for (File file:listOfFiles) {
	        if (file.isFile()) {
	            target_file = file.getName();
	            if (target_file.startsWith(startWith)
		                 && target_file.endsWith(endsWith)) {
	            	list.add(file);
		        }
		    }
		 }

		return list;
    }

}
