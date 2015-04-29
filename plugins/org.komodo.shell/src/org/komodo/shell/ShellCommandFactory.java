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
import org.komodo.shell.api.WorkspaceContext;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.CommandNotFoundCommand;
import org.komodo.shell.commands.ExitCommand;
import org.komodo.shell.commands.HelpCommand;
import org.komodo.shell.commands.core.CdCommand;
import org.komodo.shell.commands.core.CreateCommand;
import org.komodo.shell.commands.core.DeleteCommand;
import org.komodo.shell.commands.core.ExportCommand;
import org.komodo.shell.commands.core.ImportCommand;
import org.komodo.shell.commands.core.ListCommand;
import org.komodo.shell.commands.core.NavigateCommand;
import org.komodo.shell.commands.core.PlayCommand;
import org.komodo.shell.commands.core.RenameCommand;
import org.komodo.shell.commands.core.SetCommand;
import org.komodo.shell.commands.core.ShowCommand;
import org.komodo.shell.commands.core.UseTeiidCommand;
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

	private static String HELP_CMD_NAME = "help"; //$NON-NLS-1$ 
	private static String EXIT_CMD_NAME = "exit"; //$NON-NLS-1$ 
	private static String QUIT_CMD_NAME = "quit"; //$NON-NLS-1$ 
	
	private static String CD_CMD_NAME = "cd"; //$NON-NLS-1$ 
	private static String LIST_CMD_NAME = "list"; //$NON-NLS-1$
	private static String RENAME_CMD_NAME = "rename"; //$NON-NLS-1$ 
	private static String IMPORT_CMD_NAME = "import"; //$NON-NLS-1$
	private static String EXPORT_CMD_NAME = "export"; //$NON-NLS-1$
	private static String PLAY_CMD_NAME = "play"; //$NON-NLS-1$

	private WorkspaceStatus wsStatus;
	private Map<String, ShellCommand> commandMap;

	/**
	 * Constructor.
     * @param wsStatus the workspace context
	 */
	public ShellCommandFactory(WorkspaceStatus wsStatus) {
		this.wsStatus = wsStatus;
		registerCommands();
	}

	/**
	 * Registers all known commands.
	 */
	private void registerCommands() {
		commandMap = new HashMap<String, ShellCommand>();

		// commands
		List<String> allList = new ArrayList<String>(1);
		allList.add(WorkspaceContext.ALL_TYPES);
		
		ListCommand listCommand = new ListCommand(LIST_CMD_NAME,this.wsStatus);
		commandMap.put(listCommand.getName().toLowerCase(), listCommand);  

		CdCommand cdCommand = new CdCommand(CD_CMD_NAME,this.wsStatus);
		commandMap.put(cdCommand.getName().toLowerCase(), cdCommand); 

		SetCommand setCommand = new SetCommand(this.wsStatus);
		commandMap.put(setCommand.getName().toLowerCase(), setCommand);

		CreateCommand createCommand = new CreateCommand(this.wsStatus);
		commandMap.put(createCommand.getName().toLowerCase(), createCommand);
		
		DeleteCommand deleteCommand = new DeleteCommand(this.wsStatus);
		commandMap.put(deleteCommand.getName().toLowerCase(), deleteCommand);

		ImportCommand importCommand = new ImportCommand(IMPORT_CMD_NAME,this.wsStatus);
		commandMap.put(importCommand.getName().toLowerCase(), importCommand);

		ExportCommand exportCommand = new ExportCommand(EXPORT_CMD_NAME,this.wsStatus);
		commandMap.put(exportCommand.getName().toLowerCase(), exportCommand);

		UseTeiidCommand connCommand = new UseTeiidCommand(this.wsStatus);
        commandMap.put(connCommand.getName().toLowerCase(), connCommand);

        NavigateCommand traverseCommand = new NavigateCommand(this.wsStatus);
        commandMap.put(traverseCommand.getName().toLowerCase(), traverseCommand);
        
        ShowCommand showCommand = new ShowCommand(this.wsStatus);
        commandMap.put(showCommand.getName().toLowerCase(), showCommand);

        RenameCommand renameCommand = new RenameCommand(RENAME_CMD_NAME, this.wsStatus);
        commandMap.put(renameCommand.getName().toLowerCase(), renameCommand);

        PlayCommand playCommand = new PlayCommand(PLAY_CMD_NAME, this.wsStatus);
        commandMap.put(playCommand.getName().toLowerCase(), playCommand);

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
        				ShellCommand command;
						try {
							command = commandClass.newInstance();
	        				command.initValidWsContextTypes();
	        				command.setWorkspaceStatus(this.wsStatus);
	            			commandMap.put(entry.getKey(), command);
						} catch (Exception e) {
						    KEngine.getInstance().getErrorHandler().error(e);
						}
        			}
                }
            }
        }
    }

	/**
	 * Called to create a shell command.
	 * @param commandName the name of the command
	 * @return the command
	 * @throws Exception the exception
	 */
	public ShellCommand getCommand(String commandName) throws Exception {
		ShellCommand command = null;
		if (commandName.toLowerCase().equals(HELP_CMD_NAME.toLowerCase())) {
            command = new HelpCommand(HELP_CMD_NAME, this.wsStatus, getCommands());
		} else if (commandName.toLowerCase().equals(QUIT_CMD_NAME.toLowerCase())) {
			command = new ExitCommand(EXIT_CMD_NAME,this.wsStatus);
		} else if (commandName.toLowerCase().equals(EXIT_CMD_NAME.toLowerCase())) {
			command = new ExitCommand(EXIT_CMD_NAME,this.wsStatus);
		} else {
			command = commandMap.get(commandName.toLowerCase());
			if (command == null)
				return new CommandNotFoundCommand("NotFound"); //$NON-NLS-1$
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
