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
package org.komodo.shell.api;

import java.io.Writer;
import java.util.List;

/**
 * Interface implemented by all shell commands.
 * 
 * This class adapted from classes at https://github.com/Governance/s-ramp/blob/master/s-ramp-shell-api
 * - altered to use WorkspaceStatus
 * - added methods
 * 
 * @author eric.wittmann@redhat.com
 */
public interface ShellCommand {

	/**
	 * Get the name of the command.
	 * @return the command name
	 */
	public String getName();
	
	/**
	 * Set the name of the command.
	 * @param name the command name
	 */
	public void setName(String name);
	
	/**
	 * Sets the arguments that can be used by this command.
	 * @param arguments the args
	 */
	public void setArguments(Arguments arguments);

	/**
	 * Configure the command's output.
	 * @param output the writer
	 */
	public void setOutput(Writer output);

	/**
	 * Set the workspace status
	 * @param wsStatus the workspace status
	 */
	public void setWorkspaceStatus(WorkspaceStatus wsStatus);

	/**
	 * Init Valid workspace context types for this command
	 */
	public void initValidWsContextTypes();

	/**
	 * Is the command valid for the supplied Ws Context
	 * @param contextType the context type
	 * @return 'true' if valid, 'false' if not.
	 */
	public boolean isValidForWsContext(String contextType);
	
	/**
	 * Called to execute the command.
	 * @return execution status
	 * @throws Exception the exception
	 */
	public boolean execute() throws Exception;

	/**
	 * Prints the usage help for this command.
	 * @param indent number of spaces to indent
	 */
	public void printUsage(int indent);

	/**
	 * Prints the help text for this command.
	 * @param indent number of spaces to indent
	 */
	public void printHelp(int indent);
	
	/**
	 * Record the command to the current record output
	 * @param args the command args
	 */
	public void recordCommand(Arguments args);
	
	/**
	 * Record the comment to the current record output
	 * @param comment the comment
	 */
	public void recordComment(String comment);

	/**
	 * Prints the given message to the output stream.
	 * @param indent the number of chars to indent the message
	 * @param formattedMessage the message
	 * @param params the params
	 */
	public void print(int indent,String formattedMessage, Object ... params);

	/**
	 * Handle tab completion for the command.  This is optional, but provides a mechanism by
	 * which individual commands can enable command-specific tab completion functionality.
	 *
	 * The return value of this method represents the cursor position within lastArgument
	 * that represents the origin point for all of the candidates.  Return 0 to indicate that
	 * the candidates are all full-replacements of lastArgument.  Return -1 to indicate that
	 * no candidates were supplied.  Return a positive, non-negative value if the returned
	 * candidates are partial completions.
	 *
	 * For example, if the user has typed "aar", then a command could return any of:
	 *
	 * candidates=[]  rval=-1
	 * candidates=["aardvark", "aardwolf"]  rval=0
	 * candidates=["dvark", "dwolf"]  rval=3
	 *
	 * In the latter two examples, the tab-completion will be the same, but the user will
	 * be shown the different candidate values (when more than 1 candidate is returned).
	 *
	 * @param lastArgument the last arg
	 * @param candidates the candidates
	 * @return the cursor position
	 * @throws Exception if errors occur
	 */
	public int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception;

}
