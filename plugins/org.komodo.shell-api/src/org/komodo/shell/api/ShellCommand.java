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
     * @return the command name (never empty)
     */
    String getName();

	/**
	 * @return the command name aliases (never <code>null</code> but can be empty)
	 */
	String[] getAliases();

    /**
     * @return the command arguments (can be <code>null</code>)
     */
    Arguments getArguments();

    /**
     * Sets the arguments that can be used by this command.
     * @param arguments the args
     */
    void setArguments(Arguments arguments);

	/**
	 * Configure the command's output.
	 * @param output the writer
	 */
	void setWriter(Writer output);

	/**
	 * @return the stream writer (can be <code>null</code> if not set)
	 */
	Writer getWriter();

	/**
	 * Get the WorkspaceStatus
	 * @return the workspace status
	 */
	WorkspaceStatus getWorkspaceStatus();

    /**
     * @return <code>true</code> if command is valid for the current context
     */
    boolean isValidForCurrentContext();

    /**
     * @return <code>true</code> if the command can be overridden
     */
    boolean isOverridable();

    /**
     * @return <code>true</code> if the command is enabled
     */
    boolean isEnabled();

    /**
     * Called to execute the command.
     *
     * @return the result (never <code>null</code>)
     */
    CommandResult execute();

	/**
	 * Prints the usage help for this command.
	 * @param indent number of spaces to indent
	 */
	void printUsage(int indent);

	/**
	 * Prints the help text for this command.
	 * @param indent number of spaces to indent
	 */
	void printHelp(int indent);

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
	int tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception;

}
