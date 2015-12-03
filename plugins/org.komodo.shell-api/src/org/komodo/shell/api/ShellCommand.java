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

import org.komodo.utils.i18n.I18n;

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
     * Commands without a category will be placed in a general category.
     *
     * @return the command category (can be empty)
     */
    default String getCategory() {
        return I18n.bind( ShellApiI18n.generalCommandCategory );
    }

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
     * @return <code>true</code> if the command is enabled
     */
    default boolean isEnabled() {
        return true;
    }

    /**
     * @return <code>true</code> if the command can be overridden
     */
    default boolean isOverridable() {
        return true;
    }

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
	 * The return value of this method is tab completion modifier through which an user is
	 * able to e.g. turn off tab completion or append separator after the auto completed text
	 *
	 * @param lastArgument the last arg
	 * @param candidates the candidates
	 * @return tab completion modifier
	 * @throws Exception if errors occur
	 */
    default TabCompletionModifier tabCompletion( final String lastArgument,
                               final List< CharSequence > candidates ) throws Exception {
        return TabCompletionModifier.NO_AUTOCOMPLETION;
    }

}
