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

import java.io.IOException;
import org.komodo.shell.api.ShellCommand;

/**
 * Interface used to read commands from the user (or some other source, like
 * an input file).
 * 
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - no changes currently
 * 
 * @author eric.wittmann@redhat.com
 */
public interface ShellCommandReader {

	/**
	 * Called to open the shell command reader.
	 * @throws Exception the exception
	 */
	public void open() throws Exception;

	/**
	 * Reads the next command from the input source.
	 * @return the command
	 * @throws Exception the exception
	 */
	public ShellCommand read() throws Exception;

	/**
	 * Called by the shell when exiting.
	 * @throws IOException the exception
	 */
	public void close() throws IOException;

	/**
	 * @return true if this is a batch command reader
	 */
	public boolean isBatch();

    /**
     * Prompts the user for some input.  Returns the text entered by the user.
     * @param prompt the prompt
     * @return text entered
     */
    public String promptForInput(String prompt);

    /**
     * Prompts the user for a password.  Returns the text entered by the user.
     * @param prompt the prompt
     * @return text entered
     */
    public String promptForPassword(String prompt);

}
