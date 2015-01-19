/*
 * Copyright 2014 JBoss Inc
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Map;
import org.komodo.shell.api.WorkspaceStatus;

/**
 * An implementation of the {@link ShellCommandReader} that uses standard input
 * to read commands typed in by the user.
 * 
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use WorkspaceStatus
 * 
 * @author eric.wittmann@redhat.com
 */
public class StdInShellCommandReader extends AbstractShellCommandReader {

	private BufferedReader stdinReader;

	/**
	 * Constructor.
	 *
	 * @param factory the factory
     * @param wsStatus the workspace status
	 */
	public StdInShellCommandReader(ShellCommandFactory factory, WorkspaceStatus wsStatus) {
		super(factory, wsStatus);
	}

	/**
	 * Instantiates a new std in shell command reader.
	 *
	 * @param factory the factory
     * @param wsStatus the workspace status
	 * @param properties the properties
	 */
	public StdInShellCommandReader(ShellCommandFactory factory, WorkspaceStatus wsStatus,
			Map<String, String> properties) {
		super(factory, wsStatus, properties);
	}

	/**
	 * Open.
	 *
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @see org.komodo.shell.AbstractShellCommandReader#open()
	 */
	@Override
	public void open() throws IOException {
		stdinReader = new BufferedReader(new InputStreamReader(getInputStream()));
	}

	/**
	 * @see org.komodo.shell.AbstractShellCommandReader#readLine()
	 */
	@Override
	protected String readLine() throws Exception {
		if (!stdinReader.ready()) {
			String prompt = "["+getWorkspaceStatus().getCurrentContext().getName()+"] > "; //$NON-NLS-1$ //$NON-NLS-2$
			getOutputStream().print(prompt);
		}
		return stdinReader.readLine();
	}

	/**
	 * Close.
	 *
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @see org.komodo.shell.ShellCommandReader#close()
	 */
	@Override
	public void close() throws IOException {
	    // Nothing to do
	}

	/**
	 * Prompt for input.
	 *
	 * @param prompt the prompt
	 * @return the string
	 * @see org.komodo.shell.ShellCommandReader#promptForInput(java.lang.String)
	 */
	@Override
	public String promptForInput(String prompt) {
		try {
			if (!stdinReader.ready()) {
				getOutputStream().print(prompt);
			}
			return stdinReader.readLine();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Prompt for password.
	 *
	 * @param prompt the prompt
	 * @return the string
	 * @see org.komodo.shell.ShellCommandReader#promptForPassword(java.lang.String)
	 */
	@Override
	public String promptForPassword(String prompt) {
		// stdin doesn't support reading passwords
		return promptForInput(prompt);
	}

}
