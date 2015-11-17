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
import java.io.Console;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Map;
import org.komodo.shell.api.ShellCommandFactory;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.i18n.I18n;

/**
 * An implementation of the {@link ShellCommandReader} that reads data from
 * a file.
 *
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use WorkspaceStatus
 *
 * @author eric.wittmann@redhat.com
 */
public class FileShellCommandReader extends AbstractShellCommandReader {

	private final String filePath;
	private BufferedReader fileReader;

	/**
	 * Constructor.
	 *
	 * @param factory the factory
     * @param wsStatus the workspace status
	 * @param filePath the file path
	 */
	public FileShellCommandReader(ShellCommandFactory factory, WorkspaceStatus wsStatus, String filePath) {
		super(factory, wsStatus);
		this.filePath = filePath;
	}

	/**
	 * Instantiates a new file shell command reader.
	 *
	 * @param factory the factory
     * @param wsStatus the workspace status
	 * @param filePath the file path
	 * @param properties the properties
	 */
	public FileShellCommandReader(ShellCommandFactory factory, WorkspaceStatus wsStatus, String filePath,
			Map<String, String> properties) {
		super(factory, wsStatus, properties);
		this.filePath = filePath;
	}

	/**
	 * Open.
	 *
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @see org.komodo.shell.AbstractShellCommandReader#open()
	 */
	@Override
	public void open() throws IOException {
		File file = new File(filePath);
		if (!file.isFile()) {
			throw new FileNotFoundException(filePath);
		}

		InputStream inputStream = new FileInputStream(file);

		this.fileReader = new BufferedReader(new InputStreamReader(inputStream));
	}

	/**
	 * Read line.
	 *
	 * @return the string
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @see org.komodo.common.shell.AbstractShellCommandReader#readLine()
	 */
	@Override
	protected String readLine() throws IOException {
		String line = null;
		do {
			line = fileReader.readLine();
			if (line == null)
				break;
		} while (line.startsWith("#") || line.trim().length() == 0); //$NON-NLS-1$
		return line;
	}

	/**
	 * Close.
	 *
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @see org.komodo.shell.ShellCommandReader#close()
	 */
	@Override
	public void close() throws IOException {
        if ( this.fileReader != null ) {
            fileReader.close();
        }
	}

	/**
	 * Checks if is batch.
	 *
	 * @return true, if is batch
	 * @see org.komodo.shell.ShellCommandReader#isBatch()
	 */
	@Override
	public boolean isBatch() {
		return true;
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
		Console console = System.console();
		if (console != null) {
			return console.readLine(prompt);
		} else {
			throw new RuntimeException(I18n.bind(ShellI18n.fileShellCommandReaderNoConsole));
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
		Console console = System.console();
		if (console != null) {
			return new String(console.readPassword(prompt));
		} else {
			throw new RuntimeException(I18n.bind(ShellI18n.fileShellCommandReaderNoConsole));
		}
	}

}
