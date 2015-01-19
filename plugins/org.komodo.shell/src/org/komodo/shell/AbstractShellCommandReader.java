/*************************************************************************************
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership. Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 ************************************************************************************/
package org.komodo.shell;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.PrintStream;
import java.io.Writer;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.NoOpCommand;

/**
 * Abstract class for all shell command readers.
 * 
 * This class adapted from https://github.com/Governance/s-ramp/blob/master/s-ramp-shell
 * - altered to use WorkspaceStatus
 * 
 * @author eric.wittmann@redhat.com
 */
public abstract class AbstractShellCommandReader implements ShellCommandReader {

	private final WorkspaceStatus wsStatus;
	private final ShellCommandFactory factory;
	private final InputStream inStream;
	private final PrintStream outStream;
    private Map<String, String> properties;

    /**
     * Constructor.
     *
     * @param factory the factory
     * @param wsStatus the workspace status
     */
    public AbstractShellCommandReader(ShellCommandFactory factory, WorkspaceStatus wsStatus) {
    	this.factory = factory;
    	this.wsStatus = wsStatus;
        this.inStream = wsStatus.getInputStream();
        this.outStream = wsStatus.getOutputStream();
    }

    /**
     * Instantiates a new abstract shell command reader.
     *
     * @param factory the factory
     * @param wsStatus the workspace status
     * @param properties the properties
     */
    public AbstractShellCommandReader(ShellCommandFactory factory, WorkspaceStatus wsStatus,
            Map<String, String> properties) {
        this.factory = factory;
        this.wsStatus = wsStatus;
        this.properties = properties;
        this.inStream = wsStatus.getInputStream();
        this.outStream = wsStatus.getOutputStream();
    }

	/**
     * Open.
     *
	 * @throws Exception error has occurred
     */
	@Override
	public abstract void open() throws Exception;

	/**
     * Read.
     *
     * @return the shell command
     * @throws Exception the exception
     * @see org.komodo.shell.ShellCommandReader#read()
     */
	@Override
	public final ShellCommand read() throws Exception {
		String line = readLine();
		if (line == null) {
			return null;
		}
        line = filterLine(line, properties);
		Arguments arguments = new Arguments(line);
		if (arguments.isEmpty()) {
			return new NoOpCommand("NoOp"); //$NON-NLS-1$
		}

		// The first argument is the command name.
		String commandName = arguments.removeCommandName();

		// Create the command.
		ShellCommand command = factory.getCommand(commandName);
		command.setArguments(arguments);
		command.setOutput(getCommandOutput());
		return command;
	}

    /**
     * Filter the line using the properties attribute.
     *
     * @param line the line
     * @return the string
     */
    protected static String filterLine(String line, Map<String, String> properties) {
        String filtered = line;
        Pattern pattern = Pattern.compile("\\$\\{(.*?)\\}"); //$NON-NLS-1$
        Matcher matcher = pattern.matcher(line);
        while (matcher.find()) {
            String match = matcher.group();
            String key = match.substring(2, match.length() - 1);
            if (properties.containsKey(key)) {
                filtered = filtered.replace(match, properties.get(key));
            } else if (System.getProperties().containsKey(key)) {
                filtered = filtered.replace(match, (String) System.getProperties().get(key));
            }
        }

        return filtered;
    }

	/**
     * Gets the output stream that should be used by commands when they need to
     * print a message to the console.
     *
     * @return the command output
     */
	protected Writer getCommandOutput() {
		return new OutputStreamWriter(outStream);
	}

	/**
     * Reads a single line from the input source (e.g. user input) and returns
     * it.
     *
     * @return the string
     * @throws IOException
     *             Signals that an I/O exception has occurred.
     */
	protected abstract String readLine() throws Exception;

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
     * Gets the factory.
     *
     * @return the factory
     */
	public ShellCommandFactory getFactory() {
		return factory;
	}

	/**
     * Gets the workspace status.
     *
     * @return the wsStatus
     */
	public WorkspaceStatus getWorkspaceStatus() {
		return wsStatus;
	}

	/**
	 * @return input stream for receiving commands inputted into the console
	 */
	public InputStream getInputStream() {
	    return inStream;
	}

	/**
	 * @return output stream for printing messages to the console
	 */
	public PrintStream getOutputStream() {
	    return outStream;
	}

	/**
     * Checks if is batch.
     *
     * @return true, if is batch
     * @see org.komodo.shell.ShellCommandReader#isBatch()
     */
	@Override
	public boolean isBatch() {
	    return false;
	}

}
