/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */
package org.komodo.shell;

import java.io.IOException;
import java.io.OutputStreamWriter;
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
    }

	/**
     * Open.
     *
     * @throws IOException Signals that an I/O exception has occurred.
     * @see org.komodo.shell.ShellCommandReader#open()
     */
	@Override
	public abstract void open() throws IOException;

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
			return new NoOpCommand();
		}

		// The first argument is the command name.
		String commandName = arguments.removeCommandName();

		// Create the command.
		ShellCommand command = factory.getCommand(commandName);
		command.setWorkspaceStatus(this.wsStatus);
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
		return new OutputStreamWriter(System.out);
	}

	/**
     * Reads a single line from the input source (e.g. user input) and returns
     * it.
     *
     * @return the string
     * @throws IOException
     *             Signals that an I/O exception has occurred.
     */
	protected abstract String readLine() throws IOException;

	/**
     * Close.
     *
     * @throws IOException Signals that an I/O exception has occurred.
     * @see org.komodo.shell.ShellCommandReader#close()
     */
	@Override
	public void close() throws IOException {
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
