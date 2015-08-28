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
import java.io.Writer;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.komodo.shell.Messages.SHELL;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.NoOpCommand;
import org.komodo.shell.util.KomodoObjectUtils;

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
            return new NoOpCommand( this.wsStatus );
		}

		// The first argument is the command name.
		String commandName = arguments.removeCommandName();

		// Create the command.
		ShellCommand command = factory.getCommand(commandName);
		command.setArguments(arguments);
		command.setWriter(getOutputWriter());
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
	    return this.wsStatus.getInputStream();
	}

    /**
     * Gets the output stream that should be used by commands when they need to
     * print a message to the console.
     *
     * @return the command output
     */
    protected Writer getOutputWriter() {
        return this.wsStatus.getOutputWriter();
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

    protected String getPrompt() throws Exception {
        // see if full path should be displayed
        String path = null;

        try {
            if ( getWorkspaceStatus().isShowingFullPathInPrompt() ) {
                path = KomodoObjectUtils.getFullName(this.wsStatus, this.wsStatus.getCurrentContext());
            } else {
                path = KomodoObjectUtils.getName(this.wsStatus, this.wsStatus.getCurrentContext());
            }
        } catch ( final Exception e ) {
            // problem getting context name
            path = Messages.getString( SHELL.PATH_NOT_FOUND,
                                       this.wsStatus.getCurrentContext().getAbsolutePath() );
            return Messages.getString( Messages.SHELL.PROMPT, path );
        }

        assert ( path != null );

        // see if type should be displayed
        if ( getWorkspaceStatus().isShowingTypeInPrompt() ) {
            return Messages.getString( Messages.SHELL.PROMPT_WITH_TYPE, path, getWorkspaceStatus().getTypeDisplay(getWorkspaceStatus().getCurrentContext()) );
        }

        return Messages.getString( Messages.SHELL.PROMPT, path );
    }
}
