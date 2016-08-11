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
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandFactory;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.utils.TextFormat;
import org.komodo.ui.DefaultLabelProvider;

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
    public AbstractShellCommandReader( final ShellCommandFactory factory,
                                       final WorkspaceStatus wsStatus) {
        this( factory, wsStatus, null );
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
		ShellCommand command = this.wsStatus.getCommand(commandName);
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

    protected List< Entry< TextFormat, String > > getPrompt() throws Exception {
        final KomodoObject kobject = this.wsStatus.getCurrentContext();
        final List< Entry< TextFormat, String > > result = new ArrayList<>(2);

        { // add opening bracket
            final TextFormat format = new TextFormat();
            result.add( new AbstractMap.SimpleEntry< TextFormat, String >( format, "[" ) ); //$NON-NLS-1$
        }

        { // add object path
            final TextFormat format = new TextFormat();
            final String path = KomodoObjectUtils.getDisplayName( this.wsStatus, kobject, format );
            result.add( new AbstractMap.SimpleEntry< TextFormat, String >( format, path ) );
        }

        { // add object type if necessary
            if ( this.wsStatus.isShowingTypeInPrompt() &&
                DefaultLabelProvider.shouldShowType( wsStatus.getTransaction(), kobject ) ) {
                { // add opening paren
                    final TextFormat format = new TextFormat();
                    result.add( new AbstractMap.SimpleEntry< TextFormat, String >( format, " (" ) ); //$NON-NLS-1$
                }

                { // add type
                    final TextFormat format = new TextFormat();
                    final String type = this.wsStatus.getTypeDisplay( kobject, format );
                    result.add( new AbstractMap.SimpleEntry< TextFormat, String >( format, type ) );
                }

                { // add closing paren
                    final TextFormat format = new TextFormat();
                    result.add( new AbstractMap.SimpleEntry< TextFormat, String >( format, ")" ) ); //$NON-NLS-1$
                }
            }
        }

        { // add closing bracket
            final TextFormat format = new TextFormat();
            result.add( new AbstractMap.SimpleEntry< TextFormat, String >( format, "]" ) ); //$NON-NLS-1$
        }

        return result;
    }

    class NoOpCommand extends BuiltInShellCommand {

        /**
         * @param wsStatus
         *        the workspace status (cannot be <code>null</code>)
         */
        public NoOpCommand( final WorkspaceStatus wsStatus ) {
            super( wsStatus, "no-op" ); //$NON-NLS-1$
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#doExecute()
         */
        @Override
        protected CommandResult doExecute() {
            return CommandResult.SUCCESS;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
         */
        @Override
        protected int getMaxArgCount() {
            return Integer.MAX_VALUE; // set high as we don't want this to fail max num arg check
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#isOverridable()
         */
        @Override
        public boolean isOverridable() {
            return false;
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
         */
        @Override
        public boolean isValidForCurrentContext() {
            return true;
        }

        /**
         * @see org.komodo.shell.api.ShellCommand#printHelp(int indent)
         */
        @Override
        public void printHelp( final int indent ) {
            // Nothing to do
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
         */
        @Override
        protected void printHelpDescription( final int indent ) {
            // nothing to do
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
         */
        @Override
        protected void printHelpExamples( final int indent ) {
            // nothing to do
        }

        /**
         * {@inheritDoc}
         *
         * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
         */
        @Override
        protected void printHelpUsage( final int indent ) {
            // nothing to do
        }

        /**
         * @see org.komodo.shell.api.ShellCommand#printUsage(int indent)
         */
        @Override
        public void printUsage( final int indent ) {
            // Nothing to do
        }

    }

}
