/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.shell;

import java.io.Writer;
import java.util.List;
import org.komodo.shell.api.Arguments;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.utils.i18n.I18n;

/**
 * Represents a disabled {@link ShellCommand shell command}.
 */
public abstract class DisabledShellCommand extends BuiltInShellCommand {

    /**
     * @param workspaceStatus
     *        the workspace status (cannot be <code>null</code>)
     * @param names
     *        the command name and then any aliases (cannot be <code>null</code>, empty, or have an empty first element)
     */
    public DisabledShellCommand( final WorkspaceStatus workspaceStatus,
                                 final String... names ) {
        super( workspaceStatus, names );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected final CommandResult doExecute() {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "doExecute", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#getArguments()
     */
    @Override
    public final Arguments getArguments() {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "getArguments", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected final int getMaxArgCount() {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "getMaxArgCount", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#getWriter()
     */
    @Override
    public final Writer getWriter() {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "getWriter", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @return <code>false</code>
     * @see org.komodo.shell.api.ShellCommand#isEnabled()
     */
    @Override
    public final boolean isEnabled() {
        return false;
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#optionalArgument(int)
     */
    @Override
    protected final String optionalArgument( final int argIndex ) {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "optionalArgument", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#optionalArgument(int, java.lang.String)
     */
    @Override
    protected final String optionalArgument( final int argIndex,
                                             final String defaultValue ) {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "optionalArgument", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#print()
     */
    @Override
    protected final void print() {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "print", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#print(int, java.lang.String, java.lang.Object[])
     */
    @Override
    protected final void print( final int indent,
                                final String formattedMessage,
                                final Object... params ) {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "print", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#printHelp(int)
     */
    @Override
    public final void printHelp( final int indent ) {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "printHelp", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#printHelpDescription(int)
     */
    @Override
    protected final void printHelpDescription( final int indent ) {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "printHelpDescription", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#printHelpExamples(int)
     */
    @Override
    protected final void printHelpExamples( final int indent ) {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "printHelpExamples", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#printHelpUsage(int)
     */
    @Override
    protected final void printHelpUsage( final int indent ) {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "printHelpUsage", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#printUsage(int)
     */
    @Override
    public final void printUsage( final int indent ) {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "printUsage", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#requiredArgument(int, java.lang.String)
     */
    @Override
    protected final String requiredArgument( final int argIndex,
                                             final String message ) {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "requiredArgument", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.BuiltInShellCommand#setArguments(org.komodo.shell.api.Arguments)
     */
    @Override
    public final void setArguments( final Arguments newArguments ) {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "setArguments", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException
     *         if called
     * @see org.komodo.shell.api.ShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public final TabCompletionModifier tabCompletion( final String lastArgument,
                                    final List< CharSequence > candidates ) throws Exception {
        throw new UnsupportedOperationException( I18n.bind( ShellI18n.disabledCommandMethodNotSupported,
                                                            "tabCompletion", //$NON-NLS-1$
                                                            getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#toString()
     */
    @Override
    public final String toString() {
        return I18n.bind( ShellI18n.commandIsDisabled, getName() );
    }

}
