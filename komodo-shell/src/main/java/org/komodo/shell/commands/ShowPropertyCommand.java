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
package org.komodo.shell.commands;

import java.util.List;
import org.komodo.shell.BuiltInShellCommand;
import org.komodo.shell.CommandResultImpl;
import org.komodo.shell.ShellI18n;
import org.komodo.shell.api.CommandResult;
import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.TabCompletionModifier;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.util.KomodoObjectUtils;
import org.komodo.shell.util.PrintUtils;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.utils.i18n.I18n;

/**
 * A {@link ShellCommand command} that displays a specific property for a {@link KomodoObject}.
 * <p>
 * Usage:
 * <p>
 * <code>&nbsp;&nbsp;
 * show-property &lt;prop-name&gt;
 * </code>
 */
public class ShowPropertyCommand extends BuiltInShellCommand {

    /**
     * The command name.
     */
    public static final String NAME = "show-property"; //$NON-NLS-1$

    /**
     * @param wsStatus
     *        the workspace status (cannot be <code>null</code>)
     */
    public ShowPropertyCommand( final WorkspaceStatus wsStatus ) {
        super( wsStatus, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        // Not valid in root, workspace, library or environment
        if( KomodoObjectUtils.isRoot(getContext()) || KomodoObjectUtils.isRootChild(getTransaction(), getContext()) ) {
            return false;
        }
        return true;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#doExecute()
     */
    @Override
    protected CommandResult doExecute() {
        try {
            // required arg is the property name.  Verify that it is valid for the current object
            String propName = requiredArgument( 0,
                                                I18n.bind( ShellI18n.invalidArgMsgPropertyName ) );

            KomodoObject context = getContext();
            if ( !KomodoObjectUtils.isValidProperty( getWorkspaceStatus(), propName, context ) ) {
                return new CommandResultImpl( false, I18n.bind( ShellI18n.invalidPropName, propName ), null );
            }

            PrintUtils.printProperty( getWorkspaceStatus(), getWriter(), context, propName );
            return CommandResult.SUCCESS;
        } catch ( Exception e ) {
            return new CommandResultImpl( e );
        }
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.BuiltInShellCommand#getMaxArgCount()
     */
    @Override
    protected int getMaxArgCount() {
        return 1;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpDescription(int)
     */
    @Override
    protected void printHelpDescription( final int indent ) {
        print( indent, I18n.bind( ShellI18n.showPropertyHelp, getName() ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpExamples(int)
     */
    @Override
    protected void printHelpExamples( final int indent ) {
        print( indent, I18n.bind( ShellI18n.showPropertyExamples ) );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.relational.commands.datarole.DataRoleShellCommand#printHelpUsage(int)
     */
    @Override
    protected void printHelpUsage( final int indent ) {
        print( indent, I18n.bind( ShellI18n.showPropertyUsage ) );
    }

    /**
     * @see org.komodo.shell.BuiltInShellCommand#tabCompletion(java.lang.String, java.util.List)
     */
    @Override
    public TabCompletionModifier tabCompletion(String lastArgument, List<CharSequence> candidates) throws Exception {
        if(getArguments().isEmpty()) {
            updateTabCompleteCandidatesForProperty(candidates, getContext(), lastArgument);
        }
        return TabCompletionModifier.AUTO;
    }

}
