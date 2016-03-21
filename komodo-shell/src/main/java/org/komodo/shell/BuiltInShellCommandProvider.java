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

import java.util.HashSet;
import java.util.Set;

import org.komodo.shell.api.ShellCommand;
import org.komodo.shell.api.ShellCommandProvider;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.AddChildCommand;
import org.komodo.shell.commands.AddDescriptorCommand;
import org.komodo.shell.commands.CdCommand;
import org.komodo.shell.commands.CommitCommand;
import org.komodo.shell.commands.DeleteChildCommand;
import org.komodo.shell.commands.ExitCommand;
import org.komodo.shell.commands.HelpCommand;
import org.komodo.shell.commands.HomeCommand;
import org.komodo.shell.commands.LibraryCommand;
import org.komodo.shell.commands.ListCommand;
import org.komodo.shell.commands.PlayCommand;
import org.komodo.shell.commands.RemoveDescriptorCommand;
import org.komodo.shell.commands.RenameCommand;
import org.komodo.shell.commands.ResetGlobalPropertyCommand;
import org.komodo.shell.commands.RollbackCommand;
import org.komodo.shell.commands.SetAutoCommitCommand;
import org.komodo.shell.commands.SetGlobalPropertyCommand;
import org.komodo.shell.commands.SetPrimaryTypeCommand;
import org.komodo.shell.commands.SetPropertyCommand;
import org.komodo.shell.commands.SetRecordCommand;
import org.komodo.shell.commands.ShowChildrenCommand;
import org.komodo.shell.commands.ShowDescriptorsCommand;
import org.komodo.shell.commands.ShowGlobalCommand;
import org.komodo.shell.commands.ShowPropertiesCommand;
import org.komodo.shell.commands.ShowPropertyCommand;
import org.komodo.shell.commands.ShowStatusCommand;
import org.komodo.shell.commands.ShowSummaryCommand;
import org.komodo.shell.commands.UnsetPropertyCommand;
import org.komodo.shell.commands.WorkspaceCommand;
import org.komodo.spi.repository.KomodoObject;
import org.komodo.spi.repository.Repository.UnitOfWork;

/**
 * A {@link ShellCommandProvider} for {@link KomodoObject} built-in commands.
 */
public final class BuiltInShellCommandProvider implements ShellCommandProvider {

    /**
     * Constructs a command provider.
     */
    public BuiltInShellCommandProvider() {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#getStatusMessage(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public String getStatusMessage( final WorkspaceStatus wsStatus ) {
        return null;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#initWorkspaceState(org.komodo.shell.api.WorkspaceStatus)
     */
    @Override
    public void initWorkspaceState( final WorkspaceStatus status ) {
        // nothing to do
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#provideCommands()
     */
    @Override
    public Set< Class< ? extends ShellCommand > > provideCommands() {
        final Set< Class< ? extends ShellCommand > > result = new HashSet< >();

        result.add( AddChildCommand.class );
        result.add( AddDescriptorCommand.class );
        result.add( CdCommand.class );
        result.add( CommitCommand.class );
        result.add( DeleteChildCommand.class );
        result.add( ExitCommand.class );
        result.add( HelpCommand.class );
        result.add( HomeCommand.class );
        result.add( LibraryCommand.class );
        result.add( ListCommand.class );
        result.add( PlayCommand.class );
        result.add( RemoveDescriptorCommand.class );
        result.add( RenameCommand.class );
        result.add( RollbackCommand.class );
        result.add( SetAutoCommitCommand.class );
        result.add( SetGlobalPropertyCommand.class );
        result.add( ResetGlobalPropertyCommand.class );
        result.add( SetPrimaryTypeCommand.class );
        result.add( SetPropertyCommand.class );
        result.add( SetRecordCommand.class );
        result.add( ShowChildrenCommand.class );
        result.add( ShowDescriptorsCommand.class );
        result.add( ShowGlobalCommand.class );
        result.add( ShowPropertiesCommand.class );
        result.add( ShowPropertyCommand.class );
        result.add( ShowStatusCommand.class );
        result.add( ShowSummaryCommand.class );
        result.add( UnsetPropertyCommand.class );
        result.add( WorkspaceCommand.class );

        return result;
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommandProvider#resolve(org.komodo.spi.repository.Repository.UnitOfWork,
     *      org.komodo.spi.repository.KomodoObject)
     */
    @Override
    public < T extends KomodoObject > T resolve( final UnitOfWork transaction,
                                                 final KomodoObject kobject ) {
        return null; // does not resolve any KomodoObject to a subclass type
    }

}
