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
package org.komodo.relational.commands.workspace;

import org.komodo.shell.DisabledShellCommand;
import org.komodo.shell.api.WorkspaceStatus;
import org.komodo.shell.commands.UnsetPropertyCommand;
import org.komodo.spi.repository.KomodoType;

/**
 * A disabled shell command that overrides the default {@link UnsetPropertyCommand unset property} command. The workspace does not
 * have any editable properties.
 */
public final class WorkspaceUnsetPropertyCommand extends DisabledShellCommand {

    static final String NAME = UnsetPropertyCommand.NAME; // override

    /**
     * @param status
     *        the shell's workspace status (cannot be <code>null</code>)
     */
    public WorkspaceUnsetPropertyCommand( final WorkspaceStatus status ) {
        super( status, NAME );
    }

    /**
     * {@inheritDoc}
     *
     * @see org.komodo.shell.api.ShellCommand#isValidForCurrentContext()
     */
    @Override
    public boolean isValidForCurrentContext() {
        try {
            final KomodoType contextType = getContext().getTypeIdentifier( getTransaction() );
            return ( contextType == KomodoType.WORKSPACE );
        } catch ( final Exception e ) {
            return false;
        }
    }

}
